;;; emms-filters.el --- Filter for Emms          -*- lexical-binding: t; -*-
;; Copyright (C) 2023
;; Author:  <ericalinagebhart@gmail.com> (Erica Gebhart)
;; Keywords:

;;; Commentary:
;; This just sits on top of emms-browser.el, It takes over management of filters
;; and sets the filter on the browser when the filter changes.
;;
;; Emf keeps it's own filter list, emf-filters, and its only interaction
;; with the browser
;; is through refilter which sets the filter function and re-renders.
;;
;; So when we set a new filter on emms-browser, we turn it into a meta filter
;; and push it on a stack. We make a function from it and set it
;; to the current filter. If the filter is added to, we push the changes To
;; the filter stack and re-render. Quiting a filter pops it from the Stack
;; re-renders with the last filter.
;;
;;
;; Packaging:  Once a filter is properly constructed it will be a cons
;; (name . filter-function) the functions are created with one of the
;; filter factories. A meta-filter is also in this same shape, but only
;; on the emf-stack. (name . filter-lists).  The names are usually constructed
;; from the filter contents.
;;
;; Filters: simple functions which take a track
;; and return true if they do NOT match.
;; These are the exact
;; same filters and mechanism coded in emms-browser.el. emf-filters is
;; synonymous with emms-browser-filters.
;;
;; Filters can be made with filter factory functions. There are only
;; a few of these.
;;
;; Multi-filters: A multi-filter is a list of lists of filter names.
;;   Multi-filters work just like filters. Its just a little bit more
;;   complicated factory.
;;
;; Meta-filter: An interactive stack of multi-function definitions.
;;  The meta filter can create multi-filters interactively.
;;
;;; Code:
(require 'emms-browser)

(defvar  emf-replace-browser-filters t
  "Take over select, previous and next filter responsibilities.")

(defvar  emf-stack nil
  "A history of multi-filters. Our working stack.")

(defvar  emf-filter-ring nil
  "A ring of filter names for quick access with next and previous.")

(defconst emf-no-filter nil ;; '("no filter" . 'ignore)
  "A filter that turns filtering off, a better initial value than nil.")

(defvar emf-current-ring-filter  emf-no-filter
  "The current ring filter, a filter cons, (name . func).")

(defvar emf-filter-factories '()
  "An alist of filter factory functions and their argument lists.")

(defvar emf-filters '()
  "A list of available filters.")

(defvar emf-current-filter emf-no-filter
  "The current filter function")

(defvar emf-filter-menu '()
  "A list of available filters grouped by factory.")

(defun emf-browser-filter-hook (track)
  "A hook function for the browser. Freewill here for TRACK filtering.
First we test the track against the current ring filter, then we check
the result with the result of the emf-current-filter."
  (not  ;; :-{} We invert them to combine them,
   ;; and determine if we dont want it. I'd rather filters be a keep=t.
   ;; repercussions of having a 'dont want this' true variable.
   ;; Its how all the filters have been prescribed to be made in nots.
   (and (not (and emf-current-ring-filter
                  (funcall (cdr emf-current-ring-filter) track)))
        (not (and emf-current-filter
                  (funcall (cdr emf-current-filter) track))))))

(defconst emf-hook-filter (cons "emf-filter-hook" 'emf-browser-filter-hook)
  "A filter cons with the filter hook to give to the browser.")

(emms-browser-set-filter emf-hook-filter)

(defun emf-register-filter (filter-name filter)
  "Put our new FILTER function named FILTER-NAME in our filter list."
  (push (cons filter-name filter) emf-filters))

(defun emf-add-filter-menu (folder-name filter-list)
  "Add a list of FILTER-LIST as another FOLDER-NAME in the filter selection menu."
  (setq emf-filter-menu
        (cons (list folder-name filter-list)
              emf-filter-menu)))

(defun emf-add-to-filter-menu (folder-name filter-name)
  "Add FILTER-NAME to menu tree by FOLDER-NAME."
  (if (assoc folder-name emf-filter-menu)
      (push filter-name (cadr (assoc folder-name emf-filter-menu)))
    (setq emf-filter-menu
          (cons (list folder-name (list filter-name))
                emf-filter-menu))))

(defun emf-make-filter-ring (list-of-filter-names)
  "Make a ring of filter names from the LIST-OF-FILTER-NAMES.
Appends the 'no filter' filter."
  (setq emf-filter-ring
        (make-ring
         (+ 1 (length list-of-filter-names))))
  (mapcar (lambda (filter-name)
            (ring-insert emf-filter-ring filter-name))
          (cons "no filter" list-of-filter-names)))

;; This should allow people to continue using the emms-browser
;; filtering as they always have, reusing the filters they've already made.
;; The browser filter is essentially nothing more than a ring of
;; filters with no other interface.
(defun emf-make-filter-ring-from-browser-filters ()
  "Integrate Emms browser filters into emf-filters.
Make a filter ring from the emms browser filter entry names,
and add the filters to emf-filters to be used.
Add the browser-filter-names to the filter selection
menu in a folder named 'browser-filters'."
  (debug)
  (when emms-browser-filters
    (let ((name-list (mapcar 'car emms-browser-filters)))
      (emf-make-filter-ring name-list)
      (emf-add-to-filter-menu "browser-filters" name-list)
      (setq emf-filters (append emms-browser-filters emf-filters)))))

(defun emf-list-filters ()
  "List the filters in our filter list."
  (mapcar 'car emf-filters))

(defun emf-show-filters ()
  "Show the filters we have."
  (when emf-filters
    (message "Emf Filters:\n%s"
             (mapconcat 'identity (emf-list-filters) "\n"))))

(defun emf-find-filter (name)
  "A nicer way to find NAME in our list of filters."
  (assoc name emf-filters))

(defun emf-find-filter-function (filter-name)
  "Find the Function for FILTER-NAME in emf-filters.
Pass functions through untouched."
  (if (eq filter-name :not)
      :not
    (cdr (assoc filter-name emf-filters))))

;; over-ride the one in browser...
(defun emms-browser-format-search-list (search-list)
  "Create a string format of a SEARCH-LIST."
  (let ((infos (append (car (car search-list))))
        (svalue (cdar search-list)))
    (format "%s - %s"
            (mapconcat
             `(lambda (info)
                (if (symbolp info)
                    (substring (symbol-name info)  5)
                  info))
             infos " | ")
            svalue)))

(defun emf-hard-filter ()
  "A hard save of filtered results.
Build a cache of filtered tracks from the last search cache
filtered by the current filters.
Emulates a search, pushing a new cache on the search stack.
This cache is the same as all the rest and emms-cache-db.
The name is a munge to make the search list formatter happy."
  (interactive)
  ;; emulate a search list for the search name formatter.
  ;; (((field1 field2) string))
  (let* ((previous-search-name
          (emms-browser-format-search-list
           (car (emms-browser-get-search-keys))))

         (search-name
          (list (list (list previous-search-name "Filter")
                      (concat (car emf-current-ring-filter) ":"
                              (car emf-current-filter)))))

         (search-cache (make-hash-table
                        :test (if (fboundp 'define-hash-table-test)
                                  'string-hash
                                'equal))))
    (maphash (lambda (path track)
               (when (not (emf-browser-filter-hook track))
                 (puthash path track search-cache)))
             (emms-browser-last-search-cache))

    (emms-browser-cache-search search-name search-cache))
  (emms-browser-search-refilter))

(defun emf-new-filter ()
  "Build a new filter from a filter factory interactively."
  (interactive)
  nil)

;; Filter Factories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is the real filter maker - from factories.
;; they still have to be registered to use them by natural language name.
(defun emf-make-filter (factory filter-name factory-args)
  "Make a filter named FNAME using the FACTORY and FACTORY-ARGS.
if factory is a function it is used directly. Otherwise, it will
look for the function in emf-filter-factories."
  (let* ((func (if (functionp factory)
                   factory
                 (cadr (assoc factory emf-filter-factories))))
         (filter (apply func factory-args)))
    (emf-add-to-filter-menu factory filter-name)
    (emf-register-filter filter-name filter)))

(defun emf-make-filters (filter-list)
  "Make filters in FILTER-LIST into filter functions.
The filter list holds entries specified as (factory-name
filter-name factory-arguments)."
  (mapcar (lambda (filter)
            (emf-make-filter
             (car filter)
             (cadr filter) (cddr filter)))
          filter-list))

(defun emf-register-filter-factory (name func prompt-list)
  "Register FUNC as NAME with PROMPT-LIST into a filter choice.
Give it the shape: (name . (func . prompt-list))."
  (push
   (cons name (cons func prompt-list))
   emf-filter-factories))

(defun emf-list-filter-factories ()
  "List the filters in our factories list."
  (mapcar 'car emf-filter-factories))

(defun show-filter-factories ()
  "Show the filter factories we have."
  (when emf-filter-factories
    (message "Emf Filter Factories:\n%s"
             (mapconcat 'identity "\n" (emf-filter-factories)))))

(defun emf-clear-filter-factories ()
  "Reset the filter factory list."
  (setq emf-filter-factories nil))


;;; Filters
;; Here are some filter factories.
;; A filter factory is a function that returns a function which has been
;; set up to filter the values given in whichever way it does that.
;;
;; Here are some factories.
;;
;; Directory name - track path.
;; track type, 'file, 'stream ????
;; Genre.
;; Year-range
;; Year-greater
;; Year-less
;; played since
;; Not played since
;; Number field compare
;; String field compare
;; duration/playing time ><
;; Multi-filter  - A filter made of filters.

;; by registering the factory functions, they go into the factory list
;; and become choices to make new filters from.
;; They can also be easily referenced by their Names
;; when creating new filters code.

;; Factory Functions to make filter functions with.
(defun emf-make-filter-directory (dirname)
  "Generate a function to check if a track is in DIRNAME.
If the track is not in DIRNAME, return t.
Uses a regex anchoring dirname to the beginning of the expanded path."
  (lexical-let ((re (concat "^" (expand-file-name dirname))))
    #'(lambda (track)
        (not (string-match re (emms-track-get track 'name))))))

(emf-register-filter-factory "directory"
                             'emf-make-filter-directory
                             '("Directory: "))

;; seconds in a day (* 60 60 24) = 86400
(defun emf-make-filter-played-within (days)
  "Show only tracks played within the last number of DAYS."
  (lexical-let ((seconds-to-time (seconds-to-time (* days 86400))))
    #'(lambda (track)
        (let ((min-date (time-subtract
                         (current-time)
                         seconds-to-time))
              last-played)
          (not (and (setq last-played
                          (emms-track-get track 'last-played nil))
                    (time-less-p min-date last-played)))))))

(emf-register-filter-factory "Played Since"
                             'emf-make-filter-played-within
                             '("days: "))

(defun emf-make-filter-not-played-within (days)
  "Make a not played since DAYS filter."
  (lambda (track)
    (not (funcall (emf-filter-played-within days) track))))

(emf-register-filter-factory "Not Played Since"
                             'emf-make-filter-not-played-within
                             '("days: "))

;; Getting the year is special. It might be in year or date.
(defun emf-get-year (track)
  "Get the year from a TRACK. Check year and date fields.
Returns a number"
  (let* ((year (emms-track-get track 'info-year))
         (date (emms-track-get track 'info-date))
         (year (or year (emms-format-date-to-year date)))
         (year (and year (string-to-number year))))
    year))

(defun emf-make-filter-year-range (y1 y2)
  "Make a date range filter from Y1 and Y2."
  (lexical-let ((local-y1 y1)
                (local-y2 y2))
    #'(lambda (track)
        (let ((year (emf-get-year track)))
          (not (and
                year
                (<= local-y1 year)
                (>= local-y2 year)))))))

(emf-register-filter-factory "year range"
                             'emf-make-filter-year-range
                             '("start-year: " "end-year: "))

(defun emf-make-filter-year-greater (year)
  "Make a Greater than year filter from YEAR."
  (lexical-let ((local-year year))
    #'(lambda (track)
        (let ((year (emf-get-year track)))
          (not (and
                year
                (<= local-year year)))))))

(emf-register-filter-factory "greater than Year"
                             'emf-make-filter-year-greater
                             '("Year: "))

(defun emf-make-filter-year-less (year)
  "Make a Less than year filter from YEAR."
  (lexical-let ((local-year year))
    #'(lambda (track)
        (let ((year (emf-get-year track)))
          (not (and
                year
                (>= local-year year)))))))

(emf-register-filter-factory "less than Year"
                             'emf-make-filter-year-less
                             '("Year: "))

(defun emf-make-filter-field-compare (operator-func field compare-val)
  "Make a filter that compares FIELD to COMPARE-VALUE with OPERATOR-FUNC.
Works for number fields and string fields provided the appropriate
operator function and comparison function"
  (lexical-let ((local-operator operator-func)
                (local-field field)
                (local-compare-val compare-val))
    #'(lambda (track)
        (let ((track-val (emms-track-get track local-field)))
          (not (and
                track-val
                (funcall local-operator track-val local-compare-val)))))))

(emf-register-filter-factory "field compare"
                             'emf-make-filter-field-compare
                             '("operator: " "field: " "compare to: "))

;; Generic field comparison factories.
;; parameter order is good for making partials.
(setq emf-make-filter-duration-less
      (apply-partially 'emf-make-filter-field-compare
                       '<= 'info-playing-time))

(emf-register-filter-factory "duration less"
                             emf-make-filter-duration-less
                             '("duration: " ))

(setq emf-make-filter-duration-more
      (apply-partially 'emf-make-filter-field-compare
                       '>= 'info-playing-time))

(emf-register-filter-factory "duration more"
                             emf-make-filter-duration-more
                             '("duration: " ))

(setq emf-make-filter-genre
      (apply-partially 'emf-make-filter-field-compare
                       'string-equal-ignore-case 'info-genre))

(emf-register-filter-factory "genre"
                             emf-make-filter-genre
                             '("genre: " ))

(setq emf-make-filter-type
      (apply-partially 'emf-make-filter-field-compare
                       'eq 'type))

(emf-register-filter-factory "track type"
                             emf-make-filter-type
                             '("track type: "))


;; Multi-filter  - Just another factory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A filter of filters. A list of lists of filter Names.
;; Each list is Ored together and then Anded with each other.
(defun emf-or-group->multi-funcs (filter-name-list)
  "Return a list of functions from emf-filters for a FILTER-NAME-LIST.
Functions already in the list will be passed through."
  (mapcar (lambda (filter-name)
            (emf-find-filter-function filter-name))
          filter-name-list))

(defun emf-meta-filter->multi-funcs (meta-filter)
  "Return a list of functions from emf-filters for a META-FILTER."
  (mapcar (lambda (or-group)
            (emf-or-group->multi-funcs or-group))
          meta-filter))

(defun emf-reduce-or-group (or-group track)
  "Reduce OR-GROUP for TRACK."
  (cl-reduce
   (lambda (result filter-func)
     (or result
         (not
          (funcall filter-func track))))
   or-group
   :initial-value nil))

(defun emf-reduce-invert-or-group (or-group track)
  "Call an OR-GROUP list of filters with TRACK and reduce result with OR.
If the first function is 'not then invert the result from the reduction."
  (let* ((invert (eq (car or-group) :not))
         (group (if invert
                    (cdr or-group)
                  or-group))
         (result (emf-reduce-or-group group track)))
    (if invert (not result) result)))

(defun emf-make-multi-filter (meta-filter)
  "Make a track filter function from META-FILTER.
The function will take a track as a parameter and return t if the track
does not match the filters.
A multi-filter is a list of lists of filter names.
The track is checked against each filter, each list of filters is
reduced with or. The lists are reduced with and.
Returns True if the track should be filtered out."
  (lexical-let ((local-multi-funcs
                 (emf-meta-filter->multi-funcs meta-filter)))
    #'(lambda (track)
        (not (cl-reduce
              (lambda (result funclist)
                (and result
                     (emf-reduce-invert-or-group funclist track)))
              local-multi-funcs
              :initial-value t)))))

(emf-register-filter-factory "multi-filter"
                             'emf-make-multi-filter
                             '(nil))

;; better name for this use.
(defun emf-meta-filter->multi-filter (meta-filter)
  "The real META-FILTER deal.
Simply turn a meta-filter into a multi-filter function."
  (emf-make-multi-filter meta-filter))

;; Some simple filters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A simple not a filter, So we have a default of no filters to choose/return to.
(emf-register-filter "no filter" 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;             factory      name        factory arg
(setq emf-decade-filters
      '(("year range" "1900s"     1900 1909)
        ("year range" "1910s"     1910 1919)
        ("year range" "1920s"     1920 1929)
        ("year range" "1930s"     1930 1939)
        ("year range" "1940s"     1940 1949)
        ("year range" "1950s"     1950 1959)
        ("year range" "1960s"     1960 1969)
        ("year range" "1970s"     1970 1979)
        ("year range" "1980s"     1980 1989)
        ("year range" "1990s"     1990 1999)
        ("year range" "2000s"     2000 2009)
        ("year range" "2010s"     2010 2019)
        ("year range" "2020s"     2020 2029)))

(setq emf-genre-filters
      '(("genre" "waltz"      "waltz")
        ("genre" "vals"       "vals")
        ("genre" "tango"      "tango")
        ("genre" "milonga"    "milonga")
        ("genre" "condombe"   "condombe")
        ("genre" "salsa"      "salsa")
        ("genre" "blues"      "blues")
        ("genre" "rock"       "rock")
        ("genre" "swing"      "swing")
        ("genre" "pop"        "pop")
        ("genre" "rap"        "rap")
        ("genre" "hip hop"    "hip hop")
        ("genre" "classical"  "classical")
        ("genre" "baroque"    "baroque")
        ("genre" "chamber"    "chamber")
        ("genre" "reggae"     "reggae")
        ("genre" "folk"       "folk")
        ("genre" "world"      "world")
        ("genre" "metal"      "metal")
        ("genre" "fusion"     "fusion")
        ("genre" "jazz"       "jazz")))

(setq emf-last-played-filters
      '(("Played Since" "Played in the last month" 30)
        ("Not Played Since" "Not played since a year" 365)))

(setq emf-misc-filters
      '(("track type" "only files" file)))

(setq emf-duration-filters
      '(("duration less" "duration <1 min"  60)
        ("duration less" "duration <5 min"  300)
        ("duration more" "duration >5 min"  300)
        ("duration more" "duration >10 min" 600)))

(setq some-multi-filters
      '(("multi-filter"
         "1930-1949"
         (("1930-1939" "1940-1949")) )

        ("multi-filter"
         "vals | waltz"
         (("vals" "waltz")))

        ("multi-filter"
         "milonga | condombe"
         (("milonga" "condombe")))

        ("multi-filter"
         "vals && 1930-1949"
         (("vals")
          ("1930-1949")))

        ("multi-filter"
         "vals or milonga, 1930-1959"
         (("1930-1949" "1950-1959")
          ("vals | milonga")))))

(defun emf-make-default-filters()
  "Make some default filters anyone would not mind having."
  (emf-make-filters emf-decade-filters)
  (emf-make-filters emf-genre-filters)
  (emf-make-filters emf-misc-filters)
  (emf-make-filters emf-last-played-filters)
  (emf-make-filters emf-duration-filters)
  )

;; Install some default filters.
(emf-make-default-filters)

;; Some multi-filters.
(setq some-multi-filters
      '(("1930-1949"
         (("1930-1939" "1940-1949")) )

        ("Vals | Waltz"
         (("Vals" "Waltz")))

        ("Milonga | Condombe"
         (("Milonga" "Condombe")))

        ("Vals | Milonga"
         (("Vals" "Milonga")))

        ("Vals && 1930-1949"
         (("Vals")
          ("1930-1949")))

        ("Vals or milonga, 1930-1959"
         (("1930-1949" "1950-1959")
          ("Vals | Milonga")))))


;; (emf-make-multi-filters some-multi-filters)


;; The Meta filter
;; An interactive multi-filter stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The current filter is the top of a meta-filter stack.
;; We can add to the current filter which pushes a new filter to
;; the stack. emf-pop pops the stack.
;; Other filters can be added to the current filter
;; with 'and', 'or' as well as 'and not' filter selections.
;; A filter can be popped, another filter can be chose
;; or the stack can be cleared.
;; A filter may be kept for the session with keep.
;; emf-status will print the stack and the current filter.

;; Any time it changes it makes a new multi-filter filter function with
;; what is on top of the stack, and we tell the browser to re-render.

(defun emf-current-meta-filter ()
  "Return the current meta-filter from the top of the stack."
  (car emf-stack))

(defun emf-copy-meta-filter (filter)
  "Copy the meta-filter given by FILTER."
  (mapcar 'copy-sequence filter))

(defun emf-filter-name->meta-filter (filter-name)
  "Make a meta filter cons from a FILTER-NAME."
  (cons filter-name
        (list (list filter-name))))

(defun emf-format-meta-filter-groups (filter-list)
  "Format the FILTER-LIST contents to a list of strings."
  (mapconcat (lambda (fname) (format "%s " fname)) filter-list " | "))

(defun emf-make-name (meta-filter)
  "Construct a name from the META-FILTER contents."
  (mapconcat 'identity
             (mapcar 'emf-format-meta-filter-groups meta-filter) " && "))

(defun emf-make-filter-cons-from-meta-filter (filter)
  "Make a filter cons from multi-filter list FILTER."
  (cons (emf-make-name filter) filter))

(defun emf-set-filter ()
  "Set the current filter function and name for the browser.
Run the filter changed hook for the browser ?."
  (setq emms-browser-current-filter-name (caar emf-stack))
  (setq emf-current-filter
        (cons
         (caar emf-stack)
         (emf-meta-filter->multi-filter (cdar emf-stack))))
  (run-hooks 'emms-browser-filter-changed-hook))

(defun emf-browser-render ()
  "Tell the browser to re-render."
  (if (string= (buffer-name) emms-browser-search-buffer-name)
      (emms-browser-search-refilter)
    (emms-browse-by (or emms-browser-top-level-type
                        emms-browser-default-browse-type))))

(defun  emf-refilter ()
  "Make a multi-filter function from the current meta-filter.
Set it to be the current filter function and re-render."
  (emf-set-filter)
  (emf-browser-render))

(defun emf-ensure-metafilter (filter)
  "Ensure that FILTER is a meta-filter."
  (cond ((stringp filter)           ; name
         (emf-filter-name->meta-filter filter))
        ((functionp (cdr filter))       ; filter function
         (emf-filter-name->meta-filter (car filter)))
        ;; meta-filter - cdr is a listp
        (t filter)))

(defun emf-push (filter)
  "Push a copy of FILTER to the meta-filter stack.
Should be of the form (filter-name . metafilter/filter)
or a filter-name.
Make a filter function and set it. If it is a name,
look it up in our filter list. If it is a function, make
it a meta-filter, if it is a meta-filter use it."
  (push (emf-ensure-metafilter filter)
        emf-stack)
  (emf-refilter))

;;; base functions
(defun emf-current-meta-filter-name ()
  "Give the constructed name of the current filter."
  (emf-make-name (cdar emf-stack)))

(defun  emf-clear ()
  "Clear the meta filter stack and the current filter function."
  (setq emf-stack nil)
  (emf-refilter))

(defun  emf-pop ()
  "Pop the stack, set the current filter function and re-render."
  (interactive)
  (pop emf-stack)
  (emf-refilter))

(defun  emf-swap ()
  "Reverse the last two entries in the stack."
  (interactive)
  (let* ((current (pop emf-stack))
         (previous (pop emf-stack)))
    (push current emf-stack)
    (push previous emf-stack)
    (emf-refilter)))

(defun  emf-swap-pop ()
  "Swap and pop the stack."
  (interactive)
  (let* ((current (pop emf-stack)))
    (pop emf-stack)
    (push current emf-stack)))

(defun  emf-squash ()
  "Squash the stack, keep the top."
  (interactive)
  (let* ((current (pop emf-stack)))
    (setq emf-stack nil)
    (push current emf-stack)))

;;; current state functions.
(defun  emf-keep ()
  "Register the current filter into the list of filters for the session."
  (interactive)
  (message "Registering the current meta-filter as a filter for the session")
  (emf-status)

  (if (and emf-stack (consp (car emf-stack)))
      (emf-register-filter (caar emf-stack)
                           (emf-meta-filter->multi-filter (cdar emf-stack)))))

(defun  emf-default ()
  "Set to default filter."
  (interactive)
  (emf-push
   (emf-make-filter-cons-from-meta-filter '((emf-default-filter)))))

(defun emf-choose-filter-recursive (&optional choices)
  "Choose a filter from emf-filter-menu tree or the alist given as CHOICES.
Requires that the lists of filter names be lists of cons (name . name).
Allows for tree structures of any depth."
  (let* ((choices (or choices emf-filter-menu))
         (choice (assoc (completing-read
                         "Choose a filter or group:" choices nil t)
                        choices)))
    (if (consp choice)
        (emf-choose-filter (cadr choice))
      choice)))

(defun emf-choose-filter ()
  "Choose a filter from our filter menu tree.
Stupid, Assumes our tree is an alist of lists of strings."
  (completing-read
   "Choose a filter:"
   (cadr (assoc (completing-read
                 "Choose a filter group:" emf-filter-menu nil t)
                emf-filter-menu))
   nil t))

(defun emf-select-push ()
  "Select a filter from the list of filter functions."
  (interactive)
  (let ((fname (emf-choose-filter)))
    (emf-push fname)))

(defun emf-select-smash ()
  "Clear the stack and Select a filter from the list of filter functions."
  (interactive)
  (emf-clear)
  (let ((fname (emf-choose-filter)))
    (emf-push fname)))

(defun emf-push-or (filter-name meta-filter)
  "Push a new Or with FILTER-NAME to the last Or group in the META-FILTER."
  (let* ((rev-mf (reverse (emf-copy-meta-filter meta-filter)))
         (rest-mf (reverse (cdr rev-mf))))
    (append rest-mf
            (list (append (car rev-mf) (list filter-name))))))

(defun  emf-select-or ()
  "Add filter to current/last filter list in the current filter.
Creates an 'OR' filter."
  (interactive)
  (let ((fname (emf-choose-filter)))
    (emf-push
     (emf-make-filter-cons-from-meta-filter
      (emf-push-or fname (emf-copy-meta-filter (cdar emf-stack)))))))

(defun emf-push-and (filter-name filter)
  "Push a new And list with FILTER-NAME onto FILTER."
  (append filter (list (list filter-name))))

(defun  emf-select-and ()
  "Select a filter to start a new list of filters.
Creates a new 'AND' list of filters."
  (interactive)
  (let ((fname (emf-choose-filter)))
    (emf-push
     (emf-make-filter-cons-from-meta-filter
      (emf-push-and fname (emf-copy-meta-filter (cdar emf-stack)))))))

(defun  emf-select-and-not ()
  "Select a filter to start a new list of filters.
Creates a new 'AND' list of filters."
  (interactive)
  (let ((fname (emf-choose-filter)))
    (emf-push
     (emf-make-filter-cons-from-meta-filter
      (emf-push-or fname
                   (emf-push-and ':not (emf-copy-meta-filter (cdar emf-stack))))))))

(defun emf-format-stack()
  "Print the stack."
  (format  "\t%s" (mapconcat 'car emf-stack "\n\t")))

(defun emf-format-search-stack ()
  "Format the search stack."
  (interactive)
  (format "\t%s" (mapconcat #'identity (reverse (emms-browser-search-crumbs)) "\n\t")))

(defun emf-status ()
  "Print what we know."
  (interactive)
  (message (format "Ring: %s\nCurrent: %s\nSearch: %s"
                   (car emf-current-ring-filter)
                   (emf-current-meta-filter)
                   (emf-format-stack)
                   (emf-format-search-stack)
                   )))

(defun emf-set-ring-filter (filter-name)
  "Given a FILTER-NAME set the current ring filter and re-render."
  (setq emf-current-ring-filter
        (assoc filter-name emf-filters))
  (emf-browser-render))

(defun emf-current-ring-filter-name ()
  "The current ring filter name, more descriptive than car."
  (if emf-current-ring-filter
      (car emf-current-ring-filter)
    "no filter"))

(defun emf-next-ring-filter()
  "Move to the next filter in the filter ring."
  (interactive)
  (emf-set-ring-filter
   (ring-next emf-filter-ring
              (emf-current-ring-filter-name))))

(defun emf-previous-ring-filter()
  "Move to the previous filter in the filter ring."
  (interactive)
  (emf-set-ring-filter
   (ring-previous emf-filter-ring
                  (emf-current-ring-filter-name))))

(setq emms-browser-mode-map
      (let ((map emms-browser-mode-map))
        (define-key map (kbd "f S") #'emf-status)
        (define-key map (kbd "f q") #'emf-pop)
        (define-key map (kbd "f r") #'emf-swap) ; rotate ?
        (define-key map (kbd "f R") #'emf-swap-pop) ; rotate-eject, ,pop-previous
        (define-key map (kbd "f f") #'emf-squash) ;flatten
        (define-key map (kbd "f k") #'emf-keep)
        (define-key map (kbd "f h") #'emf-hard-filter)
        (define-key map (kbd "f c") #'emf-clear)
        (define-key map (kbd "f >") #'emf-next-ring-filter)
        (define-key map (kbd "f <") #'emf-previous-ring-filter)
        (define-key map (kbd "f p") #'emf-select-push)
        (define-key map (kbd "f s") #'emf-select-smash)
        (define-key map (kbd "f o") #'emf-select-or)
        (define-key map (kbd "f a") #'emf-select-and)
        (define-key map (kbd "f n") #'emf-select-and-not)
        (define-key map (kbd ">") #'emf-next-ring-filter)
        (define-key map (kbd "<") #'emf-previous-ring-filter)
        map))

(provide 'emms-filters)
;;; emms-filters.el ends here.
