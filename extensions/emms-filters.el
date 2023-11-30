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

(defvar emf-current-multi-filter nil
  "The current multi-filter source list. ie. (cadr emf-stack).")

(defvar emf-filter-factories '()
  "An alist of filter functions and their argument lists.")

(defvar emf-filters '()
  "A list of available filters.")

(defun emf-register-filter (filter-name filter)
  "Put our new FILTER function named FILTER-NAME in our filter list."
  (push (cons filter-name filter) emf-filters))

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
  "Find the Function for FILTER-NAME in emf-filters."
  (cdr (assoc filter-name emf-filters)))

;; Filter Factories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are not working yet, again, defmacro closure crap.
;;
;; ;; this is the real filter maker - from factories.
;; ;; they still have to be registered to use them by natural language name.
;; (defun emf-make-filter (factory fname factory-args)
;;   "Make a filter named FNAME using the FACTORY and FACTORY-ARGS.
;; if factory is a function it is used directly. Otherwise, it will
;; look for the function in emf-filter-factories."
;;   (let* ((filter-name (string fname))
;;          (func (if (functionp factory)
;;                    factory
;;                  (cadr (assoc factory emf-filter-factories))))
;;          ;; If we put them in lists, they're two deep here, car!.
;;          (filter (apply func (car factory-args))))
;;     (emb-register-filter filter-name filter)))

;; (defun emf-make-filters (filter-list)
;;   "Make filters in FILTER-LIST into filter functions.
;; The filter list holds entries specified as (factory-name
;; filter-name factory-arguments)."
;;   (mapcar (lambda (filter)
;;             (emf-make-filter
;;              (car filter)
;;              (cadr filter) (cddr filter)))
;;           filter-list))

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
(defun emf-filter-only-dir (dirname)
  "Generate a function to check if a track is in DIRNAME.
If the track is not in DIRNAME, return t."
  (lexical-let ((re (concat "^" (expand-file-name dirname))))
    #'(lambda (track)
        (not (string-match re (emms-track-get track 'name))))))

(emf-register-filter-factory "Only Directory"
                             'emf-filter-only-dir
                             '("Directory: "))

(defun emf-filter-only-type (type)
  "Generate a function to check a track's type.
If the track is not of TYPE, return t."
  (lexical-let ((local-type type))
    #'(lambda (track)
        (not (eq local-type (emms-track-get track 'type))))))

(emf-register-filter-factory "Only type"
                             'emf-filter-only-type
                             '("Track Type: "))

;; seconds in a day (* 60 60 24) = 86400
(defun emf-filter-only-recent (days)
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
                             'emf-filter-only-recent
                             '("days: "))

(defun emf-make-filter-not-recent (days)
  "Make a not played since DAYS filter."
  (lambda (track)
    (not (funcall (emms-browser-filter-only-recent days) track))))

(emf-register-filter-factory "Not Played Since"
                             'emf-make-filter-not-recent
                             '("days: "))

(defun emf-make-filter-genre (genre)
  "Make a filter by GENRE."
  (lexical-let ((local-genre genre))
    #'(lambda (track)
        (let ((info (emms-track-get track 'info-genre)))
          (not (and info (string-equal-ignore-case local-genre info)))))))

(emf-register-filter-factory "Genre"
                             'emf-make-filter-genre
                             '("genre: "))

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

(emf-register-filter-factory "Year range"
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

(emf-register-filter-factory "Greater than Year"
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

(emf-register-filter-factory "Less than Year"
                             'emf-make-filter-year-less
                             '("Year: "))

(defun emf-make-filter-number-field-compare (operator-func field compare-number)
  "Make a filter that compares FIELD as a number to COMPARE-NUMBER with OPERATOR-FUNC."
  (lexical-let ((local-operator operator-func)
                (local-field field)
                (local-compare-val compare-number))
    #'(lambda (track)
        (let ((track-val (string-to-number (emms-track-get track field)))
              (not (and
                    track-val
                    (funcall operator-func compare-number track-val)))))))

  ;; parameter order is good for making partials.
  ;; (setq emf-make-filter-duration-less
  ;;       (-partial emf-make-filter-number-field-compare
  ;;                 '('<= 'info-playing-time)))

  ;; Not sure how I can prompt for this.
  ;; I'll worry about it when I get there.
  (emf-register-filter-factory "Number compare"
                               'emf-make-filter-number-field-compare
                               '("operator: " "field: " "compare to: ")))

(defun emf-make-filter-string-field-compare (operator-func field compare-string)
  "Make a filter that compares FIELD as a string to COMPARE-STRING with OPERATOR-FUNC."
  (lexical-let ((local-operator operator-func)
                (local-field field)
                (local-compare-val compare-string))
    #'(lambda (track)
        (let ((track-val (emms-track-get track local-field))
              (not (and
                    track-val
                    (funcall local-operator local-compare-val track-val))))))))

(emf-register-filter-factory "String compare"
                             'emf-make-filter-string-field-compare
                             '("operator: " "field: " "compare to: "))

;; A simple not a filter, So we have a default of no filters to choose/return to.
(emf-register-filter "No Filter" 'ignore)

;; Some simple filters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;             factory      name        factory arg
(setq emf-decade-filters
      (list '("Year range" "1900s"     '(1900 1909))
            '("Year range" "1910s"     '(1910 1919))
            '("Year range" "1920s"     '(1920 1929))
            '("Year range" "1930s"     '(1930 1939))
            '("Year range" "1940s"     '(1940 1949))
            '("Year range" "1950s"     '(1950 1959))
            '("Year range" "1960s"     '(1960 1969))
            '("Year range" "1970s"     '(1970 1979))
            '("Year range" "1980s"     '(1980 1989))
            '("Year range" "1990s"     '(1990 1999))
            '("Year range" "2000s"     '(2000 2009))
            '("Year range" "2010s"     '(2010 2019))
            '("Year range" "2020s"     '(2020 2029))))

(setq emf-genre-filters
      (list '("Genre" "Waltz"      ("waltz"))
            '("Genre" "Vals"       ("vals"))
            '("Genre" "Tango"      ("tango"))
            '("Genre" "Milonga"    ("milonga"))
            '("Genre" "Condombe"   ("condombe"))
            '("Genre" "Salsa"      ("salsa"))
            '("Genre" "Blues"      ("blues"))
            '("Genre" "Rock"       ("rock"))
            '("Genre" "Swing"      ("swing"))
            '("Genre" "Pop"        ("pop"))
            '("Genre" "Rap"        ("rap"))
            '("Genre" "Hip Hop"    ("hip hop"))
            '("Genre" "Classical"  ("classical"))
            '("Genre" "Baroque"    ("baroque"))
            '("Genre" "Chamber"    ("chamber"))
            '("Genre" "Reggae"     ("reggae"))
            '("Genre" "Folk"       ("folk"))
            '("Genre" "World"      ("world"))
            '("Genre" "Metal"      ("metal"))
            '("Genre" "Fusion"     ("fusion"))
            '("Genre" "Jazz"       ("jazz"))))

(setq emf-last-played-filters
      (list '("Played Since" "Played in the last month" (30))
            '("Not Played Since" "Not played since a year" (365))))

(setq emf-misc-filters
      (list '("Only type" "Only Files" ('file))))

(setq emf-duration-filters
      (list '("Number compare" "duration <60"    ('<= 'info-playing-time 60))
            '("Number compare" "duration <5 min" ('<= 'info-playing-time 300))
            '("Number compare" "duration >5 min" ('>= 'info-playing-time 300))
            '("Number compare" "duration >10 min" ('>= 'info-playing-time 600))))

;; (defun emf-make-default-filters()
;;   "Make some default filters anyone would not mind having."
;;   (emf-make-filters emf-decade-filters)
;;   (emf-make-filters emf-genre-filters)
;;   (emf-make-filters emf-misc-filters)
;;   (emf-make-filters emf-last-played-filters)
;;   (emf-make-filters emf-duration-filters))

;; Install some default filters.
;; (emf-make-default-filters)

;; Multi-filter  - Just another factory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A filter of filters. A list of lists of filter Names.
;; Each list is Ored together and then Anded with each other.

;; should try integrating multi-func resolution at function creation.

(defun emf-meta-filter->multi-funcs (filter-name-list)
  "Return a list of functions from emf-filters for a FILTER-NAME-LIST."
  (list (mapcar (lambda (filter-name)
                  (emf-find-filter-function filter-name))
                filter-name-list)))

(defun emf-reduce-or-group (or-group track)
  "Call an OR-GROUP list of filters with TRACK and reduce result with OR."
  (cl-reduce
   (lambda (result filter-name)
     (or result
         (not
          (funcall  (emf-find-filter-function filter-name) track))))
   or-group
   :initial-value nil))

(defun emf-make-multi-filter (meta-filter)
  "Make a track filter function from MULTI-FILTERS-LIST.
The function will take a track as a parameter and return t if the track
does not match the filters.
A multi-filter is a list of lists of filter names.
The track is checked against each filter, each list of filters is
reduced with or. The lists are reduced with and.
Returns True if the track should be filtered out."
  (lexical-let ((local-meta-filter meta-filter))
    #'(lambda (track)
        (not (cl-reduce
              (lambda (result funclist)
                (and result
                     (emf-reduce-or-group funclist track)))
              local-meta-filter
              :initial-value t)))))

;; patching together for now for the meta-filter stack.
(defun emf-meta-filter->multi-filter (meta-filter)
  "The real META-FILTER deal."
  (emf-make-multi-filter meta-filter))

;; (emf-multi-filter-funcs '("vals"))

;; (defun emf-reduce-meta-filter (meta-filter track)
;;   "Filter TRACK with filter functions from META-FILTER.
;; Reduce the OR groups together with AND."
;;   (let* ((multi-funcs (emf-meta-filter->multi-funcs meta-filter)))
;;     (not (cl-reduce
;;           (lambda (result or-group)
;;             (and result
;;                  (reduce-filters-with-track or-group track)))
;;           multi-funcs
;;           :initial-value t))))

;; (defun emf-meta-filter->multi-filter (meta-filter)
;;   "Make a track filter function from META-FILTER.
;; The function will take a track as a parameter and return t if the track
;; does not match the filters.

;; A multi-filter is a list of lists of filter names.
;; That is transformed into lists of functions.
;; The track is checked against each filter, each list of filters is
;; reduced with or. The lists are reduced with and.
;; Returns True if the track should be filtered out."
;;   (lambda (track)
;;     (funcall 'emf-reduce-meta-filter meta-filter track)))

;; (defun emf-make-multi-filter (name meta-filter)
;;   "Turn NAME and META-FILTER into a multi-filter function.
;; Register the filter into the emf-filters list.
;; If name is nil, create a name from the META-FILTER."
;;   (debug)
;;   ;; emms-browser-make-filter
;;   (emf-register-filter
;;    (or name
;;        (emf-make-name meta-filter))
;;    (emf-meta-filter->multi-filter meta-filter)))

;; (defun emf-make-multi-filters (meta-filters-list)
;;   "Turn META-FILTERS-LIST into a multi-filter function."
;;   (debug)
;;   (mapcar (lambda (meta-filter)
;;             (emf-make-multi-filter (car meta-filter) (cdr meta-filter)))
;;           meta-filters-list))

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


                                        ; (emf-make-multi-filters some-multi-filters)


;; The Meta filter
;; An interactive multi-filter stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basically the current filter is the top of a meta-filter stack.
;; we can add to the current filter which pushes a new filter to
;; the stack. emf-pop pops the stack.
;; Another filter can be chosen or the stack can be cleared.
;; A filter may be kept for the session with keep.

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
  (mapconcat 'identity filter-list " | "))

(defun emf-make-name (meta-filter)
  "Construct a name from the META-FILTER contents."
  (mapconcat 'identity
             (mapcar 'emf-format-meta-filter-groups meta-filter) " && "))

(defun emf-make-filter-cons-from-meta-filter (filter)
  "Make a filter cons from multi-filter list FILTER."
  (cons (emf-make-name filter) filter))

(defun  emf-refilter ()
  "Make a multi-filter function from the current meta-filter.
Set it to be the current filter function and re-render."
  (emms-browser-refilter
   (cons
    (caar emf-stack)
    (emf-meta-filter->multi-filter (cdar emf-stack)))))

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
  (emf-make-name (cadr emf-stack)))

(defun  emf-clear ()
  "Clear the meta filter stack and the current filter function."
  (setq emf-stack nil)
  (emf-refilter))

(defun  emf-pop ()
  "Pop the stack, set the current filter function and re-render."
  (interactive)
  (pop emf-stack)
  (emf-refilter))

;;; current state functions.
(defun  emf-keep ()
  "Register the current filter into the list of filters for the session."
  (interactive)
  (pass)

  ;;(if (and emf-stack (consp (car emf-stack)))
  ;; (emf-register-filter (caar emf-stack)
  ;; (emf-meta-filter->multi-filter (cdar emf-stack))))
  )

(defun  emf-default ()
  "Set to default filter."
  (interactive)
  (emf-push
   (emf-make-filter-cons-from-meta-filter '((emf-default-filter)))))

(defun emf-choose-filter ()
  "Choose a filter from our filter list."
  (completing-read "Choose a filter:"
                   emf-filters nil t))

(defun emf-select ()
  "Select a filter from the list of filter functions."
  (interactive)
  (let ((fname (emf-choose-filter)))
    (emf-push fname)))

(defun emf-push-or (filter-name meta-filter)
  "Push a new Or with FILTER-NAME to the last Or group in the META-FILTER."
  (let* ((rev-mf (reverse (emf-copy-meta-filter meta-filter)))
         (rest-mf (reverse (cdr rev-mf))))
    (append rest-mf
            (list (append (car rev-mf) (list filter-name))))))

(defun  emf-or-select ()
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

(defun  emf-and-select ()
  "Select a filter to start a new list of filters.
Creates a new 'AND' list of filters."
  (interactive)
  (let ((fname (emf-choose-filter)))
    (emf-push
     (emf-make-filter-cons-from-meta-filter
      (emf-push-and fname (emf-copy-meta-filter (cdar emf-stack)))))))

(defun emf-print-stack()
  "Print the stack."
  ;; (message (format "%s" emf-stack))
  (message (format "%s" (mapconcat 'car emf-stack "\n"))))

(defun emf-status ()
  "Print what we know."
  (message (Format "%s\nStack size: %s\nCurrent: %s"
                   (emf-print-stack)
                   (length emf-stack)
                   (emf-current-meta-filter))))

(defun emf-next (&optional reverse)
  "Redisplay with the next filter. Reverse the order if REVERSE is true."
  (interactive)
  (let* ((list (if reverse
                   (reverse emf-filters)
                 emf-filters))
         (key emms-browser-current-filter-name)
         (next (cadr (member (assoc key list) list))))
    ;; wrapped
    (unless next
      (setq next (car list)))
    (emf-push next)))

(defun emf-previous ()
  "Redisplay with the previous filter."
  (interactive)
  (emf-next-filter t))

;; (setq emms-browser-mode-map
;;       (let ((map emms-browser-mode-map))
;;         (define-key map (kbd ">") #'emf-next)
;;         (define-key map (kbd "<") #'emf-previous)
;;         (define-key map (kbd "f s") #'emf-select)
;;         map))

(setq emms-browser-mode-map
      (let ((map emms-browser-mode-map))
        (define-key map (kbd "f q") #'emf-pop)
        (define-key map (kbd "f >") #'emf-next)
        (define-key map (kbd "f <") #'emf-previous)
        (define-key map (kbd "f r") #'emf-status)
        (define-key map (kbd "f c") #'emf-clear)
        (define-key map (kbd "f k") #'emf-keep)
        (define-key map (kbd "f s") #'emf-select)
        (define-key map (kbd "f o") #'emf-or-select)
        (define-key map (kbd "f a") #'emf-and-select)
        ;; (define-key map (kbd "f n") #'emf-and-not-select)
        map))

(provide 'emms-filters)
;;; emms-filters.el ends here.
