;;; emms-filters.el --- Filter for Emms          -*- lexical-binding: t; -*-
;; Copyright (C) 2023 2024
;; Author:  Erica Gebhart <e.a.gebhart@gmail.com> (Erica Gebhart)
;; Keywords: emms, filter, search, cache, stack

;;; Commentary:
;; This code allows you to filter and search the metadata cache.
;; This manages the search and filter functionalities of emms-browser.

;; Usage
;; -------------------------------------------------------------------
;; Use filters as before with <> keys in the browser buffer.
;; Search-by something to create a new cache, or emf-select-push to get started
;; building a filter.
;;
;; Use 'emf-status-print' to watch the stacks and filters in effect.

;; Use 'emf-show-filter-menu' to see a list of all filters known
;; organized by factory.
;;
;; Apply a filter with the functions
;;    emf-select-push, emf-select-or, emf-select-and, emf-select-and-not
;;    emf-select-smash and emf-select-one-shot.
;; manipulate the stack with the functions:
;; emf-push, emf-pop, emf-clear and emf-squash, swap and swap-pop.
;;
;; Interactively create and use new filters by choosing 'new filter'
;; in the filter selection lists.
;;
;; The function `emf-current-meta-filter' gives the multi-filter data source
;; for the the current filter.
;;
;; A filter can be 'kept'. The function 'emf-keep will create and register
;; a multi-filter of the current filter, adding it to the multi-filter menu.
;; This only lasts until the current Emacs session ends.
;; If emf-multi-filter-save-file is set, a usable multi-filter definition will also be
;; appended to the file.
;;
;; Manage the search cache with emf-hard-filter, emf-one-shot, emf-quick-one-shot,
;; emf-search-by, emf-pop-cache, emf-squash-caches,
;; emf-clear-caches, emf-push-db-cache
;;
;; Caches can be stashed for the session and pushed back to the stack
;; at any time. The Emms-DB is the default.
;;
;; Switch the active ring filter with <> which correspond to
;; emf-next-ring-filter and emf-previous-ring-filter.
;;
;; The filter stack can be cleared with emf-clear, the caches
;; with emf-clear-caches and the ring with emf-clear-ring-filter.
;;
;; All stacks and filters can be cleared with 'emf-clear-all


;; Some Definitions:
;; -------------------------------------------------------------------
;;    Filtering:  Displaying the narrowed results from looking for matches
;;                in a list of items.
;;    Search:
;;       The saving of the narrowed results created from filtering a list of items,
;;       such that future filtering and searching will have a smaller list of items.
;;
;;    Filter or filter cons,  a cons of the form (name . function)
;;         Registration takes care of this.
;;         Once a filter is properly constructed it will be a cons
;;         (name . filter-function) the functions are created with one of the
;;         filter factories.
;;
;;    Filter function - a function that takes a track as its argument
;;      and returns true or False.
;;
;;    Filter Factory: A function which creates a filter function given the
;;      the desired parameters.
;;
;;    Multi-filter:  A filter factory which is other filters combined
;;                   using Or, And as well as And-Not.
;;
;;    Meta-filter: A multi-filter data definition.
;;       The filter stack uses meta-filters in a cons
;;       like this; (name . meta-filter).
;;       Filter names for meta-filters can be easily constructed.
;;
;;       This meta-filter:
;;
;;            '(("Vals" "Milonga")
;;              ("1900-1929" "1929-1937"))
;;
;;       Is to:
;;       Match on genre of vals OR
;;                         milonga AND
;;                             any year between
;;                         1900-1929 OR
;;                         1929-1937.
;;
;;       Making one or more multi-filter is easy.
;;       (emf-make-filters
;;        '(("Multi-filter"
;;           "Vals | milonga - 1900-1937"
;;           (("Vals" "Milonga")
;;            ("1900-1929" "1929-1937")))));

;;    Meta-filter-stack:  An interactive stack of meta-filters which allow
;;      the creation, combination and use of all filters.
;;
;;    Filter-ring: A ring of filter names, which can be easily selected with
;;       next and previous controls. All filters created through
;;       'emms-browser-make-filter are added here by default.
;;
;;       The filter ring replaces the functionality of emms-browser-filters.
;;       The easiest way to make the filter ring is with a list of filters.
;;       (emf-make-filter-ring '("Tango" "Vals" "Milonga"))


;; Backward compatibility:
;; -------------------------------------------------------------------
;; This code replaces both emms-browser filters and search-by.
;; emms-browser-make-filter and search-by use emms-filters for their
;; current functionality.
;;
;; Emms-browser-filter functions are specified to return an inverted value.
;; emms-browser-make-filter is a slightly different mechanism from emms-filters.el.
;; but has been modified to pass its filters to emms-filters.
;; Those filters will be properly inverted and added to emf-filters and to the
;; emf-filter-ring. This should provide a seamless experience for previous users
;; of emms-browser filtering. As the emf-filter-ring is functionally equivalent.
;;
;; Search-by was just one filter factory, 'fields-search', and searches are
;; not inverted. The only real difference between a filter and a search was
;; that a filter was rendered and a search was saved for subsequent filtering.
;; The equivalent to the emms-browser search-by is just a one shot
;; interactive new fields-search factory filter that saves a cache.
;;
;; Filters are slightly different when coded for emms-filters.
;; 1. They should return true if they match the tracks
;; 2. The factory should wrap the lambda in a lexical-let.
;; 3. The factory and the filters must both be registered with emms-filters.
;;    This provides a higher level of interaction with the filters.
;; 4. There is no difference between a search function and a filter function.


;; The moving parts.
;; -------------------------------------------------------------------
;; Emms-filters consists of a few different mechanisms.
;; There are factories to make filters. There is the filter stack
;; to manage the creation and use of filters.
;;
;; There is the cache stack to handle the saving of a current filtered results
;; into a reduced database cache for subsequent searches.
;;
;; There is the filter ring for quickly switching between commonly used filters.
;;
;; - Filter Factories - To make filters.
;; - Filters - To be used by the meta-filter stack to create more filters.
;; - Filter menu - A customizable select list of filters organized by their factories.
;; - Multi-filter - A filter factory to create complex filters by combining them.
;; - Meta-filter - A multi-filter data definition. A list of lists of filter names.
;; - The filter stack - A meta-filter manipulator and multi-filter creator.
;; - The cache stack - A stack of database caches.
;; - The filter ring. - A subset of convenient to use filters.


;;; Filter factories
;; -------------------------------------------------------------------
;; Filter factories make filters which are simply test functions which
;; take a track and return true or false.
;;
;; Here is the Genre Factory which is actually made from the
;; field-compare factory. This is a common pattern to create
;; a simpler factory from a more complex one.
;;
;; (emf-register-filter-factory
;;  "Genre"
;;  (apply-partially 'emf-make-filter-field-compare
;;                   'string-equal-ignore-case 'info-genre)
;;  '(("Genre: " (:string . nil))));;
;;
;; The actual filter factory is the field comparison factory.
;; This single function can be a new factory for any data field
;; using any comparison function we would like.
;;
;; Filter factories depend upon lexical context of their parameters. In
;; order to have data values that stick after function creation there
;; is lexical-let to ensure the factory behaves as expected.
;; Transfer the values to local values and use them as normal
;; in the returned #'(lambda (track)...).
;;
;; (defun emf-make-filter-field-compare (operator-func field compare-val)
;;   "Make a filter that compares FIELD to COMPARE-VALUE with OPERATOR-FUNC.
;; Works for number fields and string fields provided the appropriate
;; type match between values and the comparison function. Partials can
;; easily make more specific factory functions from this one."
;;   (lexical-let ((local-operator operator-func)
;;                 (local-field field)
;;                 (local-compare-val compare-val))
;;     #'(lambda (track)
;;         (let ((track-val (emms-track-get track local-field)))
;;           (and
;;            track-val
;;            (funcall local-operator local-compare-val track-val))))))

;; The registration for this factory is more complex because of the prompting
;; for all the parameters. By changing just the registration name and the
;; prompts we can create two factories, one for numbers and one for strings.
;; Note the use of the ` and , to force the select lists to resolve.
;;
;; (emf-register-filter-factory "Number field compare"
;;                              'emf-make-filter-field-compare
;;                              ;; prompts
;;                              `(("Compare Function: "
;;                                 (:function . ,emf-number-compare-functions))
;;                                ("Field name: "
;;                                 (:symbol . ,emf-number-field-names))
;;                                ("Compare to: "
;;                                 (:number . nil))))
;;
;;
;; Making a filter from a factory is easy.
;;
;; (emf-make-filter "Genre" "My Genre filter" "Somevalue")
;;
;; Or make a lot of filters at once.
;;
;; (emf-make-filters '(("Genre" "Waltz"      "waltz")
;;                     ("Genre" "Salsa"      "salsa")
;;                     ("Genre" "Blues"      "blues")
;;                     ("Genre" "Jazz"       "jazz")))
;;
;; Or just push a filter onto the stack with emf-select-push,
;; select 'new filter' and follow the prompts.

;;; Factory Prompts.
;;; Interactive factory prompting for filter building.
;; -------------------------------------------------------------------
;; Registering a factory associates a name, a function and a list of
;; prompt definitions so that we may create filters interactively by name.
;;
;; The factory prompt data is used to interactively create new filters.
;; A prompt is  (prompt (type . select-list)) if there is no
;; select list we read a string and coerce the value to the correct
;; type as needed.  :number, :string, :list :symbol :function
;; are the coercion type choices.
;;
;; Here is a simple factory registration for the Genre filter factory function.
;; Which takes a single string parameter.
;;
;; (emf-register-filter-factory "Genre"
;;                              emf-make-filter-genre
;;                              '(("Genre: " (:string . nil))))
;;
;; Parameters are of the form: '((prompt (type . select-list)) ... )
;;
;; This prompt will coerce the value it receives into a number.
;;
;; '(("Days: " (:number . nil)))
;;
;; The compare field factory takes a compare function,
;; an :info-field specifier and a string to compare.
;; Note the use of ` and , in order to resolve the selection lists here.
;;
;; `(("Compare Function:"
;;    (:function . ,emf-string-compare-functions))
;;   ("Field name:"
;;    (:symbol . ,emf-string-field-names))
;;   ("Compare to:"
;;    (:string . nil)));;
;;

;; The Filter stack
;; -------------------------------------------------------------------
;; The filter stack keeps track of the filters you use as you create them.
;; Adding to the filter or replacing it with another push continues to add filters
;; to the filter stack. To return to the previous filter simply pop the stack.
;;
;; To use a filter, emf-push it to create a new current filter.
;; It will become a meta-filter on the filter stack
;; and the current active filter will be a multi-filter version of it.
;;
;; The filter ring works independently of the filter stack. Each re-filtering of
;; tracks uses the current ring filter and the current filter together.
;;
;; A filter can be 'kept'. The function 'emf-keep will create and register
;; a multi-filter of the current filter, adding it to the multi-filter menu.
;; This only lasts until the current Emacs session ends.
;; If emf-multi-filter-save-file is set, a usable filter definition will be
;; appended to the file.
;;
;; Other commands for manipulating the stack.
;;    Push, pop, squash, clear, swap, swap-pop.


;;; The Search Cache Stack
;; --------------------------------------------------
;; The cache stack is a simply a stack of emms-cache-db style hash tables.
;; Each entry is a subset of the master emms-cache-db created through filtering.
;; Their names are constructed from the filters which created them.
;;
;; Filtering and displaying of tracks is done against the top cache on the stack.
;;
;; The function; emf-hard-filter creates a cache from the current filter
;; and cache, and pushes it to the stack.
;;
;; Using emf-select-one-shot, emf-quick-one-shot and emf-browser-search
;; also create caches on the stack.
;;
;; The usual commands exist for manipulating the stack.
;;    Pop, squash, clear, swap, swap-pop, push-db
;;
;;    Push and stash work with the cache stash. The stash always has
;; the emms-db-cache. The current cache on the stack can be stashed
;; at anytime. The cache will become a selection for push-cache.
;;
;; A one-shot filter combined with a factory name is 'emf-quick-one-shot.
;;
;; The function 'emf-browser-search is 'emf-quick-one-shot with the
;; fields-search filter factory.
;;
;; This effectively emulates the former emms-browser search behavior of
;; quickly prompting, filtering and saving a cache by pushing a filter,
;; hard-filter then pop.


;; How it works.
;; -------------------------------------------------------------------
;; It starts with filter factory functions.
;; Factories create filters, either directly or interactively.
;; There are a lot of common, pre-canned, filters which can be used
;; to create more complex filters.

;; There are numerous factory choices and most common searches can
;; be easily made with the factories and filters which already exist.
;; Many factories are partials of other factories.

;; Filters which are created interactively can be kept for the session
;; with emf-keep.  One shots, (searches), are automatically kept for the session.
;; Keep may also write them to a file for later use.
;;
;; Push a filter to the filter stack with emf-select-push and then
;; add to it with the other emf-select-* functions. Each change results
;; in new filter on the stack.
;; Add another filter pop, try again.
;;
;; Use emf-select-or to add another filter and choose 'new filter' to
;; interactively create and add a filter to the current filter.
;;
;; Add in an extra layer of quick switch filtering with next and previous
;; filter-ring filters.  The filter ring filters can be accessed with
;; < and >.
;;
;; You may want to keep your results for a while, or you may
;; wish to start with a clear search for a name, either way,
;; a hard-filter will push a cache-db onto the cache stack.
;;
;; Subsequent filtering continues with this new DB cache. A cache can also
;; be pushed to the stack with a one-shot function. One shots
;; make, use, cache, and pop a filter, leaving a new cache and the filter
;; stack as it was.
;;
;; Create a new factory function, register it in
;; emf-factories along with its parameters and prompts.
;; From this point on filters can be created interactively by selecting
;; to push a new filter, and choosing the new factory.
;;
;; In code use emf-make-filter or emf-make-filters to use the factory by name.

;; Filter stack interaction
;; -------------------------------------------------------------------
;; To interactively create a filter, start with a push.
;; The filter stack itself is the interactive filter factory for multi-filters.
;;
;; Choose an existing filter or 'new filter'.  Follow the prompts.
;;
;; Do an emf-select-or to add another possible match or emf-select-and or
;; select-and-not to add another restriction. Build the filter how you like.
;;
;; When a new filter is pushed, it turns into a meta filter
;; and is pushed on the filter stack. A filter function is made from
;; the meta-filter and set it to the current filter, and the browser is
;; asked to re-render the results.
;;
;; Later, if the filter is added to, it all happens again.
;; Popping a filter pops it from the stack and re-renders with the last filter.
;;
;; Use emf-status or the emms-filter-hydra to see the stacks and
;; current filters.

;; Making Filters from factories, in code.
;; -------------------------------------------------------------------
;; Filter factories include the following. Most common filters can be
;; easily constructed from these. The number of available filters is too
;; numerous to list. For instance, a filter already exists for every
;; track type and there many common genres and year range filters.
;;
;; Filter factories like artist, album artist, composer, Names, etc.
;; are all just specialized field compare or fields search factories.
;;
;; Factories
;; ----------
;; "Album
;; Album-artist
;; All text fields
;; Artist
;; Artists
;; Artists and composer
;; Composer
;; Directory
;; Duration less
;; Duration more
;; Fields search
;; Genre
;; Greater than Year
;; Less than Year
;; Multi-filter
;; Names
;; Names and titles
;; Not played since
;; Notes
;; Number field compare
;; Orchestra
;; Performer
;; Played since
;; String field compare
;; Title
;; Titles
;; Track type
;; Year range"

;; Filters also have names, and are added
;; to their respective factory's filter selection menu.
;; here are some example filter definitions.
;;
;; ;; Filters are easily described as data.
;; ;;        factory      Name        arguments
;;
;; (setq tango-filters
;;       '(("Year range" "1900-1929" 1900 1929)
;;         ("Year range" "1929-1937" 1929 1937)
;;         ("Directory" "tangotunes" "tangotunesflac")
;;
;;         ("Genre" "Vals"    "vals")
;;         ("Genre" "Tango"   "tango")
;;         ("Genre" "Milonga" "milonga")
;;
;;         ("Multi-filter"
;;          "1900-1937"
;;          (("1900-1929" "1929-1937")))
;;
;;         ("Multi-filter"
;;          "Vals | milonga"
;;          (("Vals" "Milonga")))
;;
;;         ("Multi-filter"
;;          "Vals 1900-1929"
;;          (("Vals") ("1900-1929")))
;;
;;         ("Multi-filter"
;;          "Not vals"
;;          ((:not "Vals")))
;;
;;         ("Multi-filter"
;;          "Vals or milonga 1900-1937"
;;          (("Vals" "Milonga")
;;           ("1900-1929" "1929-1937")))
;;         ))
;;
;; (emf-make-filters tango-filters)
;;
;; ;; Add my own filter selection menu with tango filters in it.
;; (emf-add-filter-menu-from-filter-list "Tango" tango-filters)
;;
;; The easiest way to make a filter ring.
;; (emf-make-filter-ring '("Tango" "Vals" "Milonga"))

;;; Code:
(require 'cl)  ; for lexical-let
(require 'emms-browser)

(defvar  emf-replace-browser-filters t
  "Take over select, previous and next filter responsibilities.")

(defvar emf-browse-by-level-type emms-browser-default-browse-type
  "Browse-by setting to use with emms-browse-by when rendering tracks.")

(defvar  emf-stack nil
  "A history of multi-filters. Our working stack.")

(defvar  emf-filter-ring nil
  "A ring of filter names for quick access with next and previous.")

(defconst emf-no-filter nil ;; '("no filter" . nil)
  "A filter that turns filtering off, a better initial value than nil.")

(defvar emf-current-ring-filter  emf-no-filter
  "The current ring filter, a filter cons, (name . func).")

(defvar emf-filter-factories '()
  "An alist of filter factory functions and their argument lists.")

(defvar emf-filters '(("no filter" . nil))
  "A list of available filters.")

(defvar emf-automatic-filter-names t
  "Automatically generate filter names when creating filters interactively.")

(defvar emf-current-filter emf-no-filter
  "The current filter function")

(defvar emf-current-filter-name "no filter"
  "A name string of the filter for everyone to use.")

(defvar emf-filter-menu '("no filter" "new filter")
  "A list of available filters grouped by factory.")

;; for backwards compatibility with emms-browser
(defvar emf-filter-changed-hook emms-browser-filter-changed-hook
  "Hook to run after the filter has changed.")

(defvar emf-multi-filter-save-file nil
  "A file name to write the kept meta-filters from the session to.")

(defvar emf-cache-stash '(("Emms DB" . emms-cache-db))
  "A list of cons (name . cache).")

(defun emf-browser-filter-hook (track)
  "A hook function for the browser. Freewill here for TRACK filtering.
First we test the track against the current ring filter if we have one,
then we combine with the result of the emf-current-filter."
  (and (if (cdr emf-current-ring-filter)
           (funcall (cdr emf-current-ring-filter) track)
         t)
       (if (cdr emf-current-filter)
           (funcall (cdr emf-current-filter) track)
         t)))

;; We set this and forget it.
;; This and 'emf-refilter() are the interfaces to emms-browser.
(setq emms-browser-filter-tracks-hook 'emf-browser-filter-hook)

(defun emf-register-filter (filter-name filter)
  "Put our new FILTER function named FILTER-NAME in our filter list."
  (push (cons filter-name filter) emf-filters))

(defun emf-register-if-missing (filter)
  "Register a cons FILTER if it isn't already in the emf-filters list."
  (when (not (assoc (car filter) emf-filters))
    (push filter emf-filters )))

;; (defun emf-add-filter-menu-item (folder-name name-list)
;;   "Add a list of NAME-LIST, a list of strings,
;; as another FOLDER-NAME in the filter selection menu."
;;   (setq emf-filter-menu
;;         (cons (list folder-name name-list)
;;               emf-filter-menu)))

(defun emf-add-to-filter-menu-from-filter-list (folder filters)
  "Add a FOLDER and FILTERS to the filter select list menu. "
  (emf-add-to-filter-menu folder (mapcar 'cadr tango-filters)))

(defun emf-add-to-filter-menu (folder-name filter-or-list)
  "Add to a FOLDER-NAME in the filter select menu creating it as needed.
Adds filter name(s) given in FILTER-OR-LIST to the FOLDER-NAME
of the filter select menu tree."
  (if (listp filter-or-list)
      (mapcar (lambda (filter)
                (emf-add-name-to-filter-menu folder-name filter))
              filter-or-list)
    (emf-add-name-to-filter-menu folder-name filter-or-list)))

(defun emf-add-name-to-filter-menu (folder-name filter-name)
  "Add FILTER-NAME to menu tree of FOLDER-NAME."
  (if (assoc folder-name emf-filter-menu)
      (push filter-name (cadr (assoc folder-name emf-filter-menu)))
    (setq emf-filter-menu
          (cons (list folder-name (list filter-name))
                emf-filter-menu))))

(defun emf-show-filter-menu ()
  "Show the menu tree of filters."
  (interactive)
  (message "%s"
           (mapconcat
            (lambda (menu)
              (if (consp menu)
                  (format "%s : \n%s\n"
                          (car menu)
                          (mapconcat 'identity
                                     (cadr menu) ", "))
                menu))
            emf-filter-menu "\n")))

(defun emf-make-filter-ring (list-of-filter-names)
  "Make a ring of filter names from the LIST-OF-FILTER-NAMES.
Appends the 'no filter' filter."
  (setq emf-filter-ring
        (make-ring
         (+ 1 (length list-of-filter-names))))
  (mapcar (lambda (filter-name)
            (ring-insert emf-filter-ring filter-name))
          (cons "no filter" list-of-filter-names)))

(defun emf-append-to-filter-ring (filter-name)
  "Append a single FILTER-NAME to the filter-ring,
This creates the filter ring as needed."
  (if emf-filter-ring
      (ring-insert+extend emf-filter-ring
                          filter-name t)
    (emf-make-filter-ring (list filter-name))))

;; This should allow people to continue using the emms-browser
;; filtering as they always have, reusing the filters they've already made.
(defun emf-register-filter-into-ring (filter)
  "Integrate Emms browser filters into emf-filters.
Register a FILTER to emf-filters if it's name is missing.
Add its name to the filter ring and filter menu in
the 'browser-filters' selection menu."
  (emf-register-if-missing filter)
  (let ((name (car filter)))
    (emf-append-to-filter-ring name)
    (emf-add-to-filter-menu "browser-filters" name)))

(defun emf-list-filters ()
  "List the filters in our filter list."
  (mapcar 'car emf-filters))

(defun emf-show-filters ()
  "Show the filters we have."
  (interactive)
  (when emf-filters
    (message "Emf Filters:\n%s"
             (mapconcat 'identity (emf-list-filters) "\n"))))

(defun emf-show-filter-ring ()
  "Show the filters in the filter ring."
  (interactive)
  (message "Ring filters: %s" (ring-elements emf-filter-ring)))

(defun emf-find-filter (name)
  "A nicer way to find NAME in our list of filters."
  (assoc name emf-filters))

(defun emf-find-filter-function (filter-name)
  "Find the Function for FILTER-NAME in emf-filters.
  Pass functions through untouched."
  (if (eq filter-name :not)
      :not
    (cdr (assoc filter-name emf-filters))))

(defun emf-format-search (fields value)
  "Create a string format from a list of FIELDS and a compare VALUE."
  (format "%s : %s"
          (mapconcat
           `(lambda (info)
              (if (symbolp info)
                  (substring (symbol-name info)  5)
                info))
           fields " | ")
          value))

;; The Filter Factory of factories.
;; making them, using them, keeping them organized.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun emf-make--filter (factory factory-args)
  "Make a filter using the FACTORY and FACTORY-ARGS.
If factory is a function it is used directly. Otherwise, it will
look for the function in emf-filter-factories."
  (let ((factory-func (if (functionp factory)
                          factory
                        (cadr (assoc factory emf-filter-factories)))))
    (apply factory-func factory-args)))

(defun emf-make-filter (factory filter-name factory-args)
  "Make a filter named FILTER-NAME using the FACTORY and FACTORY-ARGS.
  if factory is a function it is used directly. Otherwise, it will
  look for the function in emf-filter-factories."
  (emf-add-to-filter-menu factory filter-name)
  (emf-register-filter
   filter-name
   (emf-make--filter factory factory-args)))

(defun emf-make-filters (filter-list)
  "Make filters in FILTER-LIST into filter functions.
The filter list holds entries specified as
  '(factory-name filter-name factory-arguments)."
  (mapcar (lambda (filter)
            (emf-make-filter
             (car filter)
             (cadr filter) (cddr filter)))
          filter-list))

(defun emf-new-filter (&optional factory-name make-filter-name)
  "Build a new filter from a filter factory interactively.
  Use FACTORY-NAME instead of prompting if given.
  If MAKE-FILTER-NAME or EMF-AUTOMATIC-FILTER-NAMES is true the name will
  be constructed instead of prompted.

  Normally prompts for a filter factory and its parameters, prompts for a
  filter name and then creates and registers a new filter,then returns its name."
  (interactive)
  (let* ((factory-name (if factory-name factory-name
                         (emf-choose-factory)))
         (make-name (or make-filter-name emf-automatic-filter-names))
         (parameters (emf-get-factory-parameters factory-name))
         (filter-name (if make-name
                          (format "%s : %s" factory-name parameters)
                        (read-string "filter name:"))))

    (message "%s | %s parms %s" factory-name filter-name parameters)

    (emf-make-filter factory-name filter-name parameters)
    filter-name))

(defun emf-fields-search-quick-one-shot (fields compare-value)
  "Make and register a fields-search filter, searching FIELDS for COMPARE-VALUE.
Push the filter onto the filter stack.
Push matching tracks to the cache stack, then pop the filter."
  (let ((filter-name (emf-format-search fields compare-value)))
    (emf-register-filter
     filter-name
     (emf-make-filter-fields-search (fields compare-value)))
    (emf-add-to-filter-menu "fields-search" filter-name)
    (emf-one-shot filter-name)))

(defun emf-register-filter-factory (name func prompt-list)
  "Register FUNC as NAME with PROMPT-LIST into a filter choice.
Give it the shape: (name . (func . prompt-list))."
  (push
   (cons name (cons func prompt-list))
   emf-filter-factories))

(defun emf-list-filter-factories ()
  "List the filters in our factories list."
  (mapcar 'car emf-filter-factories))

(defun emf-show-filter-factories ()
  "Show the filter factories we have."
  (interactive)
  (when emf-filter-factories
    (message "Emf Filter Factories:\n%s"
             (mapconcat 'identity "\n" emf-filter-factories))))

(defun emf-clear-filter-factories ()
  "Reset the filter factory list."
  (setq emf-filter-factories nil))



;;; Factory Prompting.
;;
;; This function is a bit brittle for my taste.
;; It needs more use cases.
;; It might be the only one, or we only ever have lists of symbols...
;; This is used for the fields-search factory.
(defun emf-string-field-list-prompt (prompt)
  "Recursively PROMPT for elements of a list.
  Prompt must define a select list. The only usage example so
  far is the field-search list which is all symbols.
  (info-artist, info-genre, ...).  intern-soft works for those."
  (let* ((prompt-string (car prompt))
         (selections (cdar (cdr prompt)))
         (value
          (completing-read prompt-string
                           (cons "quit" selections) nil t)))
    (if (string= value "quit")
        nil
      (cons (intern-soft value)
            (emf-string-field-list-prompt
             (cons (concat (car prompt) " " value)
                   (cdr prompt)))))))

(defun emf-coerce-prompt-value (prompt value)
  "Coerce VALUE, a string, according to the prompt type inside PROMPT.
  PROMPT should be in the form (prompt (type . <select-list>)).
  Types are :number, :symbol, :string and :function.
  Strings pass through."
  (let ((type (car (cadr prompt))))
    (cond
     ((string= type :number) (string-to-number value))
     ((string= type :symbol) (intern-soft value))
     ((string= type :function) (intern-soft value))
     (t value))))

(defun emf-read-string-or-choose (prompt)
  "Choose the method input using PROMPT.
  Do a string read or completing read if PROMPT has a select-list.
  Do a recursive completing read with selection-list if a :list type.
  A prompt should look like this; (prompt (type . <select-list>))."
  (let* ((prompt-string (car prompt))
         (selections (cdr (cadr prompt)))
         (_ (message "Selections %s" selections))
         (type (car (cadr prompt)))
         (value (cond ((string= type :list) (emf-string-field-list-prompt prompt))
                      (selections
                       (completing-read prompt-string selections nil t))
                      (t (read-string prompt-string)))))
    (emf-coerce-prompt-value prompt value)))

(defun emf-get-factory-parameters (factory-name)
  "Prompt for the parameters needed by a factory identified by FACTORY-NAME.
Coerce their types as indicated and return the list of parameters.

A prompt should be of the form (prompt (type . <list>)) where prompt is a string
and type is :number :function :symbol or :string"
  (interactive)
  (let ((prompts (cddr (assoc factory-name emf-filter-factories))))
    (mapcar (lambda (prompt)
              (emf-read-string-or-choose prompt))
            prompts)))


;;; Factory Functions to make filter functions with.
;; A filter factory is a function that returns a function which
;; returns true if it likes the values from the track it was given.
;;
;; Registering them makes them interactive and invokable
;; by name.

(defun emf-make-filter-directory (dirname)
  "Generate a function to check if a track is in DIRNAME.
If the track is not in DIRNAME, return t.
Uses a regex anchoring dirname to the beginning of the expanded path."
  (lexical-let ((re (concat "^" (expand-file-name dirname))))
    #'(lambda (track)
        (string-match re (emms-track-get track 'name)))))

(emf-register-filter-factory "Directory"
                             'emf-make-filter-directory
                             '(("Directory: " (:string . nil))))

;; seconds in a day (* 60 60 24) = 86400
(defun emf-make-filter-played-within (days)
  "Show only tracks played within the last number of DAYS."
  (lexical-let ((seconds-to-time (seconds-to-time (* days 86400))))
    #'(lambda (track)
        (let ((min-date (time-subtract
                         (current-time)
                         seconds-to-time))
              last-played)
          (and (setq last-played
                     (emms-track-get track 'last-played nil))
               (time-less-p min-date last-played))))))

(emf-register-filter-factory "Played since"
                             'emf-make-filter-played-within
                             '(("Days: " (:number . nil))))

(defun emf-make-filter-not-played-within (days)
  "Make a not played since DAYS filter."
  (lambda (track)
    (funcall (emf-filter-played-within days) track)))

(emf-register-filter-factory "Not played since"
                             'emf-make-filter-not-played-within
                             '(("Days: " (:number . nil))))

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
          (and
           year
           (<= local-y1 year)
           (>= local-y2 year))))))

(emf-register-filter-factory "Year range"
                             'emf-make-filter-year-range
                             '(("Start year:" (:number . nil))
                               ("End year:" (:number . nil))))

(defun emf-make-filter-year-greater (year)
  "Make a Greater than year filter from YEAR."
  (lexical-let ((local-year year))
    #'(lambda (track)
        (let ((year (emf-get-year track)))
          (and
           year
           (<= local-year year))))))

(emf-register-filter-factory "Greater than Year"
                             'emf-make-filter-year-greater
                             '(("Greater than year: " (:number . nil))))

(defun emf-make-filter-year-less (year)
  "Make a Less than year filter from YEAR."
  (lexical-let ((local-year year))
    #'(lambda (track)
        (let ((year (emf-get-year track)))
          (and
           year
           (>= local-year year))))))

(emf-register-filter-factory "Less than Year"
                             'emf-make-filter-year-less
                             '(("Less than year: " (:number . nil))))

;; fields-search
;; -------------
;; A replacement filter factory for the emms-browser-fields-search filter.
(defun emf-make-filter-fields-search (fields compare-value)
  "Make a filter that can look in multiple track FIELDS for COMPARE-VALUE.
This replaces the original emms-browser search match-p functionality."
  (lexical-let ((local-fields fields)
                (local-compare-value compare-value))
    #'(lambda (track)
        (cl-reduce
         (lambda (result field)
           (let ((track-value (emms-track-get track field "")))
             (or result
                 (and track-value
                      (string-match local-compare-value track-value)))))
         local-fields
         :initial-value nil))))

(defvar emf-string-field-names
  '(info-albumartist
    info-artist
    info-composer
    info-performer
    info-title
    info-album
    info-date
    info-originaldate
    info-note
    info-genre)
  "The list of track field names that are strings")

(emf-register-filter-factory
 "Fields search"
 'emf-make-filter-fields-search
 `(("Choose fields to search : "
    (:list . ,emf-string-field-names))
   ("Search: " (:string . nil))))

;; field-compare
;; -------------
(defvar emf-number-field-names
  '(info-tracknumber
    info-discnumber
    info-year
    info-originalyear
    info-originaldate
    info-playing-time)
  "The list of track field names that are numbers.")

(defvar emf-string-compare-functions
  '(emf-match-string
    string-equal-ignore-case
    string=
    string<
    string>
    string-match)
  "Compare functions for filter creation.")

(defvar emf-number-compare-functions
  '(> >= = <= <)
  "Compare functions for filter creation.")

(defvar emf-track-types
  '(file url stream streamlist playlist)
  "Types of tracks we can have.")

(defun emf-match-string (string1 string2)
  "Check to see if STRING2 is in STRING1.

This is the inverse parameter list of string-match.
So we can continue with the language of
'filter track where field contains string'
'filter track where field > value'."
  (string-match string2 string1))

(defun emf-make-filter-field-compare (operator-func field compare-val)
  "Make a filter that compares FIELD to COMPARE-VALUE with OPERATOR-FUNC.
Works for number fields and string fields provided the appropriate
type match between values and the comparison function. Partials can
easily make more specific factory functions from this one."
  (lexical-let ((local-operator operator-func)
                (local-field field)
                (local-compare-val compare-val))
    #'(lambda (track)
        (let ((track-val (emms-track-get track local-field)))
          (and
           track-val
           (funcall local-operator local-compare-val track-val))))))

;; not sure anyone will use these directly but you never know.
;; Its a good test for the prompting system.
;; Note the use of ` and , to resolve the selection lists here.
(emf-register-filter-factory "Number field compare"
                             'emf-make-filter-field-compare
                             ;; prompts
                             `(("Compare Function: "
                                (:function . ,emf-number-compare-functions))
                               ("Field name: "
                                (:symbol . ,emf-number-field-names))
                               ("Compare to: "
                                (:number . nil))))

(emf-register-filter-factory "String field compare"
                             'emf-make-filter-field-compare
                             ;; prompts
                             `(("Compare Function: "
                                (:function . ,emf-string-compare-functions ))
                               ("Field name: "
                                (:symbol . ,emf-string-field-names))
                               ("Compare to: "
                                (:string . nil))))

;; Generic field comparison factories.
;; parameter order is good for making partials.
(emf-register-filter-factory
 "Duration less"
 (apply-partially 'emf-make-filter-field-compare
                  '<= 'info-playing-time)
 '(("Duration: " (:number . nil))))

(emf-register-filter-factory
 "Duration more"
 (apply-partially 'emf-make-filter-field-compare
                  '>= 'info-playing-time)
 '(("Duration: " (:number . nil))))

(emf-register-filter-factory
 "Genre"
 (apply-partially 'emf-make-filter-field-compare
                  'string-equal-ignore-case 'info-genre)
 '(("Genre: " (:string . nil))))

(emf-register-filter-factory
 "Track type"
 (apply-partially 'emf-make-filter-field-compare
                  'eq 'type)
 '(("Track type: "
    (:string . '(file url stream streamlist playlist)))))

;; Search fields for text.  Same behavior as emms-browser-search.
;; Replace the emms browser searches with these filter factories.

(emf-register-filter-factory
 "Album-artist"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-albumartist))
 '(("Search album artist: " (:string . nil))))

(emf-register-filter-factory
 "Artist"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-artist))
 '(("Search artist: " (:string . nil))))

(emf-register-filter-factory
 "Artists"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-artist info-albumartist))
 '(("Search artists: " (:string . nil))))

(emf-register-filter-factory
 "Artists and composer"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-artist info-albumartist info-composer))
 '(("Search artists and composer: " (:string . nil))))

(emf-register-filter-factory
 "Album"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-album))
 '(("Search album: " (:string . nil))))

(emf-register-filter-factory
 "Title"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-title))
 '(("Search title: " (:string . nil))))

(emf-register-filter-factory
 "Performer"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-performer))
 '(("Search performer: " (:string . nil))))

(emf-register-filter-factory
 "Orchestra"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-orchestra))
 '(("Search orchestra: " (:string . nil))))

(emf-register-filter-factory
 "Composer"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-composer))
 '(("Search composer: " (:string . nil))))

(emf-register-filter-factory
 "Notes"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-note))
 '(("Search notes: " (:string . nil))))

(emf-register-filter-factory
 "Titles"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-title
                    info-album))
 '(("Search titles: " (:string . nil))))

(emf-register-filter-factory
 "Names"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-albumartist
                    info-name
                    info-artist
                    info-composer
                    info-performer))
 '(("Search names: " (:string . nil))))

(emf-register-filter-factory
 "Names and titles"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-albumartist
                    info-artist
                    info-composer
                    info-performer
                    info-name
                    info-title
                    info-album))
 '(("Search names and titles: " (:string . nil))))

(emf-register-filter-factory
 "All text fields"
 (apply-partially 'emf-make-filter-fields-search
                  '(info-albumartist
                    info-artist
                    info-composer
                    info-performer
                    info-title
                    info-album
                    info-name
                    info-date
                    info-originaldate
                    info-note
                    info-genre))
 '(("Search all text fields: " (:string . nil))))

;; Multi-filter  - Just another factory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A filter of filters. A list of lists of filter Names and maybe a :not.
;; Each list is Reduced with Or then reduced together with And and Not.
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
         (funcall filter-func track)))
   or-group
   :initial-value nil))

(defun emf-reduce-invert-or-group (or-group track)
  "Call an OR-GROUP list of filters with TRACK and reduce result with OR.
If the first item is :not then invert the result from the reduction."
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
        (cl-reduce
         (lambda (result funclist)
           (and result
                (emf-reduce-invert-or-group funclist track)))
         local-multi-funcs
         :initial-value t))))

(emf-register-filter-factory "Multi-filter"
                             'emf-make-multi-filter
                             '(nil))

;;; Some filters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A simple not a filter, So we have a default of no filters to choose/return to.
(emf-register-filter "No filter" nil)

;;             factory      name        factory arg
(setq emf-decade-filters
      '(("Year range" "1900s"     1900 1909)
        ("Year range" "1910s"     1910 1919)
        ("Year range" "1920s"     1920 1929)
        ("Year range" "1930s"     1930 1939)
        ("Year range" "1940s"     1940 1949)
        ("Year range" "1950s"     1950 1959)
        ("Year range" "1960s"     1960 1969)
        ("Year range" "1970s"     1970 1979)
        ("Year range" "1980s"     1980 1989)
        ("Year range" "1990s"     1990 1999)
        ("Year range" "2000s"     2000 2009)
        ("Year range" "2010s"     2010 2019)
        ("Year range" "2020s"     2020 2029)))

(setq emf-genre-filters
      '(("Genre" "Waltz"      "waltz")
        ("Genre" "Vals"       "vals")
        ("Genre" "Tango"      "tango")
        ("Genre" "Milonga"    "milonga")
        ("Genre" "Condombe"   "condombe")
        ("Genre" "Salsa"      "salsa")
        ("Genre" "Blues"      "blues")
        ("Genre" "Rock"       "rock")
        ("Genre" "Swing"      "swing")
        ("Genre" "Pop"        "pop")
        ("Genre" "Rap"        "rap")
        ("Genre" "Hip hop"    "hip hop")
        ("Genre" "Classical"  "classical")
        ("Genre" "Baroque"    "baroque")
        ("Genre" "Chamber"    "chamber")
        ("Genre" "Reggae"     "reggae")
        ("Genre" "Folk"       "folk")
        ("Genre" "World"      "world")
        ("Genre" "Metal"      "metal")
        ("Genre" "Fusion"     "fusion")
        ("Genre" "Jazz"       "jazz")))

(setq emf-last-played-filters
      '(("Played since" "Played in the last month" 30)
        ("Not played since" "Not played since a year" 365)))

(setq emf-track-type-filters
      '(("Track type" "File" file)
        ("Track type" "Url" url)
        ("Track type" "Stream" stream)
        ("Track type" "Stream list" streamlist)
        ("Track type" "Play list" playlist)))

(setq emf-duration-filters
      '(("Duration less" "Duration <1 min"  60)
        ("Duration less" "Duration <5 min"  300)
        ("Duration more" "Duration >5 min"  300)
        ("Duration more" "Duration >10 min" 600)))

(defun emf-make-default-filters()
  "Make some default filters anyone would not mind having."
  (emf-make-filters emf-decade-filters)
  (emf-make-filters emf-genre-filters)
  (emf-make-filters emf-track-type-filters)
  (emf-make-filters emf-last-played-filters)
  (emf-make-filters emf-duration-filters))

;; Install some default filters.
(emf-make-default-filters)

;; The Meta-Filter stack
;; An interactive multi-filter stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The current filter is the multi-filter version of the meta-filter
;; at the top of the filter stack.
;;
;; Adding more filters to the current filter pushes a new filter to the stack.
;; emf-pop pops the stack, returning to the last filter.
;;
;; Other filters can be added to the current filter
;; with 'and', 'or' as well as 'and-not' and 'smash' filter selections.

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
  "Make a filter cons from meta-filter FILTER."
  (cons (emf-make-name filter) filter))

(defun emf-set-filter (filter)
  "Set the current filter to FILTER.
Filter should be a filter cons in the form of '(name . function)."
  (setq emf-current-filter-name (car filter))
  (setq emf-current-filter filter))

(defun emf-browse-by ()
  "The single interface to emms-browser. Re-render please."
  (emms-browse-by (or emms-browser-top-level-type
                      emms-browser-default-browse-type)))

(defun  emf-refilter ()
  "Make a multi-filter function from the current meta-filter and set it.
Run the filter changed hook. Ask the browser to re-render."
  (emf-set-filter (cons (caar emf-stack)
                        (emf-make-multi-filter (cdar emf-stack))))
  (run-hooks 'emf-filter-changed-hook)
  (emf-browse-by)
  (emms-browser-expand-all))

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

(defun emf-clear-all ()
  "Reset the cache stack, the filter stack and the filter-ring."
  (interactive)
  (emf-clear)
  (emf-clear-caches)
  (emf-set-ring-filter "no filter"))

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

(defun emf-append-string-to-file (string filename)
  "Appends STRING to FILENAME."
  (interactive)
  (append-to-file string nil filename))

(defun emf-save-meta-filter (meta-filter)
  (when emf-multi-filter-save-file
    (append-to-file
     (format "(\"Multi-filter\"\n %S\n %S)\n\n"
             (car meta-filter)
             (cdr meta-filter))
     nil emf-multi-filter-save-file)))

(defun  emf-keep ()
  "Register the current filter into the list of filters for the session."
  (interactive)
  (message "Registering the current meta-filter as a filter for the session")
  (emf-status)

  (when (and emf-stack (consp (car emf-stack)))
    (emf-save-meta-filter (car emf-stack))
    (emf-register-filter (caar emf-stack)
                         (emf-make-multi-filter (cdar emf-stack)))
    (emf-add-to-filter-menu "Kept filters" (caar emf-stack))))

(defun emf-hard-filter ()
  "A hard save of filtered results.
Build a cache of filtered tracks from the current cache
filtered by the current filters.

Emulates a search, pushing a new cache on the cache stack.
This cache is the same as all the rest and emms-cache-db.

See also: ems-pop-cache."
  (interactive)
  (let* ((search-name (emf-full-name))

         (search-cache (make-hash-table
                        :test (if (fboundp 'define-hash-table-test)
                                  'string-hash
                                'equal))))
    (maphash (lambda (path track)
               (when (emf-browser-filter-hook track)
                 (puthash path track search-cache)))
             (emf-last-search-cache))

    (emf-push-cache search-name search-cache))
  (emf-refilter))

(defun emf-choose-filter-recursive (&optional choices)
  "Choose a filter from emf-filter-menu tree or the alist given as CHOICES.
Requires that the lists of filter names be lists of cons (name . name).
Allows for tree structures of any depth."
  (let* ((choices (or choices emf-filter-menu))
         (choice (assoc (completing-read
                         "Choose a filter or group:" choices nil t)
                        choices)))
    (if (consp choice)
        (emf-choose-filter-recursive (cadr choice))
      (if (string= "new filter" choice)
          (emf-new-filter))
      choice)))

(defun emf-choose-filter ()
  "Choose a filter from our filter menu tree.
Stupid, Assumes our tree is an alist of lists of strings."
  (let* ((choice (completing-read
                  "Choose a filter group:" emf-filter-menu nil t))
         (newlist (cadr (assoc choice emf-filter-menu))))
    (if newlist
        (completing-read "Choose a filter:" newlist nil t)
      (if (string= "new filter" choice)
          (emf-new-filter)
        choice))))

(defun emf-choose-factory ()
  "Choose a filter factory from our list of factories."
  (completing-read
   "Choose a filter factory:"
   (mapcar (lambda (factory)
             (when (car (cddr factory))
               factory))
           emf-filter-factories)
   nil t))

(defun emf-one-shot (filter-name)
  "Push FILTER-NAME given onto the filter stack,
hard filter to create a cache, Then pop the filter."
  (emf-push filter-name)
  (emf-hard-filter)
  (emf-pop))

(defun emf-select-one-shot ()
  "Select or create a filter from the list of filter functions.
The filter will be used to create a new entry on the
cache stack and will be added to the filter menu.

Steps are;
 1. Create or choose a filter,
 2. Push filter,
 3. Push cache with filter,
 4. Pop filter.
If a filter was created it will remain as a filter choice for the session.
This is like browser-search, but with more choices."
  (interactive)
  (emf-one-shot (emf-choose-filter)))

(defun emf-quick-one-shot (factory-name)
  "Create a new filter from FACTORY-NAME, using a generated filter name.
Push the filter, push the resulting cache, then pop.
Leaving a new cache on the search stack. And the filter stack as it was.
The filter will rest under the factory name filter menu for the session.
This imitates the emms browser search."
  (interactive)
  (emf-one-shot (emf-new-filter factory-name t)))

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
Creates a new 'AND-NOT' list of filters."
  (interactive)
  (let ((fname (emf-choose-filter)))
    (emf-push
     (emf-make-filter-cons-from-meta-filter
      (emf-push-or fname
                   (emf-push-and ':not (emf-copy-meta-filter (cdar emf-stack))))))))

(defun emf-format-stack()
  "Print the stack."
  (format  "\t%s" (mapconcat 'car emf-stack "\n\t")))

(defun emf-full-name ()
  "Give a full name for the current filtering.
Used by emms-browser mode description.
Includes the ring filter name plus current filter name.
Does not show the current cache name.
Only show the ring filter name if its function is not nil.
Use the current filter name so that 'no filter' shows."
  (let ((ring (when (cdr emf-current-ring-filter)
                (car emf-current-ring-filter)))
        (current (car emf-current-filter)))
    (cond ((and ring current)
           (format "%s : %s" ring current))
          (ring ring)
          (current current)
          (t nil))))

(defun emf-status ()
  "Format what we know into something readable."
  (interactive)
  (format "Ring: %s\nCurrent: %s\nFilter Stack:\n%s\nCache stack:\n %s"
          (car emf-current-ring-filter)
          (emf-current-meta-filter)
          (emf-format-stack)
          (emf-format-cache-stack)))

(defun emf-status-print ()
  "Print what we know."
  (interactive)
  (message (emf-status)))

(defun emf-set-ring-filter (filter-name)
  "Given a FILTER-NAME set the current ring filter and re-render."
  (setq emf-current-ring-filter
        (assoc filter-name emf-filters))
  (emf-refilter))

(defun emf-clear-ring-filter ()
  "Set the ring filter to no filter."
  (interactive)
  (emf-set-ring-filter "no filter"))

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
              (if emf-current-ring-filter
                  (car emf-current-ring-filter)
                (ring-ref emf-filter-ring 0)))))

(defun emf-previous-ring-filter()
  "Move to the previous filter in the filter ring."
  (interactive)
  (emf-set-ring-filter
   (ring-previous emf-filter-ring
                  (if emf-current-ring-filter
                      (car emf-current-ring-filter)
                    (ring-ref emf-filter-ring 0)))))

;; --------------------------------------------------
;; Searching
;; --------------------------------------------------
;;; The Search Cache Stack
;;
;; The search cache stack is a simply a stack of emms-cache-db style hash tables.
;; Each entry is a subset of the master emms-cache-db created through filtering.
;; Their names are constructed from the filters which created them.
;;
;; Filtering and displaying of tracks is done against the top cache on the stack.
;;
;; A cache of the current filter results can be pushed to the cache stack at any
;; time with hard-filter. These results will reflect the current meta-filter
;; as well as the filter currently chosen in the filter ring.
;;
;; A one-shot filter combined with a hard filter is emf-quick-one-shot.
;; This effectively emulates the former emms-browser search behavior of
;; filtering and saving a cache by pushing a filter, hard-filter, pop.

(defvar emf-search-caches '()
  "The stack of search result caches.")

(defun emf-browser-search (fields)
  "Search track FIELDS in the cache for a compare string.
An exact replacement for emms-browser-search.
Prompt for the value to search, emulate
the behavior of emms-browser-search using the filter and cache stacks
with the 'fields search' filter factory.

 1. Make a filter function,
 2. Push the filter function to the filter stack,
 3. Hard filter the results to the cache stack,
 4. Pop the filter.
Leaving the search cache on the stack, the filter stack how it was,
and a new filter in the 'Fields search' factory choices menu."
  (let* ((prompt (format "Searching with %S: " fields))
         (compare-value (read-string prompt))
         (filter-name (emf-format-search fields compare-value)))

    (emf-register-filter
     filter-name
     (emf-make-filter-fields-search (fields compare-value)))
    (emf-add-to-filter-menu "Fields search" filter-name)))

(defun emf-push-cache (&optional filter-name cache)
  "Cache/Store FILTER-NAME and CACHE in a stack.
If FILTER-NAME and CACHE are not present, interactively,
allow selection of a cache from the cache stash."
  (interactive)
  (if (and filter-name cache)
      (push (cons filter-name cache) emf-search-caches)
    (let ((stashed-cache
           (assoc (completing-read "Select Cache"
                                   emf-cache-stash nil t)
                  emf-cache-stash)))
      (push stashed-cache emf-search-caches)))
  (emf-refilter))

(defun emf-stash-cache ()
  "Stash the current-cache for later."
  (interactive)
  (push (car emf-search-caches) emf-cache-stash))

(defun emf-stash-pop-cache ()
  "Stash the current-cache for later, pop it from the stack."
  (interactive)
  (emf-stash-cache)
  (emf-pop-cache))

(defun emf-get-search-keys ()
  "Return the search-list keys for the current search cache."
  (if (< 0 (length emf-search-caches))
      (reverse (mapcar #'car emf-search-caches))
    '()))

(defun emf-current-cache-name ()
  "Return the name of the current search cache."
  (car (emf-get-search-keys)))

(defun emf-format-search-list (search-list)
  "Create a string format of a SEARCH-LIST.
Search lists are what is used by the old emms-browser search function,
or the emf-filter-factory 'search-fields'."
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

(defun emf-format-cache-stack ()
  "Create a list of search crumb strings for the current search cache."
  (mapconcat #'identity (emf-get-search-keys) "\n"))

(defun emf-show-cache-stack ()
  "Message the current search cache stack."
  (interactive)
  (message "Emms Cache stack:\n%s\n"
           (mapconcat #'identity (emf-get-search-keys) "\n")))

(defun emf-show-cache-stash ()
  "Show the cache names in the stash."
  (interactive)
  (message "Emms cache stash:\n%s\n"
           (mapconcat 'identity
                      (reverse (mapcar #'car emf-cache-stash))
                      ", ")))

(defun emf-last-search-cache ()
  "Return the cache portion of the last search cache entry."
  (if (< 0 (length emf-search-caches))
      (cdar emf-search-caches)
    emms-cache-db))

(defun emf-pop-cache ()
  "Pop the search results cache and then render to show the previous search result."
  (interactive)
  (pop emf-search-caches))

(defun  emf-clear-caches ()
  "Clear the cache stack."
  (interactive)
  (setq emf-search-caches nil)
  (emf-refilter))

(defun  emf-swap-cache ()
  "Swap / reverse the last two entries in the cache stack."
  (interactive)
  (let* ((current (pop emf-search-caches))
         (previous (pop emf-search-caches)))
    (push current emf-search-caches)
    (push previous emf-search-caches)
    (emf-refilter)))

(defun  emf-swap-pop-cache ()
  "Swap and pop the cache stack."
  (interactive)
  (let* ((current (pop emf-search-caches)))
    (pop emf-search-caches)
    (push current search-caches-stack)))

(defun  emf-squash-caches ()
  "Squash the cache stack, keep the top entry."
  (interactive)
  (let* ((current (pop emf-search-caches)))
    (setq emf-search-caches nil)
    (push current emf-search-caches)))

(defun emf-search-stack-size ()
  "Give the current length of the search cache stack."
  (length emf-search-caches))

(defun emf-empty-result-message ()
  "Display some help if the results are empty."
  (concat "No records match with the current search cache and filters.\n"
          (format "Cache: %s\n Ring: %s\n Filter: %s\n"
                  (emf-current-cache-name)
                  (emf-current-ring-filter-name)
                  (car emf-current-filter))))

(defun emf-search-by (filter-factory-name)
  "Search using FILTER-FACTORY-NAME to create a filter.
Emulating the browser search, build a filter using factory name
and cache the results to the cache stack."
  (interactive)
  (emf-quick-one-shot filter-factory-name))

;; replacements for emms-browser and then some.
(defun emf-search-by-albumartist ()
  (interactive)
  (emf-quick-one-shot "Album artist"))

(defun emf-search-by-artist ()
  (interactive)
  (emf-quick-one-shot "Artist"))

(defun emf-search-by-composer ()
  (interactive)
  (emf-quick-one-shot "Composer"))

(defun emf-search-by-performer ()
  (interactive)
  (emf-quick-one-shot "Performer"))

(defun emf-search-by-title ()
  (interactive)
  (emf-quick-one-shot "Title"))

(defun emf-search-by-album ()
  (interactive)
  (emf-quick "Album"))

(defun emf-search-by-titles ()
  (interactive)
  (emf-quick-one-shot "Titles"))

(defun emf-search-by-names-and-titles ()
  (interactive)
  (emf-quick-one-shot "Names and titles"))

(defun emf-search-by-names ()
  (interactive)
  (emf-quick-one-shot "Names"))

(defun emf-search-by-all-text ()
  (interactive)
  (emf-quick-one-shot "All text"))

(setq emms-browser-mode-map
      (let ((map emms-browser-mode-map))
        (define-key map (kbd "Q") #'emf-pop-cache)
        (define-key map (kbd ">") #'emf-next-ring-filter)
        (define-key map (kbd "<") #'emf-previous-ring-filter)
        (define-key map (kbd "f !") #'emf-clear-ring-filter)
        (define-key map (kbd "f >") #'emf-next-ring-filter)
        (define-key map (kbd "f <") #'emf-previous-ring-filter)

        (define-key map (kbd "f S") #'emf-status-print)
        (define-key map (kbd "f S") #'emf-show-filters)
        (define-key map (kbd "f F") #'emf-show-filter-factories)
        (define-key map (kbd "f P") #'emf-pop)
        (define-key map (kbd "f h") #'emf-hard-filter)
        (define-key map (kbd "f r") #'emf-swap) ; rotate ?
        (define-key map (kbd "f R") #'emf-swap-pop) ; rotate-eject, ,pop-previous
        (define-key map (kbd "f f") #'emf-squash) ;flatten
        (define-key map (kbd "f k") #'emf-keep)
        (define-key map (kbd "f C") #'emf-clear-all)
        (define-key map (kbd "f c") #'emf-clear)
        (define-key map (kbd "f p") #'emf-select-push)
        (define-key map (kbd "f s") #'emf-select-smash)
        (define-key map (kbd "f o") #'emf-select-or)
        (define-key map (kbd "f a") #'emf-select-and)
        (define-key map (kbd "f n") #'emf-select-and-not)

        (define-key map (kbd "c p") #'emf-push-cache)
        (define-key map (kbd "c z") #'emf-stash-pop-cache)
        (define-key map (kbd "c Z") #'emf-stash-cache)
        (define-key map (kbd "c P") #'emf-pop-cache)
        (define-key map (kbd "c h") #'emf-hard-filter)
        (define-key map (kbd "c r") #'emf-swap-cache)
        (define-key map (kbd "c R") #'emf-swap-pop-cache)
        (define-key map (kbd "c f") #'emf-squash-caches)
        (define-key map (kbd "c c") #'emf-clear-caches)
        (define-key map (kbd "c s") #'emf-show-caches)
        (define-key map (kbd "c S") #'emf-show-cache-stash)

        (define-key map (kbd "s o") #'emf-search-by-albumartist)
        (define-key map (kbd "s a") #'emf-search-by-artist)
        (define-key map (kbd "s c") #'emf-search-by-composer)
        (define-key map (kbd "s p") #'emf-search-by-performer)
        (define-key map (kbd "s A") #'emf-search-by-album)
        (define-key map (kbd "s t") #'emf-search-by-title)
        (define-key map (kbd "s T") #'emf-search-by-titles)
        (define-key map (kbd "s n") #'emf-search-by-names)
        (define-key map (kbd "s s") #'emf-search-by-names-and-titles)
        (define-key map (kbd "s e") #'emf-search-by-all-text) ;everything.
        map))

;;; Testing
;;; -------------------------------------------------------------------
;;; Some convenience functions to make it easy to test a filter.

(defun emf-test-get-track-samples (cache &optional drop take)
  "Return a list of tracks from the CACHE, DROP tracks then TAKE as indicated.
Will drop 0 and take 1O by default."
  (let* ((tracks (list))
         (drop (or drop 0))
         (take (+ (or take 10) drop))
         (counter 0))
    (maphash (lambda (_path track)
               (when
                   (and
                    (> counter drop)
                    (< counter take))
                 (push track tracks))
               (setq counter (+ counter 1)))
             cache)
    tracks))

(defun emf-test-factory (factory-name parms track)
  "Create and test filter from FACTORY-NAME and PARMS.
Test it against TRACK."
  (funcall
   (emf-make--filter factory-name parms)
   track))

(defun emf-test-factory-interactive (factory-name track)
  "Interactively create and test filter from FACTORY-NAME.
Test it against TRACK."
  (funcall
   (emf-new-filter factory-name t)
   track))

(defun emf-test-filter-name (track filter-name &optional ring-filter-name)
  "Test filters identified by FILTER_NAME and RING-FILTER-NAME against a TRACK."
  (emf-test-filter
   track
   (cdr (assoc filter-name emf-filters))
   (if ring-filter-name
       (cdr (assoc ring-filter-name emf-filters))
     nil)))

(defun emf-test-filter (track filter &optional ring-filter)
  "Test TRACK against FILTER and optional RING-FILTER.
A functional equivalent to the emf-browser-hook function.
First we test the track against the ring-filter, then we combine
the result with the result of the filter."
  (and (if ring-filter
           (funcall ring-filter track)
         t)
       (if filter
           (funcall filter track)
         t)))

(defun emf-test-filter-tracks-direct (factory-name parms tracks)
  "Test a list of TRACKS against a filter created from FACTORY-NAME and PARMS.
Uses emf-test-factory directly rather than emulating the browser-hook-function.
Test it against some portion starting with START records and stopping
at STOP records of the existing cache-db.
Returns a list of cons with the filter result and the track."
  (mapcar (lambda (track)
            (cons
             (emf-test-factory factory-name parms track)
             track))
          tracks))

(defun emf-test-filter-tracks (factory-name parms tracks)
  "Test a list of TRACKS against a filter created from FACTORY-NAME and PARMS.
Emulates the browser-hook function by using emf-test-filter.
Test it against some portion starting with START records and stopping
at STP records of the existing cache-db.
Returns a list of cons with the filter result and the track."
  (let ((filter (emf-make--filter factory-name parms)))
    (mapcar (lambda (track)
              (cons
               (emf-test-filter track filter)
               track))
            tracks)))

(defun emf-test-filter-tracks-name (filter-name tracks)
  "Test a list of TRACKS against a FILTER-NAME.
Emulates the browser-hook function by using emf-test-filter.
Test it against some portion starting with START records and stopping
at STP records of the existing cache-db.
Returns a list of cons with the filter result and the track."
  (let ((filter (cdr (assoc filter-name emf-filters))))
    (mapcar (lambda (track)
              (cons
               (emf-test-filter-name track filter)
               track))
            tracks)))

(defun emf-test-find-tracks (cache filter)
  "Return a list of tracks from the CACHE filtered by function FILTER."
  (let* ((tracks (list))
         (counter 0))
    (maphash (lambda (_path track)
               (when (funcall filter track)
                 (push track tracks)))
             cache)
    tracks))

(defun emf-test-find-tracks-with-name (cache filter-name)
  "Return a list of tracks from the CACHE filtered by function FILTER."
  (let* ((tracks (list))
         (counter 0))
    (maphash (lambda (_path track)
               (when (funcall (cdr (assoc filter-name emf-filters)) track)
                 (push track tracks)))
             cache)
    tracks))

;;; Testing
;;; Some actual testing.
;;; Some sample tracks to use for data.
(setq emf-test-tracks
      '((*track* (type . file)
                 (name . "/Someone/Some-album/Some-song/track0001")
                 (info-playing-time . 180)
                 (info-discnumber . "1")
                 (info-artist . "Someone-else")
                 (info-title . "Some-song")
                 (info-tracknumber . "01")
                 (info-album . "Some-album")
                 (info-albumartist . "Someone")
                 (info-year . 1940)
                 (info-genre . "vals"))
        (*track* (type . file)
                 (name . "/Another-one/Another-album/Another-song/track0002")
                 (info-playing-time . 180)
                 (info-discnumber . "1")
                 (info-artist . "Another-Someone-else")
                 (info-title . "Another-song")
                 (info-tracknumber . "02")
                 (info-album . "Another-album")
                 (info-albumartist . "Another-one")
                 (info-year . 1935)
                 (info-genre . "tango"))))

(defun pretty-cons (cons-list)
  "pretty print a list of cons."
  (mapconcat (lambda (str) (format "%s\n" str))
             cons-list))

(defun emf-do-tests ()
  "A function for isolating and running some tests."
  ;; Make some sample data from the first few tracks from the cache.
  (let  ((emf-test-tracks-sample
          (emf-test-get-track-samples emms-cache-db))
         (first-test-track (car emf-test-tracks))
         (second-test-track (cadr emf-test-tracks)))

    ;; A direct use of the generated filter.

    ;; Create a filter from a factory and test it against a single track.
    (emf-test-factory "Genre" '("vals") first-test-track)
    (emf-test-factory "Genre" '("vals") second-test-track)

    (emf-test-factory "Titles" "Some" first-test-track)
    (emf-test-factory "Titles" "Some" second-test-track)

    ;; Test a few tracks against it.
    (pretty-cons (emf-test-filter-tracks "Genre" '("vals") emf-test-tracks))
    (pretty-cons (emf-test-filter-tracks "Genre" '("vals") emf-test-tracks-sample))
    (pretty-cons (emf-test-filter-tracks "Titles" '("Some") emf-test-tracks))
    (pretty-cons (emf-test-filter-tracks "Titles" '("Some") emf-test-tracks-sample))
    (pretty-cons (emf-test-filter-tracks "Titles" '("Viv") emf-test-tracks-sample))

    (emf-test-find-tracks emms-cache-db (emf-make--filter "Titles" '("sollo")))

    ;; Test interactive creation of a filter from a factory.
    ;; create a filter from a factory and test it against a single track.
    (emf-test-factory-interactive "Genre" first-test-track)
    (emf-test-factory-interactive "Titles" first-test-track)))

;; Make some old style browser filters to test
;; the filter-ring backward compatibility.
;; Steps to test:
;; 1. Make some old style emms-browser filters,
;; 3. Try them out directly by name.
;;
;; emms-browser-make-filter now inverts the filter result
;; for compatibility with emms-filters. The only interface to them
;; were next and previous functions.
;; That functionality is replicated with the emf-filter-ring.

;; (defun emms-browser-make-filter-genre (genre)
;;   "Make a filter by GENRE."
;;   (let ((filter (funcall emf-make-filter-genre genre)))
;;     (lambda (track)
;;       (not (filter track)))))

;; (defun emms-browser-make-filter-genre (genre)
;;   "Make a filter by GENRE."
;;   (lambda (track)
;;     (let ((info (emms-track-get track 'info-genre)))
;;       (not (and info (string-equal-ignore-case genre info))))))
;;
;; (emms-browser-make-filter "test-vals"
;;                           (emms-browser-make-filter-genre "vals"))
;; (emms-browser-make-filter "test-tango"
;;                           (emms-browser-make-filter-genre "tango"))
;; (emms-browser-make-filter "test-milonga"
;;                           (emms-browser-make-filter-genre "milonga"))

;; emf-filter-ring
;; (pretty-cons (emf-test-filter-tracks-name "test-vals" emf-test-tracks))

(provide 'emms-filters)
;;; emms-filters.el ends here.
