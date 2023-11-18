;; (defun emms-browser-make-multi-filter (filter-name multi-list)
;;   "Create a filter function with FILTER-NAME from MULTI-LIST."
;;   (emms-browser-make-filter
;;    filter-name
;;    (emms-browser-make-filter-multi-filter multi-list)))

;; A meta multi-filter. to manage the filters.
;; (key . multi-filter-list)
;; (key . filter-func)
;; ("vals or milonga, 1900-1937" . '(("1900-1929" "1929-1937")
;;                                   ("vals" "milonga")))
;;
;; A filter function is represented like so.
;; (key . '((key)) )

;; So when a new filter is set, we turn it into a meta filter
;; and push it on a stack. We make a function from it and set it
;; to the current filter.
;; emms-browser-current-filter

;; then, if anyone wants to add to the filter, there are ands and ors .
;; so we can manipulate the active filter and push changes to a stack
;; that we can quit out of.

;; If someone wants to keep a filter we make a filter from it and it
;; goes into the normal filter list.

;;; Vars
(defvar default-multi-filter nil
  "The default multi-filter.")

(defvar  meta-filter-stack nil
  "A history of multi-filters. Our working list.")

(defvar emms-browser-current-multi-filter nil
  "The current multi-filter source list. ie. (cadr multi-filter-stack)")

(defun last-multi-filter ()
  "The last active multi-filter."
  (nth 2 meta-filter-stack))

(defun emms-browser-copy-meta-filter (filter)
  "Use FILTER, (name . filter) to create a meta filter.
Copy multi-filters. (name . multi-filter)"
  ;; copy from the multi-filters-list into the current working current-multi-filter
  ;;    (mapcar #'copy-sequence list-of-strings) So we dont modify them.
  (cons (car filter)
        (mapcar
         (lambda (filter-list)
           (mapcar #'copy-sequence filter-list)
           (cdr filter)))))

(defun emms-browser-make-meta-filter (filter)
  "Use FILTER, (name . filter) to create a meta filter.
Copy multi-filters. (name . multi-filter)"
  (if (funcp (cdr filter))
      (cons (car filter)
            (list (list (car filter))))
    (emms-browser-copy-meta-filter filter)))

;; add, del, change - filter,
;; make new filter cons with constructed name.
;; push to meta stack
;; make function.

(defun make-filter-cons-from-metafilter (filter)
  "Make a filter cons from multi-filter list FILTER."
  (cons (emms-browser-make-meta-filter-name filter) filter))

(defun  set-current-multi-filter-function ()
  "Make a multi-filter function from MULTI-FILTER and set it as current."
  (setq emms-browser-current-filter
        (emms-browser-make-filter-multi-filter (car multi-filter-stack))))

(defun  push-multi-filter (filter)
  "Push a copy of MULTI-FILTER to the meta-filter stack.
Make a filter function and set it."
  (push (emms-browser-make-meta-filter filter) multi-filter-stack)
  (set-current-multi-filter-function)

  ;; rerender.----------------------
  )

(defun push-meta-filter (meta-filter)
  "Push a copy of MULTI-FILTER to the meta-filter stack.
Construct a name from its contents.
Make a filter function and set it."
  (push-multi-filter
   (make-filter-cons-from-metafilter filter)))

;;; base functions

(defun format-meta-filter-groups (filter-list)
  "Format the MULTI-FILTER contents to a list of strings."
  (mapconcat %identity " | " filter-list))

(defun emms-browser-make-meta-filter-name (multi-filter)
  "Construct a name from the MULTI-FILTER contents."
  (mapconcat
   #'identity " && "
   (mapcar format-multi-filter-groups multi-filter)))

(defun emms-browser-current-meta-filter-name ()
  "Give the constructed name of the current filter."
  (emms-browser-make-multi-filter-name (car multi-filter-stack)))

;; what does this even mean ? set it to current filter whatever that Is.
;; delete and set to whatever is set by name.

(defun  clear-meta-filter-stack ()
  "Clear the stack - a stack of multi-filter-lists."
  (setq meta-filter-stack nil)
  ;; rerender.----------------------
  )

(defun  quit/back/pop-meta-filter-stack ()
  "Pop the history stack, set the current filter function and re-render."
  (pop meta-filter-stack)
  (set-current-multi-filter-function)
  ;; rerender.----------------------
  )

;;; current state functions.
(defun  keep-meta-filter ()
  "Import the current filter into the list of multi-filters for the session."
  (import-meta-filter (car meta-filter-stack)))

(defun  clear-multi-filter ()
  "Set to nil '((nil)) filter."
  (setq meta-filter-stack nil)
  (push-multi-filter (cons "((nil))"
                           #'ignore ))
  ;; rerender.----------------------
  )

(defun  default ()
  "Set to default filter."
  ;; rerender.----------------------
  (push-meta-filter '((emms-browser-default-filter)) ))

(defun emms-browser-select-filter ()
  "Select a filter."
  (interactive)
  (let* ((filter-name
          (completing-read "Choose a filter:"
                           emms-browser-filters nil t))
         (filter (assoc filter-name emms-browser-filters)))
    (emms-browser-refilter filter)))

(defun  or-select ()
  "Add to current list. 'OR' new choice."
  (let* ((filter (car meta-filter-stack))
         (filter-list (cdar filter))
         (filter-name
          (completing-read "Choose a filter:"
                           emms-browser-filters nil t))
         (filter-list (cons filter-name filter-list)))
    (push-meta-filter filter)))
;; rerender.----------------------

(defun  and-select ()
  "Add to current filter, new list, new choice, select a filter to 'AND'."
  (let* ((filter-lists (cadr meta-filter-stack)))
    (add-(filter-name
          (completing-read "Choose a filter:"
                           emms-browser-filters nil t))
         to-list filter-lists '((filter-name)) )
    (push-meta-filter filter-lists))
  ;; rerender.----------------------
  )

(defun  delete-current-or-group ()
  "Delete the current/last 'OR' group."
  (let* ((filter (car meta-filter-list))
         (filter (reverse (cdr (reverse filter)))))

    (push-meta-filter filter)))
;; rerender.----------------------
