;;; emms-metafilter.el --- A metafilter for Emms          -*- lexical-binding: t; -*-
;;; Commentary:
;; Copyright (C) 2023

;; Author:  <ericalinagebhart@gmail.com> (Erica Gebhart)
;; Keywords:
;;; Code:

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

(require 'emms-browser)

(defvar  emf-stack nil
  "A history of multi-filters. Our working list.")

(defvar emf-current-multi-filter nil
  "The current multi-filter source list. ie. (cadr emf-stack)")

;; add, del, change - filter,
;; make new filter cons with constructed name.
;; push to meta stack
;; make function.

;; not sure I need this.
(defun emf-copy-meta-filter (filter)
  "Use FILTER, (name . filter) to create a meta filter.
Copy multi-filters. (name . multi-filter)"
  ;; copy from the multi-filters-list into the current working current-multi-filter
  ;;    (mapcar #'copy-sequence list-of-strings) So we dont modify them.
  (cons (car filter)
        (mapcar
         (lambda (filter-list)
           (mapcar #'copy-sequence filter-list)
           (cdr filter)))))

(defun emf-make-meta-filter (filter)
  "Use FILTER, (name . filter) to create a meta filter.
Copy multi-filters. (name . multi-filter)"
  (if (funcp (cdr filter))
      (cons (car filter)
            (list (list (car filter))))
    (emf-copy-meta-filter filter)))

(defun emf-format-meta-filter-groups (filter-list)
  "Format the FILTER-LIST contents to a list of strings."
  (mapconcat %identity " | " filter-list))

(defun emf-make-name (multi-filter)
  "Construct a name from the MULTI-FILTER contents."
  (mapconcat
   #'identity " && "
   (mapcar emf-format-multi-filter-groups multi-filter)))

(defun emf-make-filter-cons-from-meta-filter (filter)
  "Make a filter cons from multi-filter list FILTER."
  (cons (emf-make-name filter) filter))

(defun  emf-set-current-meta-filter-function ()
  "Make a multi-filter function from the current meta-filter.
Set it to be the current filter function."
  (setq emms-browser-current-filter
        (emms-browser-make-filter-multi-filter (car emf-stack)))
  ;; rerender.----------------------
  ;; (emms-browser-refilter emms-browser-current-filter)
  nil)

(defun emf-push-filter (filter)
  "Push a copy of FILTER to the meta-filter stack.
Make a filter function and set it."
  (push (emf-make-meta-filter filter) emf-stack)
  (emf-set-current-meta-filter-function))

(defun emf-push-meta-filter (meta-filter)
  "Push a copy of META-FILTER to the meta-filter stack.
Construct a name from its contents.
Make a filter function and set it."
  (emf-push-filter
   (emf-make-filter-cons-from-meta-filter filter)))

;;; base functions

(defun emf-current-meta-filter-name ()
  "Give the constructed name of the current filter."
  (emf-make-name (car emf-stack)))

(defun  emf-clear-stack ()
  "Clear the meta filter stack - a stack of multi-filter-lists."
  (setq emf-stack nil))

(defun  emf-quit/back/pop-meta-filter-stack ()
  "Pop the history stack, set the current filter function and re-render."
  (pop emf-stack)
  (emf-set-current-multi-filter-function))

;;; current state functions.
(defun  emf-keep-meta-filter ()
  "Import the current filter into the list of multi-filters for the session."
  (emf-set-current-meta-filter-function))

(defun  emf-clear-filter ()
  "Set to nil '((nil)) filter."
  (pop emf-stack)
  (emf-push-meta-filter '(("((nil))" nil))))

(defun  emf-default ()
  "Set to default filter."
  (emf-push-meta-filter '((emms-browser-default-filter)) ))

(defun emf-select-filter ()
  "Select a filter."
  (interactive)
  (let* ((filter-name
          (completing-read "Choose a filter:"
                           emms-browser-filters nil t))
         (filter (assoc filter-name emms-browser-filters)))
    (emms-browser-refilter filter)))

(defun  emf-or-select ()
  "Add to current list. 'OR' new choice."
  (let* ((filter (car emf-stack))
         (filter-list (cdar filter))
         (filter-name
          (completing-read "Choose a filter:"
                           emms-browser-filters nil t))
         (filter-list (cons filter-name filter-list)))
    (emf-push-meta-filter filter)))

(defun  emf-and-select ()
  "Add to current filter, new list, new choice, select a filter to 'AND'."
  (let* ((filter-lists (cadr emf-stack))
         (filter-name
          (completing-read "Choose a filter:"
                           emms-browser-filters nil t)))
    (add-to-list filter-lists '((filter-name)))
    (emf-push-meta-filter filter-lists)))

(defun  emf-delete-current-or-group ()
  "Delete the current/last 'OR' group."
  (let* ((filter (car emf-stack))
         (filter (cdr filter)))
    (emf-push-meta-filter filter)))

(provide 'emms-metafilter.el)
;;; emms-metafilter.el ends here.
