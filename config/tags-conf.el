;;;package --- Summary

;;; Commentary:
(require 'ctags-update)


;;; Code:

;;(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t)
;;(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
;;(add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)
;;(add-hook 'emacs-clojure-mode-hook  'turn-on-ctags-auto-update-mode)

(autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
                                        ;(global-set-key "\C-cE" 'ctags-update)

                                        ; I don't want to be asked to confirm everytime the TAGS file changes.
(setq tags-revert-without-query 1)

;; (defun find-tag-without-ns (next-p)
;;   (interactive "P")
;;   (find-tag (car (last (split-string (symbol-name (symbol-at-point)) "/")))
;;             next-p))

;; (defun find-next-tag-without-ns (s)
;;   (interactive "P")
;;   (find-tag (car (last (split-string (symbol-name (symbol-at-point)) "/")))
;;             t))

(defun find-tag-without-ns (tagname &optional next-p regexp-p)
  "Find tag (in current tags table) the names which contain TAGNAME.
Select the buffer containing the tag's definition, and move point there.
The default for TAGNAME is the expression in the buffer around or before point.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is the atom `-' (interactively, with prefix arg that is a negative number
or just \\[negative-argument]), pop back to the previous tag gone to.

If third arg REGEXP-P is non-nil, treat TAGNAME as a regexp.

A marker representing the point when this command is invoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'."
  (interactive (find-tag-without-ns-interactive "Find tag: "))
  (let* ((buf (find-tag-noselect tagname next-p regexp-p))
	 (pos (with-current-buffer buf (point))))
    (condition-case nil
	(switch-to-buffer buf)
      (error (pop-to-buffer buf)))
    (goto-char pos)))

(defun find-tag-without-ns-interactive (prompt &optional no-default)
  "Get interactive arguments at PROMPT for tag functions, NO-DEFAULT.
The functions using this are `find-tag-noselect',
`find-tag-other-window', and `find-tag-regexp'."
  (if (and current-prefix-arg last-tag)
      (list nil (if (< (prefix-numeric-value current-prefix-arg) 0)
                    '-
                  t))
    (list (if no-default
              (read-string prompt)
            (find-tag-tag-without-ns prompt)))))

(defun find-tag-tag-without-ns (string)
  "Read a tag name from STRING, with defaulting and completion."
  (let* ((completion-ignore-case (if (memq tags-case-fold-search '(t nil))
				     tags-case-fold-search
				   case-fold-search))
	 (default (funcall (or find-tag-default-function
			       (get major-mode 'find-tag-default-function)
			       'find-tag-default)))
         (default (car (last (split-string default "/")))) ; remove the namespace
         (spec (completing-read (if default
                                    (format "%s (default %s): "
                                            (substring string 0 (string-match "[ :]+\\'" string))
                                            default)
                                  string)
                                (tags-lazy-completion-table)
                                nil nil nil nil default)))
    (if (equal spec "")
	(or default (user-error "There is no default tag"))
      spec)))
