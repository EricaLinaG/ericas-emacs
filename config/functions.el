;;; package --- Summary
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions
;;
;; Purpose:  Give a space for miscellaneous functions for experimentation
;;           and enhancements to your personal environment.
;;
;; Author: Eric Gebhart  e.a.gebhart@gmail.com
;;         Erica Lina
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(defun main-window ()
  (interactive)
  (balance-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (split-window-horizontally)
  (split-window-horizontally)
  ;;(split-window-below)
  ;;(cb-get-shell)
  )

;; (require 'eaf-browser)
;; (defun eaf-open-url (url)
;;   "Non interactive way to open a browser url in eaf-browser.
;;    Wraps urls with https:// as needed."
;;   (eaf-open (eaf-wrap-url url) "browser"))

(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

( defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

(defun test-window ()
  (interactive)
  (display-buffer
      (get-buffer-create "*shell*")
      '((
            display-buffer-below-selected
            display-buffer-pop-up-window
         )
        (reusable-frames . 0)
        (window-height . 10) )))

;complete find-tag with ido
 (defun ido-find-tag ()
    "Find a tag using ido"
    (interactive)
    (tags-completion-table)
    (let (tag-names)
      (mapatoms (lambda (x)
                  (push (prin1-to-string x t) tag-names))
                tags-completion-table)
      (find-tag (ido-completing-read "Tag: " tag-names))))

;find files in TAGS file.
(defun ido-find-file-in-tag-files ()
      (interactive)
      (save-excursion
        (let ((enable-recursive-minibuffers t))
          (visit-tags-table-buffer))
        (find-file
         (expand-file-name
          (ido-completing-read
           "Project file: " (tags-table-files) nil t)))))


;; Electrify return with paredit mode.

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;;; electrify for paredit...
(defun par-edit-mode-hook-func ()
  (paredit-mode t)
  (eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  (local-set-key (kbd "RET") 'electrify-return-if-match)
  (eldoc-add-command 'electrify-return-if-match)
  (show-paren-mode t))
