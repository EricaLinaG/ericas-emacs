;;; package --- Summary
;; I initially did this when popwin was causing incompatibilty problems.
;; The reality is that things like popwin obsolete and are replaced by the
;; display-buffer-alist hints, since v25 I think.
;;
;; In the end, I really like the flow of magit in a second temporary frame.
;; Eric - 2019

;;; Commentary:
;; Provides functionality to open magit in another frame.
;; When configured, will always create a new frame for magit status,
;; which will close with 'q' in magit if the magit-mode-map is modified.
;; It keeps magit out of my windows.

;; Do something like this, where ever it is that you keep your keybindings.
;; ;; force magit into a new temporary frame so it doesn't mess with my
;; ;; current frame.
;; (global-set-key (kbd "C-c g") 'magit-other-frame)
;; (define-key magit-mode-map (kbd "q") 'delete-frame)

;;; Code:
(require 'magit-git)
(require 'magit-status)

(defun magit-other-frame ()
  "Start Magit status in another frame."
  (interactive)
  (if (magit-toplevel)
      (progn
        (select-frame (make-frame-command))
        (magit-status-setup-buffer)
        (delete-other-windows))
    (magit--not-inside-repository-error)))

(provide 'git-frame)
;;; git-frame.el ends here
