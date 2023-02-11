(require 'diredfl)
(diredfl-global-mode)
;;(setq diredfl-mode 't)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(dired-async-mode 1)

(setq dired-create-destination-dirs 'always)
;; I have git gutter already.
;; (global-diff-hl-mode)
;; (add-hook 'prog-mode-hook 'diff-hl-mode)
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(add-to-list 'auto-mode-alist '("\\.\\(cbr\\)\\'" . archive-mode))

(provide 'dired-conf)
;;; dired-conf.el ends here
