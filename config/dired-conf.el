(require 'diredfl)
(diredfl-global-mode)
;;(setq diredfl-mode 't)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(dired-async-mode 1)

;; I have git gutter already.
;; (global-diff-hl-mode)
;; (add-hook 'prog-mode-hook 'diff-hl-mode)
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
