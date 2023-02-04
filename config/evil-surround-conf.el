;; Evil-surround configuration
(require 'evil-surround)
(global-evil-surround-mode 1)
;; Add suport for paredit.
(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-change . change))
(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-delete . delete))
