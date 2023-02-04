;;
;;
;;; Turn on Vi mode.
(evil-mode t)

(global-hl-line-mode 1)

;;; Great for smaller screens.
(golden-ratio-mode)

;;(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Start up a shell.  Pick your poison.
;; see config/terminal-conf.el
;;
;; shell is perhaps the easiest, but limited.
;; multi-term is great once you get it working.
;; eshell is great if you want a lisp shell, not good for
;; big files, or streams.
;; term and ansi-term misbehave a lot in my experience.

;;(setq cb-shell-command 'shell)
(setq cb-shell-command 'multi-term)
;;(setq cb-shell-command 'eshell)
;;(setq cb-shell-command 'term)
;;(setq cb-shell-command 'ansi-term)

;;;squiggly-clojure.

;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

;; (add-hook 'cider-mode-hook
;;           (lambda () (setq next-error-function #'flycheck-next-error-function)))
