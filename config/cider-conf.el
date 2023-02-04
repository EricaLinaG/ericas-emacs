(require 'cider)
(require 'ac-cider)
;;(require 'highlight)
;;(require 'cider-eval-sexp-fu)


(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;;; is for using autocomplete, not company mode.
;;(add-hook 'cider-mode-hook 'ac-cider-setup)
;;(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;; (eval-after-load "auto-complete"
;; '(add-to-list 'ac-modes 'cider-mode))

(add-hook 'cider-repl-mode-hook #'eldoc-mode)
;; (add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
;; (add-hook 'cider-repl-mode-hook #'(lispy-mode 1))
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(add-hook 'cider-mode-hook #'eldoc-mode)
;; (add-hook 'cider-mode-hook #'paredit-mode)
;; (add-hook 'cider-mode-hook #'(lispy-mode 1))


(setq cider-show-error-buffer t)
(add-to-list 'same-window-buffer-names "*cider*")
(setq cider-repl-use-clojure-font-lock t)
(setq cider-auto-select-error-buffer nil)
(setq cider-repl-use-pretty-printing t)

(defadvice cider-load-current-buffer (after switch-namespace activate compile)
  "switch to namespace"
  (cider-repl-set-ns (cider-current-ns))
  (cider-switch-to-repl-buffer))
