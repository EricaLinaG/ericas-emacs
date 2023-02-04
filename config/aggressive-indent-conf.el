;;; Package --- Summary
;;; Commentary:

;; if global mode is on, the variable aggressive-indent-excluded-modes
;; is in effect, and it has a long list of modes to not use aggressive indent.
;; python, haskell, yaml, sql, etc.

;; I miss it in those langs, but it doesn't work so well I guess.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- from a thread somewhere.
;;
;; Aggressive-indent intentionally disables itself on major modes where it
;; wouldn't make sense. python-mode is one of them.

;; The problem is that indentation is not absolute in python, so Emacs has
;; no way of performing it automatically.

;; You can disable this safeguard (at your own risk) with:

;; (setq aggressive-indent-excluded-modes
;;       (remove 'python-mode aggressive-indent-excluded-modes))
;;
;; -- Malarba - the author.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(require 'aggressive-indent)

(global-aggressive-indent-mode t)

;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'css-mode-hook #'aggressive-indent-mode)
;; (add-hook 'python-mode-hook #'aggressive-indent-mode)
;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
;; (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'scheme-mode-hook #'aggressive-indent-mode)
;; (add-hook 'c-mode-hook #'aggressive-indent-mode)
;; (add-hook 'sh-mode-hook #'aggressive-indent-mode)

;; You can use this hook on any mode you want, aggressive-indent is not
;; exclusive to emacs-lisp code. In fact, if you want to turn it on for
;; every programming mode, you can do something like:

;; (global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
