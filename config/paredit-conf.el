;;; Package --- Summary
;;; Commentary:
;;; Choose your poison.
;;; paredit & evil-paredit or
;;; smartparens and evil-smartparens or evil-cleverparens.
;;; or maybe just autopair.
;;; Code:

;; (autoload 'enable-paredit-mode "paredit"
;;  "Turn on pseudo-structural editing of Lisp code." t)

;; (require 'paredit)
;; (require 'evil-paredit)
(require 'highlight-parentheses)
(require 'smartparens-config)
(require 'evil-smartparens)

;; (require 'evil-cleverparens)
;; (require 'evil-cleverparens-text-objects)

;; Always start smartparens mode in js-mode.
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'sh-mode-hook #'smartparens-mode)
(add-hook 'esh-mode-hook #'smartparens-mode)
(add-hook 'r-mode-hook #'smartparens-mode)
(add-hook 'ruby-mode-hook #'smartparens-mode)


;; Too many years with paredit. Use those key bindings for smartparens.
(sp-use-paredit-bindings)

;; This will turn on evil-smartparens whenever smartparens is
;; activated.
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

;; This might not be what you want, as you're likely to get some
;; surprises when working in buffers running say markdown-mode. If you just
;; want to enable evil-smartparens in certain modes you can do something
;; like this:

(add-hook 'emacs-lisp-mode-hook       #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook             #'smartparens-strict-mode)
(add-hook 'lisp-interaction-mode-hook #'smartparens-strict-mode)
(add-hook 'scheme-mode-hook           #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook          #'smartparens-strict-mode)
(add-hook 'cider-mode-hook            #'smartparens-strict-mode)
(add-hook 'sh-mode-hook               #'smartparens-strict-mode)

;; I think cleverparens is too magical and not intuitive.

;; (add-hook 'emacs-lisp-mode-hook       #'evil-cleverparens-mode)
;; (add-hook 'lisp-mode-hook             #'evil-cleverparens-mode)
;; (add-hook 'lisp-interaction-mode-hook #'evil-cleverparens-mode)
;; (add-hook 'scheme-mode-hook           #'evil-cleverparens-mode)
;; (add-hook 'clojure-mode-hook          #'evil-cleverparens-mode)
;; (add-hook 'cider-mode-hook            #'evil-cleverparens-mode)
;; (add-hook 'sh-mode-hook               #'evil-cleverparens-mode)

;; paredit doesn't work so well with non-lisps. I miss it when
;; editing imperative languages. - so smartparens instead.
;; (add-hook 'emacs-lisp-mode-hook       'par-edit-mode-hook-func)
;; (add-hook 'lisp-mode-hook             'par-edit-mode-hook-func)
;; (add-hook 'lisp-interaction-mode-hook 'par-edit-mode-hook-func)
;; (add-hook 'scheme-mode-hook           'par-edit-mode-hook-func)
;; (add-hook 'clojure-mode-hook          'par-edit-mode-hook-func)
;; (add-hook 'cider-mode-hook            'par-edit-mode-hook-func)

;; (defvar autopair-modes '(r-mode ruby-mode python-mode sh-mode))
;; (defun turn-on-autopair-mode () (autopair-mode 1))
;; (dolist (mode autopair-modes) (add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-autopair-mode))

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
