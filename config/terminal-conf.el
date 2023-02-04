;;; Package -- Summary
;;; Commentary:
;;; Configure ansi-term and term modes.

(require 'multi-term)

;;; Code:

;;; terminal settings...

;;; You may want to run this to make sure the terminfo is available.
;;; this is the OS X version. which may not be necessary on other systems.
;;; basically make sure you have emacs eterm-color entry in your terminfo.
;;mkdir ~/.terminfo
;;tic -o ~/.terminfo /Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti

;;; Turn off yas snippet for terminal so auto complete <TAB> will work.
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))

;; pick up said terminfo entry.
(setq system-uses-terminfo nil)

(setq explicit-shell-file-name "/bin/zsh")
(setq multi-term-program "/bin/zsh")

;; increase the history size.
;; set the colors so they play nice.
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)))

;; get copy and paste to work in terminal.
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-v") 'term-paste)))

;; most good themes will set these. This example is stolen from Zenburn.
;; ;; term
;; `(term-color-black ((t (:foreground ,zenburn-bg
;;                                     :background ,zenburn-bg-1))))
;; `(term-color-red ((t (:foreground ,zenburn-red-2
;;                                   :background ,zenburn-red-4))))
;; `(term-color-green ((t (:foreground ,zenburn-green
;;                                     :background ,zenburn-green+2))))
;; `(term-color-yellow ((t (:foreground ,zenburn-orange
;;                                      :background ,zenburn-yellow))))
;; `(term-color-blue ((t (:foreground ,zenburn-blue-1
;;                                    :background ,zenburn-blue-4))))
;; `(term-color-magenta ((t (:foreground ,zenburn-magenta
;;                                       :background ,zenburn-red))))
;; `(term-color-cyan ((t (:foreground ,zenburn-cyan
;;                                    :background ,zenburn-blue))))
;; `(term-color-white ((t (:foreground ,zenburn-fg
;;                                     :background ,zenburn-fg-1))))
;; '(term-default-fg-color ((t (:inherit term-color-white))))
;; '(term-default-bg-color ((t (:inherit term-color-black))))
