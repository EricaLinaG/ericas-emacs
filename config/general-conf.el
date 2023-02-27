(require 'general)

;; * Global Keybindings
;; `general-define-key' acts like `evil-define-key' when :states is specified
(general-define-key
 :states 'motion
 ;; swap ; and :
 ";" 'evil-ex
 ":" 'evil-repeat-find-char)
;; same as

(general-define-key
 :states 'motion
 ";" 'evil-ex
 ":" 'evil-repeat-find-char)
;; `general-def' can be used instead for `evil-global-set-key'-like syntax

(general-def 'motion
  ";" 'evil-ex
  ":" 'evil-repeat-find-char)

;; alternative using `general-translate-key'
;; swap ; and : in `evil-motion-state-map'
(general-swap-key nil 'motion
  ";" ":")

;; * Mode Keybindings
(general-define-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 ;; or xref equivalent
 "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
;; `general-def' can be used instead for `evil-define-key'-like syntax
(general-def 'normal emacs-lisp-mode-map
  "K" 'elisp-slime-nav-describe-elisp-thing-at-point)

;; * Prefix Keybindings
;; :prefix can be used to prevent redundant specification of prefix keys
;; again, variables are not necessary and likely not useful if you are only
;; using a definer created with `general-create-definer' for the prefixes
;; (defconst my-leader "SPC")
;; (defconst my-local-leader "SPC m")

(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix "SPC m")

;; ** Global Keybindings
(my-leader-def
 :keymaps 'normal
 ;; bind "SPC a"
 "a" 'org-agenda
 "b" 'counsel-bookmark
 "t" 'hydra-persp/body
 "c" 'org-capture
 ;;;;;;;;;;;;;;;;;;;;;;
 "D" 'describe-mode
 "d" 'dmenu
 "l" 'hydra-language/body


  ;;; Finding
 "n" 'hydra-window/body
 "T" 'find-tag-without-ns  ;; ctags
 "a" 'ag
 "s" 'evil-ace-jump-word-mode  ;;  ace jump search

 "u" 'hydra-sub-map
 "U" 'hydra-hydras/body
 "G" 'golden-ratio-mode
 "g" 'magit-other-frame  ;; magit is git.
 "i" 'imenu
 "j" 'dired-jump

 "f" 'find-file
 ;;"O" 'ido-find-file-other-window

 "o" 'hydra-org/body
 ;; "p" 'projectile-buffers-with-file
 "h" 'ibuffer
 "H" 'consult-buffer  ;switch-to-buffer
 "w" 'helm-buffer  ;switch-to-buffer
 "m" 'woman

 "x" 'smex  ;; M-x.
 "K" 'delete-frame
 "k" 'kill-this-buffer
 "q" 'kill-this-buffer

 "e" 'er/expand-region

 "0" 'delete-window
 "1" 'delete-other-windows
 "2" 'split-window-below
 "3" 'split-window-right

 )

;; `general-create-definer' creates wrappers around `general-def', so
;; `evil-global-set-key'-like syntax is also supported
(my-leader-def 'normal
               "a" 'org-agenda
               "b" 'counsel-bookmark
               "c" 'org-capture)

;; to prevent your leader keybindings from ever being overridden (e.g. an evil
;; package may bind "SPC"), use :keymaps 'override
(my-leader-def
 :states 'normal
 :keymaps 'override
 "a" 'org-agenda)
;; or
(my-leader-def 'normal 'override
               "a" 'org-agenda)

;; ** Mode Keybindings
(my-local-leader-def
 :states 'normal
 :keymaps 'org-mode-map
 "y" 'org-store-link
 "p" 'org-insert-link
 ;; ...
 )
;; `general-create-definer' creates wrappers around `general-def', so
;; `evil-define-key'-like syntax is also supported
(my-local-leader-def 'normal org-mode-map
                     "y" 'org-store-link
                     "p" 'org-insert-link
                     ;; ...
                     )

;; * Settings
;; change evil's search module after evil has been loaded (`setq' will not work)
(general-setq evil-search-module 'evil-search)
