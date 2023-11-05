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

(general-define-key
 ;; NOTE: keymaps specified with :keymaps must be quoted
 :keymaps 'emms-playlist-mode-map
 "h" 'hydra-emms/body)
(general-define-key
 ;; NOTE: keymaps specified with :keymaps must be quoted
 :keymaps 'emms-browser-mode-map
 "h" 'hydra-emms/body)


;; * Prefix Keybindings
;; :prefix can be used to prevent redundant specification of prefix keys
;; again, variables are not necessary and likely not useful if you are only
;; using a definer created with `general-create-definer' for the prefixes
;; (defconst my-leader "SPC")
;; (defconst my-local-leader "SPC m")

(general-create-definer my-leader-def
  ;; :prefix my-leader
  :keymaps '(normal insert emacs)
  :prefix "SPC"
  :non-normal-prefix "C-c SPC"
  :global-prefix "C-c SPC"
  )

(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix "SPC m")

;; `general-create-definer' creates wrappers around `general-def,, so
;; `evil-global-set-key'-like syntax is also supported
;; (my-leader-def '(normal insert emacs) mykeys)

;; ** Global Keybindings
(my-leader-def
  "A" 'org-agenda
  ;; "a" 'org-agenda
  ;; "b" 'counsel-bookmark
  ;; "c" 'org-capture
  "B" 'counsel-bookmark
  "b" 'helm-buffer
  "t" 'hydra-persp/body
  "e" 'hydra-emms/body
  "c" 'org-capture
 ;;;;;;;;;;;;;;;;;;;;;;
  "D" 'describe-mode
  "d" 'dmenu
  "l" 'hydra-language/body
  "u" 'hydra-frames-windows/body

  ;;; Finding
  "n" 'hydra-window/body
  "T" 'find-tag-without-ns  ;; ctag
  "a" 'ag
  "s" 'evil-ace-jump-word-mode  ;;  ace jump search

  ;; "u" 'hydra-sub-map
  "U" 'hydra-hydras/body
  "G" 'golden-ratio-mode
  "g" 'magit-other-frame  ;; magit is git.
  "i" 'imenu
  "j" 'dired-jump

  ;; "f" 'find-file
  "f" 'helm-find-file
  ;;"O" 'ido-find-file-other-window

  "o" 'hydra-org/body
  "r" 'hydra-org-roam/body
  ;; "p" 'projectile-buffers-with-file
  "H" 'ibuffer
  "h" 'consult-buffer  ;switch-to-buffer
  "m" 'woman
  "M" 'hydra-multiple-cursors/body
  "M" 'hydra-macros/body
  "p" 'hydra-persp/body  ; this is on t also.
  "P" 'hydra-programs/body
  "w" 'hydra-window-hydras/body

  "x" 'smex  ;; M-x.
  "K" 'delete-frame
  "k" 'kill-this-buffer
  "q" 'kill-this-buffer

  "E" 'er/expand-region

  "0" 'delete-window
  "1" 'delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-right
  )

;; (my-global-leader-def
;;   ;; bind "C-c SPC a"
;;   mykeys
;;   )


;; ;; to prevent your leader keybindings from ever being overridden (e.g. an evil
;; ;; package may bind "SPC"), use :keymaps 'override
;; (my-leader-def
;;  :states 'normal
;;  :keymaps 'override
;;  "a" 'org-agenda)
;; ;; or
;; (my-leader-def 'normal 'override
;;                "a" 'org-agenda)

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
