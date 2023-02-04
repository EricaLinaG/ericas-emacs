;;; package ---Summary
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the loadpath / bytecompile everything - Go -- Eric
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code:
;; cl is obsolete, cl-lib is the replacement
;; Lots of current packages still use cl.
;; this gets rid of the warning everytime emacs starts.
(setq byte-compile-warnings '(cl-functions))

(setq load-path
      (append (list
               (expand-file-name "./early-packages" user-emacs-directory)
	       (expand-file-name "./themes" user-emacs-directory)
	       (expand-file-name "./extensions" user-emacs-directory)
	       (expand-file-name "./config" user-emacs-directory))
	      load-path))

(setq custom-file "~/.config/emacs-custom.el")
(load custom-file)

;; so org doesn't step on the S-Arrow keys from windmove.
(setq org-replace-disputed-keys t)

(load "mypackages") ; make sure all the packages are installed.

;; just so I dont have to look at Emacs default so long.
(load "appearance")
(load "themes-init")
;; this is a local theme I made. Code is in themes.
;; (load-theme 'strange-deeper-blue t)
;; (enable-theme 'strange-deeper-blue)
(require 'modus-themes)


;; Faint yellow comments and a different shade of green for strings
(setq modus-themes-common-palette-overrides
      '((string yellow-faint)
        (fringe unspecified)
        ;;(string green-warmer)
        (bg-region bg-lavender) ; try `bg-ochre' `bg-lavender', `bg-sage'
        (fg-region unspecified)
        (bg-paren-match bg-magenta-intense)
        (underline-paren-match fg-main)))
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-prompts '(intense-bold)
      modus-themes-completions 'intense-bold
      modus-themes-org-blocks 'tinted-background)

(load-theme 'modus-vivendi-tinted t)             ; Dark theme

;; load up everything, compiling as needed.
;; still the simplest, even though just load could
;; work if auto compile was on for loading and saving..
;; theoretically, if everything was just right.
;; takes the same amount of time either way.

;; conditionally recompile and load these dirs.
(require 'bytecompiledir)
(byte-compile-directory (expand-file-name "./extensions" user-emacs-directory))
(byte-compile-directory (expand-file-name "./config" user-emacs-directory))
(byte-compile-directory (expand-file-name "./themes" user-emacs-directory))

(load "display_hints") ; advice for various buffers.

;;; Some global modes
;;; Turn on Vi mode.
(evil-mode t)

;;; line highlight
(global-hl-line-mode 1)

;;; Great for smaller screens. or bigger ones...
(golden-ratio-mode)

(setq visible-bell 1)

;; windmove navigation
(windmove-default-keybindings)         ; shifted arrow keys
(setq windmove-wrap-around t)

(provide 'init)
;;; init.el ends here
