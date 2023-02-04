;;; Package --- Summary
;;; Commentary:
;;; Code:

;; Ace-window, Ace-jump-buffer, frog-jump-buffer
(require 'ace-jump-buffer)
(require 'frog-menu)

(global-set-key (kbd "M-o") 'ace-window)

;; dvorak home row, beakl regions (3x3).
(setq aw-keys '(?o ?e ?u ?h ?t ?n ?, ?. ?p ?g ?c ?l ?q ?j ?k ?m ?w ?v))

;; Whenever ace-window prompts for a window selection, it grays out
;; all the window characters, highlighting window labels in red.  To
;; disable this behavior, set this:

;;(setq aw-background nil)

;; If you want to know the selection characters ahead of time, turn on
(ace-window-display-mode)

;; variable from from menu.
;; Dvorak with beakl regions, (3x3).
;; "Frog menu keys used for `avy-keys'.
(custom-set-variables '(frog-menu-avy-keys (append (string-to-list "oeuhtn")
                                                   (string-to-list ",.pgcl")
                                                   (string-to-list "qjkmwv")
                                                   (string-to-list "OEUHTN")
                                                   (string-to-list "<>PGCR")
                                                   (string-to-list "QJKMWV")
                                                   (number-sequence ?, ?@))))

;; This magnificent package takes care of this issue. It’s unnoticeable if
;; you have <3 panes open, but with 3 or more, upon pressing C-x o you will
;; notice how your buffers turn a solid color and each buffer is asigned a
;; letter (the list below shows the letters, you can modify them to suit your
;; liking), upon pressing a letter asigned to a window, your will be taken
;; to said window, easy to remember, quick to use and most importantly, it
;; annihilates a big issue I had with emacs.

;; An alternative is ace-window,
;; however by default it also changes the behaviour of C-x o even if only
;; 2 windows are open, this is bad, it also works less well with exwm for
;; some reason.

;; (use-package switch-window
;;              :ensure t
;;              :config
;;              (setq switch-window-input-style 'minibuffer)
;;              (setq switch-window-increase 4)
;;              (setq switch-window-threshold 2)
;;              (setq switch-window-shortcut-style 'qwerty)
;;              (setq switch-window-qwerty-shortcuts
;;                    '("o" "e" "u" "h" "t" "n" "i" "d" "a" "s"))
;;              :bind
;;              ([remap other-window] . switch-window))

(provide 'ace-window-conf)
;;; ace-window-conf.el ends here
