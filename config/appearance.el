;;; package --- Summary
;;; Commentary:
;;; Code:

;;make sure ansi colour character escapes are honoured
(require 'ansi-color)
(require 'all-the-icons)
(ansi-color-for-comint-mode-on)

(setq use-dialog-box nil)

;; (require 'mini-frame)

;; make it half as wide as the frame.
;; (custom-set-variables
;;  '(mini-frame-show-parameters
;;    '((top . 10)
;;      (width . 0.5)
;;      (left . 0.5))))

;; (mini-frame-mode)

;; posframe for transient popups like magit commands.
;; hope it works better than all the rest
(require 'transient-posframe)
(transient-posframe-mode)

(setq font-lock-maximum-decoration t
      color-theme-is-global t)

;; Line-wrapping
(set-default 'fill-column 72)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(75 . 50))
(add-to-list 'default-frame-alist '(alpha . (75 . 50)))

;;get rid of clutter, menus, scrollbars, etc.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;remove bells
(setq ring-bell-function 'ignore)

;; font settng functions
(require 'cl-lib)

;;(set-frame-font "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1")
;;(set-frame-font "-*-Iosevka-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1")
(set-frame-font "Iosevka term 12")  ; 12 is size, not height. height is more.

;;make fringe bigger
(if (fboundp 'fringe-mode)
    (fringe-mode 10))
