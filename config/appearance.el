;;; package --- Summary
;;; Commentary:
;;; Code:

;;make sure ansi colour character escapes are honoured
(require 'ansi-color)
(require 'all-the-icons)
(ansi-color-for-comint-mode-on)

(setq use-dialog-box nil)

;; Over ride the code block background colors. They were
;; much to bright.
(setq modus-vivendi-tinted-palette-overrides
      '(
        (bg-red-nuanced     "#250004")
        (bg-green-nuanced   "#002718")
        (bg-yellow-nuanced  "#1c0f00")
        (bg-blue-nuanced    "#131c2d")
                                        ;(bg-magenta-nuanced "#1f032f")
        (bg-magenta-nuanced "#1f000f")
        (bg-cyan-nuanced    "#04253f")
        (bg-hl-line         "#484d67")
        (bg-blue-subtle     "#24262c")

        ;; (bg-red-intense     "#9d1f1f")
        ;; (bg-green-intense   "#2f822f")
        ;; (bg-yellow-intense  "#7a6100")
        ;; (bg-blue-intense    "#164050")
        ;; (bg-magenta-intense "#7030af")
        ;; (bg-cyan-intense    "#2266ae")

        ;; (bg-red-subtle      "#620f2a")
        ;; (bg-green-subtle    "#00422a")
        ;; (bg-yellow-subtle   "#4a4000")
        ;; (bg-magenta-subtle  "#552f5f")
        ;; (bg-cyan-subtle     "#004065")

        ;; (fg-main "#333333")
        ;; (comment red-faint)
        ;; (keyword cyan-cooler)

        ))


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
(set-frame-parameter (selected-frame) 'alpha '(85 . 70))
(add-to-list 'default-frame-alist '(alpha . (85 . 70)))

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
(set-frame-font "Iosevka term 10")  ; 12 is size, not height. height is more.

;;make fringe bigger
(if (fboundp 'fringe-mode)
    (fringe-mode 10))
