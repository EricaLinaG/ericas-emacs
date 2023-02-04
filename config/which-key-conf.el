(require 'which-key)

(which-key-mode)
(which-key-posframe-mode)

;;(which-key-setup-side-window-right)
;;(which-key-setup-minibuffer)


;; (use-package! which-key
;;   :hook (doom-first-input . which-key-mode)
;;   :init

(setq which-key-sort-order #'which-key-key-order-alpha
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-frame-max-height 20
      which-key-max-display-columns 6
      which-key-min-display-lines 6
      ;;which-key-side-window-slot -10
      )

;;   :config

;; (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
;; (add-hook! 'doom-before-reload-hook
;;            (defun doom-reset-which-key-replacements-h ()
;;              (setq which-key-replacement-alist (get 'which-key-replacement-alist 'initial-value))))

;;   ;; general improvements to which-key readability
;;   (which-key-setup-side-window-bottom)
;;   (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

;; (which-key-add-key-based-replacements doom-leader-key "<leader>")
;; (which-key-add-key-based-replacements doom-localleader-key "<localleader>"))


;; Additional settings

;;(setq which-key-popup-type 'minibuffer)

;;(setq which-key-popup-type 'side-window)

;; location of which-key window. valid values: top, bottom, left, right,
;; or a list of any of the two. If it's a list, which-key will always try
;; the first location first. It will go to the second location if there is
;; not enough room to display any keys in the first location
;;(setq which-key-side-window-location 'bottom)

;; max width of which-key window, when displayed at left or right.
;; valid values: number of columns (integer), or percentage out of current
;; frame's width (float larger than 0 and smaller than 1)
;;(setq which-key-side-window-max-width 0.33)

;; max height of which-key window, when displayed at top or bottom.
;; valid values: number of lines (integer), or percentage out of current
;; frame's height (float larger than 0 and smaller than 1)
;;(setq which-key-side-window-max-height 0.25)


;;(setq which-key-popup-type 'frame)
;; max width of which-key frame: number of columns (an integer)
;; (setq which-key-frame-max-width 10)
