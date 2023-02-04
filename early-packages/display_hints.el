;; set the initial frame size
;;(setq initial-frame-alist '((top . 20) (left . 0) (width . 200) (height . 60)))

;;Display-Buffer-Alist controls window display choices.
                                        ; display buffer alist is not really working in 24.3 as far
                                        ; as I can tell. some settings do work, but not all. Waiting for 24.4
                                        ; there is a window test function in functions to play with this.
                                        ; setting to nil at the end.

(add-to-list 'display-buffer-alist
             '("\\*.*\\*"
               (display-buffer-pop-up-window
                )
               (reusable-frames)
               (window-height . 5)))

(add-to-list 'display-buffer-alist
             '("\\*shell\\*"
               (
                display-buffer-in-previous-window
                display-buffer-below-selected
                )
               (reusable-frames)
               (window-height . 15)
               (window-width . 60)))

(add-to-list 'display-buffer-alist
             '("\\*help\\*"
               (
                display-buffer-reuse-window
                display-buffer-in-previous-window
                display-buffer-below-selected
                display-buffer-pop-up-window)
               (reusable-frames)
               (window-height . 20)))

(add-to-list 'display-buffer-alist
             '("\\*cider\\*"
               (
                display-buffer-below-selected
                display-buffer-reuse-window
                display-buffer-in-previous-window
                display-buffer-pop-up-window )
               (reusable-frames)
               (window-height . 20)))

(setq display-buffer-alist nil)
(setq checkdoc-arguments-in-order-flag nil)
