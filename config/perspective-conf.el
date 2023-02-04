(require 'perspective)
(require 'persp-hydra)
;; (global-set-key (kbd "C-x C-b") 'persp-list-buffers)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))
(persp-mode)

(define-key perspective-map (kbd "h") #'hydra-persp/body)
(define-key perspective-map (kbd "H") #'hydra-persp2/body)

(require 'persp-hydra)

;; (hfj/define-layout "Tasks"
;;                    (let ((first-window (get-buffer-window)))
;;                      (cd "~/Documents/org")
;;                      (find-file "time-sheet.org")
;;                      (split-window-below)
;;                      (windmove-down)

;; (find-file "tasks.org")
;; (split-window-right)
;; (windmove-right)

;; (find-file "journal")
;; (select-window first-window)))

;; (hfj/define-layout "Elisp"
;;                    (let ((first-window (get-buffer-window)))
;;                      (cd '~/Arch-Setup/emacs-setup)
;;                      (find-file "readme.md")
;;                      (split-window-right)
;;                      (windmove-right)

;; (dired-jump)
;; (split-window-right)
;; (windmove-right)

;; (find-file "packages.el")
;; (select-window first-window)))

;; (hfj/define-layout "Xmonad"
;;                    (let ((first-window (get-buffer-window)))
;;                      (cd "~/Arch-Setup/xmonad-setup")
;;                      (find-file "readme.md")
;;                      (split-window-below)
;;                      (windmove-down)

;; (eshell)
;; (split-window-right)
;; (windmove-right)

;; (find-file "xmonad.hs")
;; (split-window-right)
;; (windmove-right)

;; (find-file "Makefile")
;; (select-window first-window)))

;; (hfj/define-layout "Comm"
;;                    (let ((first-window (get-buffer-window)))
;;                      (cd "~/Docmuments")

;; (eg/launch-discord)
;; (split-window-right)
;; (windmove-right)

;; ;;(browse-url "http://github.com/ericalinag")
;; (eg/launch-telegram)
;; ))

;; (hfj/define-layout "BD"
;;                    (let ((first-window (get-buffer-window)))
;;                      (cd "/home/BD")
;;                      (dired-jump)
;;                      (split-window-right)
;;                      (windmove-right)

;; (eg/launch-yacreader)
;; (split-window-right)))

(provide 'perspective-conf)
;;; perspective-conf.el ends here
