;; Enable vertico
(require 'vertico)

(vertico-mode)

(require 'vertico-posframe)
(vertico-posframe-mode 1)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(require 'marginalia)

;; (use-package marginalia
;;              ;; Either bind `marginalia-cycle' globally or only in the minibuffer
;;              :bind (("M-A" . marginalia-cycle)
;;                     :map minibuffer-local-map
;;                     ("M-A" . marginalia-cycle))
;;)

(marginalia-mode)

;; Different scroll margin
;; (setq vertico-scroll-margin 0)

;; Show more candidates
(setq vertico-count 20)

;; Grow and shrink the Vertico minibuffer
;; (setq vertico-resize t)

;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
(setq vertico-cycle t)
