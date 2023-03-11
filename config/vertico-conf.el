;;; Vertico-conf --- Vertico needs some settings
;;; Commentary:
;;; Code:

;; only turn this on if we arent in a terminal.
;; It behaves extra badly then.
(when (display-graphic-p)
  (require 'vertico-posframe)
  (vertico-posframe-mode 1))

(require 'vertico)
(require 'consult)

(vertico-mode)

(consult-customize consult--source-buffer :hidden t :default nil)
(add-to-list 'consult-buffer-sources persp-consult-source)



(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(require 'marginalia)

(setq completion-styles '(orderless))

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
