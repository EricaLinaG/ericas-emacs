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
(require 'cape)
;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.

(require 'corfu)
(global-corfu-mode)

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

;; vertico directory is unavailable.

;; (require vertico-directory)
;; ;; Configure directory extension.
;; (define-key vertico-map (kbd "RET") 'vertico-directory-enter)
;; (define-key vertico-map (kbd "DEL") 'vertico-directory-delete-char)
;; (define-key vertico-map (kbd "M-DEL") 'vertico-directory-delete-word)

;; ;; Tidy shadowed file names
;; (add-hook 'rfn-eshadow-update-overlay vertico-directory-tidy)

;; Add extensions

;; capf and corfu. not done adding.

;; ;; Bind dedicated completion commands
;; ;; Alternative prefix keys: C-c p, M-p, M-+, ...
;; ((global-set-key "C-c p p" 'completion-at-point) ;; capf
;;  (global-set-key "C-c p t" 'complete-tag)        ;; etags
;;  (global-set-key "C-c p d" 'cape-dabbrev)        ;; or dabbrev-completion
;;  (global-set-key "C-c p h" 'cape-history)
;;  (global-set-key "C-c p f" 'cape-file)
;;  (global-set-key "C-c p k" 'cape-keyword)
;;  (global-set-key "C-c p s" 'cape-symbol)
;;  (global-set-key "C-c p a" 'cape-abbrev)
;;  (global-set-key "C-c p l" 'cape-line)
;;  (global-set-key "C-c p w" 'cape-dict)
;;  (global-set-key "C-c p \\" 'cape-tex)
;;  (global-set-key "C-c p _" 'cape-tex)
;;  (global-set-key "C-c p ^" 'cape-tex)
;;  (global-set-key "C-c p &" 'cape-sgml)
;;  (global-set-key "C-c p r" 'cape-rfc1345))

;; ;; Add `completion-at-point-functions', used by `completion-at-point'.
;; ;; NOTE: The order matters!
;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;; (add-to-list 'completion-at-point-functions #'cape-file)
;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;; ;;(add-to-list 'completion-at-point-functions #'cape-history)
;; ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
;; ;;(add-to-list 'completion-at-point-functions #'cape-tex)
;; ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
;; ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
;; ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
;; ;;(add-to-list 'completion-at-point-functions #'cape-dict)
;; ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
;; ;;(add-to-list 'completion-at-point-functions #'cape-line)
