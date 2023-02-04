;;; Package: --- Summary
;;; Commentary:
;;; Code:

;; love this. doesn't work with magit. it never gets focus,
;; and its impossible to get focus in a helm window once it is lost.
;; (helm-posframe-enable)

;;(setq helm-posframe-width 110)

;;(setq helm-display-buffer-reuse-frame t)
;;(setq helm-display-function #'helm-display-buffer-popup-frame)

;;(define-key global-map [remap find-file] 'helm-find-files)
;;(define-key global-map [remap occur] 'helm-occur)
;;(define-key global-map [remap list-buffers] 'helm-buffers-list)
;;(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;;(define-key global-map [remap execute-extended-command] 'helm-M-x)
;;(define-key global-map [remap apropos-command] 'helm-apropos)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;;(global-set-key (kbd "C-c h") 'helm-command-prefix)
;;(global-unset-key (kbd "C-x c"))
;;(global-set-key (kbd "M-x") 'helm-M-x)
;;(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;(global-set-key (kbd "C-x b") 'helm-mini)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
;;(global-set-key (kbd "C-c h o") 'helm-occur)
;;(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;;; from http://tuhdo.github.io/helm-intro.html

(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
(add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "/tmp/helm-cfg.el") (delete-file "/tmp/helm-cfg.el"))))
(require 'helm)
;;(require 'helm-config)

;; turn on fuzzy matching
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(setq helm-locate-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)   ; make TAB work in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action)               ; list actions using C-z

;; turn on man page at point
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 40)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode t)
(semantic-mode 1)

;; for golden ratio. We dont want it interfering.
;; They don't get along...
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)


;; helm shell history for eshell.
(require 'helm-eshell)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)


;; browse keybinding descriptions
(require 'helm-descbinds)
(helm-descbinds-mode)


;; not sure I will like this. I can get fish completion in eshell one way or another...
(when (require 'helm-fish-completion nil 'noerror)
  (define-key shell-mode-map (kbd "<tab>") 'helm-fish-completion)
  (setq helm-esh-pcomplete-build-source-fn #'helm-fish-completion-make-eshell-source))

;; `fish-completion-mode' must be disabled.


(require 'helm-swoop)

(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direction.  'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line t)

;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)

;; If you would like to use migemo, enable helm's migemo feature
;;(helm-migemo-mode 1)


(helm-mode 0)
