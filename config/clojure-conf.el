;; anonymous function longhand.
(require 'flycheck-pos-tip)

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

;; anonymous function shorthand.
(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

;; sets
(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

;; turn (partial into (P  ;; from Jay Fields.
(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(partial\\)[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "Ƥ")
                               nil))))))

;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("(\\(->>\\)[[:space:]]"
;;                      (0 (progn (compose-region (match-beginning 1)
;;                                                (match-end 1) "→→")
;;                                nil))))))

;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("(\\(->\\)[[:space:]]"
;;                      (0 (progn (compose-region (match-beginning 1)
;;                                                (match-end 1) "→")
;;                                nil))))))

;;this is the new way to do this.... if only it worked.
(global-prettify-symbols-mode +1)

;; these do work but why bother.
(add-hook 'clojure-mode-hook
          (lambda ()
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)
                                        ;→
                                        ;▶
                                        ;▷
                                        ;>
            ;;(push '("->" . ?→) prettify-symbols-alist)

            ;; (push '("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
            ;;                       (Bl . Bl) ?→ (Bc . Br) ?→)) prettify-symbols-alist)

            ;;(push '("->" . ?→) prettify-symbols-alist)
            ;;(push '("->>" . (?→ ?→)) prettify-symbols-alist)
            ;;(push '("->" . ?➤) prettify-symbols-alist)
            ;;(push '("->>" . (?➤ (Bc . Bc) ?➤)) prettify-symbols-alist)

            ;;(push '("->" . (?\s (Br . Bl) ?\s (Bc . Bc) ?🠊)) prettify-symbols-alist)
            ;;(push '("->" . (?\s (Br . Bl) ?\s (Bc . Bc) ?🠊)) prettify-symbols-alist)
            ;;(push '("->" . (?- (Br . Bc) ?- (Br . Bc) ?>)) prettify-symbols-alist)

            ;; (push '("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
            ;;                       (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
            ;;                       (Bc . Bl) ?- (Br . Br) ?>)) prettify-symbols-alist)
            ))

;; ;; this almost works. The above works better.
;; (add-hook 'clojure-mode-hook
;;           (lambda ()
;;             (push '("\\(fn\\[\[[:space:]]" . ?λ)  prettify-symbols-alist)
;;             (push '("\(partial" .  ?Ƥ) prettify-symbols-alist)
;;             (push '("\#{" .  ?∈) prettify-symbols-alist)
;;             (push '("\#(" . ?ƒ)  prettify-symbols-alist)))


;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("(\\(sw?\\)[[:space:]]")
;;                     0 font-lock-builtin-face)))

(require 'clojure-mode)

;; extra font locking is imprecise, but nice.
(require 'clojure-mode-extra-font-locking)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq buffer-save-without-query t)))

;;Treat hyphens as a word character when transposing word
(defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
  (let ((st (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st))

(defun transpose-words-with-hyphens (arg)
  "Treat hyphens as a word character when transposing words"
  (interactive "*p")
  (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
    (transpose-words arg)))

(define-key clojure-mode-map (kbd "M-t") 'transpose-words-with-hyphens)

(setq auto-mode-alist (append '(("\\.cljs$" . clojure-mode))
                              auto-mode-alist))

(dolist (x '(scheme emacs-lisp lisp clojure hy python sh))
  ;;(add-hook (intern (concat (symbol-name x) "-mode-hook")) 'enable-paredit-mode)
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

;; Pull in the awesome clj-refactor lib by magnars
(require 'clj-refactor)
(require 'flycheck-clj-kondo)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")))

(define-key clojure-mode-map (kbd "C-:") 'cljr-cycle-stringlike)
(define-key clojure-mode-map (kbd "C->") 'cljr-cycle-coll)

(defun warn-when-cider-not-connected ()
  (interactive)
  (message "nREPL server not connected. Run M-x cider or M-x cider-jack-in to connect."))

(define-key clojure-mode-map (kbd "C-M-x")   'warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-x C-e") 'warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-e") 'warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-l") 'warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-r") 'warn-when-cider-not-connected)


;;; this is nice when it works. Currently it is interfering basic functionality
;;; of loading code.

;;; squiggly-clojure.

(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)


;; this one works.

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; OR This one.

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; instead of flycheck-pos-tip use flycheck-tip.
;; (require 'flycheck-tip)
;; (flycheck-tip-use-timer 'verbose)

;; (add-hook 'cider-mode-hook
;;           (lambda () (setq next-error-function #'flycheck-next-error-function)))
