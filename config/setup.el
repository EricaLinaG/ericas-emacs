;;; setup.el --- hello                               -*- lexical-binding: t; -*-
;;; Commentary
;; Copyright (C) 2023

;; Author: ;; <ericalinagebhart@gmail.com>
;; Keywords:
;;

;;; Turn on Vi mode.
(evil-mode t)

;; Stupid Stallman, thinks two spaces come after a period.
(setq sentence-end-double-space nil)

(global-hl-line-mode 1)

(setq help-window-select t)

(setq vc-follow-symlinks t)

;;; Great for smaller screens.
(golden-ratio-mode)

;;; add cbr and cbz to archive mode in the automode alist.
(add-to-list 'auto-mode-alist '("\\.\\(cbr\\)\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.\\(cbz\\)\\'" . archive-mode))

;;(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Start up a shell.  Pick your poison.
;; see config/terminal-conf.el
;;
;; shell is perhaps the easiest, but limited.
;; multi-term is great once you get it working.
;; eshell is great if you want a lisp shell, not good for
;; big files, or streams.
;; term and ansi-term misbehave a lot in my experience.

;;(setq cb-shell-command 'shell)
;;(setq cb-shell-command 'multi-term)
(setq cb-shell-command 'eshell)
;;(setq cb-shell-command 'term)
;;(setq cb-shell-command 'ansi-term)

;;;squiggly-clojure.

;; I always want to see the end of my messages buffer.
(add-hook 'post-command-hook
          (lambda ()
            (let ((messages (get-buffer "*Messages*")))
              (unless (eq (current-buffer) messages)
                (with-current-buffer messages
                  (goto-char (point-max)))))))

;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

;; (add-hook 'cider-mode-hook
;;           (lambda () (setq next-error-function #'flycheck-next-error-function)))
