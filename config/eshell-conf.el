(require 'eshell)
;;;;;;;;;;;;;;;;;;;;;
;; ESHELL

;; (require 'em-tramp)

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(setq eshell-prompt-regexp "^[^#$]*[#$] ")

(setq eshell-history-size         10000
      eshell-buffer-maximum-lines 10000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t)

;; Save command history when commands are entered
(add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

;; ;; Bind some useful keys for evil-mode
;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
;; (evil-normalize-keymaps)

;; ;; (require 'eshell-git-prompt)
;; ;; (eshell-git-prompt-use-theme 'powerline)

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "vim" "vi" "neovim")))


(load "em-hist")           ; So the history vars are defined

(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)

(defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))


(setq eshell-prefer-lisp-functions nil)
(setq eshell-prefer-lisp-variables nil)
(setq password-cache nil) ; enable password caching
(setq password-cache-expiry 3600) ; for one hour (time in secs)

(setq eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil
      eshell-destroy-buffer-when-process-dies t)

;; (require 'eshell-prompt-extras)

;; (with-eval-after-load "esh-opt"
;;   (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;   (setq eshell-highlight-prompt nil
;;         eshell-prompt-function 'epe-theme-lambda))

(defun th/eshell-here (arg)
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive "P")
  (let* ((root-buffer (window-buffer (selected-window)))
         (parent (if (buffer-file-name root-buffer)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (shellname (concat "*eshell: " parent "*"))
         (shell-buffer (get-buffer shellname)))

    (when (not arg)
      (split-window-below -20)
      (windmove-down))

    (if shell-buffer
        ;; If we're already in eshell, go back to where we were.
        ;; Otherwise, switch to it said eshell.
        (if (string= (buffer-name root-buffer)
                     (buffer-name shell-buffer))
            (previous-buffer)
          (switch-to-buffer shell-buffer))

      ;; It doesn't exist yet - let's create it!
      (eshell "new")
      (rename-buffer shellname))))

(defun th/eshell-dired ()
  (interactive)
  (th/eshell-here 1))

(global-set-key (kbd "C-x e") #'th/eshell-here)

(setenv "PAGER" "cat")
(defalias 'e 'find-file)

;;(global-set-key (kbd "C-x M-e") (th/mode-menu eshell-mode))

(defun th/eshell-toggle-sudo ()
  "Add sudo at the beginning of the current line.

If already there, remove it."
  (interactive)
  (save-excursion
    (eshell-bol)
    (if (looking-at "sudo")
        (delete-forward-char 5)
      (insert "sudo "))))

;; (bind-key "C-c C-s" #'th/eshell-toggle-sudo eshell-mode-map)

(defun eshell-next-prompt (n)
  "Move to end of Nth next prompt in the buffer. See `eshell-prompt-regexp'."
  (interactive "p")
  (re-search-forward eshell-prompt-regexp nil t n)
  (when eshell-highlight-prompt
    (while (not (get-text-property (line-beginning-position) 'read-only) )
      (re-search-forward eshell-prompt-regexp nil t n)))
  (eshell-skip-prompt))

(defun eshell-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer. See `eshell-prompt-regexp'."
  (interactive "p")
  (backward-char)
  (eshell-next-prompt (- n)))

(defun eshell-insert-history ()
  "Displays the eshell history to select and insert back into your eshell."
  (interactive)
  (insert (ivy-completing-read "Eshell history: "
                               (delete-dups
                                (ring-elements eshell-history-ring)))))

(add-hook
 'eshell-mode-hook
 (lambda ()
   (define-key eshell-mode-map (kbd "M-P") 'eshell-previous-prompt)
   (define-key eshell-mode-map (kbd "M-N") 'eshell-next-prompt)
   (define-key eshell-mode-map (kbd "M-r") 'eshell-insert-history)
   (define-key eshell-mode-map (kbd "M-s") 'th/eshell-toggle-sudo)))

(provide 'th-eshell)






;;; ---- path manipulation

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat "["
                          (if (and (> (length git-output) 0)
                                   (not (string-match "fatal:.*" git-output)))
                  (substring git-output 0 -1)
                "no branch")
              "]") 'face `(:foreground "#A09000"))
      )))


;; Prompts - pick what you want at the bottom.
(defun simple-time-prompt ()
  (setq eshell-prompt-function
        (lambda ()
          (concat (format-time-string "%Y-%m-%d %H:%M" (current-time))
                  (if (= (user-uid) 0) " # " " $ ")))))

(defun simple-path-prompt ()
  (setq eshell-prompt-function
        (lambda nil
          (concat
           (eshell/pwd)
           " $ "))))

; A simple colored prompt.
(defun simple-prompt ()
  (setq eshell-prompt-function (lambda nil
                                 (concat
                                  (propertize (eshell/pwd) 'face `(:foreground "blue"))
                                  (propertize " $ " 'face `(:foreground "green")))))
  (setq eshell-highlight-prompt nil))

; a simple prompt, limiting the path to the last three entries
; Adding the git branch if it can find one.
(defun short-path-prompt ()
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize ((lambda (p-lst)
                          (if (> (length p-lst) 3)
                              (concat
                               (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                                          (substring elm 0 1)))
                                          (butlast p-lst 3)
                                          "/")
                               "/"
                               (mapconcat (lambda (elm) elm)
                                          (last p-lst 3)
                                          "/"))
                            (mapconcat (lambda (elm) elm)
                                       p-lst
                                       "/")))
                        (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "yellow"))
           (or (curr-dir-git-branch-string (eshell/pwd)))
           (propertize "# " 'face 'default))))

  (setq eshell-highlight-prompt nil))


;; A two line prompt. path, date and time on one line.
;; userid and git branch at the prompt.
 (defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))

  (defun shk-eshell-prompt ()
    (let ((header-bg "#100050"))
      (concat
       (with-face (concat (eshell/pwd) " ") :foreground "#008888" :background header-bg)
       (with-face (format-time-string "--------------- (%Y-%m-%d %H:%M)"
                                      (current-time)) :background header-bg :foreground "#888")
       (with-face
        (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) "")
        :background header-bg)
       (with-face "\n" :background header-bg)
       (with-face user-login-name :foreground "#909090")
       (or (curr-dir-git-branch-string (eshell/pwd)))
       (if (= (user-uid) 0)
           (with-face " #" :foreground "red")
         " $")
       " ")))

(defun fancy-2-line-prompt ()
  (setq eshell-prompt-function 'shk-eshell-prompt)
  (setq eshell-highlight-prompt nil))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; choose your prompt. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;(fancy-2-line-prompt)
                                        ;(simple-time-prompt)
                                        ;(simple-path-prompt)
                                        ;(simple-prompt)
                                        ;(short-path-prompt)
