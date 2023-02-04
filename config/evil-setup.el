(require 'evil)

(setq evil-shift-width 2)
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t)
(setq evil-complete-all-buffers nil)
(setq evil-cross-lines t)
;;(setq evil-want-fine-undo t)
;;(setq evil-regexp-search t)


;; Clear insert state bindings.
;; (setcdr evil-insert-state-map nil)

;; Set the initial evil state that certain major modes will be in.
(evil-set-initial-state 'cider-stacktrace-mode 'emacs)
(evil-set-initial-state 'cider-browse-ns-mode 'emacs)
(evil-set-initial-state 'cider-docview-mode 'emacs)
(evil-set-initial-state 'git-rebase-mode 'emacs)
(evil-set-initial-state 'magit-log-edit-mode 'emacs)
(evil-set-initial-state 'nav-mode 'emacs)
(evil-set-initial-state 'grep-mode 'emacs)
(evil-set-initial-state 'cider-connections-buffer-mode 'emacs)
(evil-set-initial-state 'ibuffer-mode 'normal)
(evil-set-initial-state 'mu4e:compose 'normal)

(require 'evil-surround)
(global-evil-surround-mode 1)

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
                           nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
        (delete-char -1)
        (set-buffer-modified-p modified)
        (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                                              (list evt))))))))

;; Don't wait for any other keys after escape is pressed.
(setq evil-esc-delay 0)

; BS-menu
(defadvice bs-mode (before bs-mode-override-keybindings activate)
  ;; use the standard bs bindings as a base
  (evil-make-overriding-map bs-mode-map 'normal t)
  (evil-define-key 'normal bs-mode-map "h" 'evil-backward-char)
  (evil-define-key 'normal bs-mode-map "q" 'bs-abort)
  (evil-define-key 'normal bs-mode-map "j" 'bs-down)
  (evil-define-key 'normal bs-mode-map "k" 'bs-up)
  (evil-define-key 'normal bs-mode-map "l" 'evil-forward-char)
  (evil-define-key 'normal bs-mode-map "RET" 'bs-select))

;; Change :q and :wq save and kill buffer instead of closing
;; the window.  Do the same for ZZ.

;; In Vim, q and zz kill the tab and the buffer.
;; emacs doesn't have tabs. does it make sense to kill the window?
;; I almost never want to get rid of a window I have.

(defun my-evil-save-and-close ()
  (interactive)
  (save-buffer)
  (kill-this-buffer))

;; map space and backspace do go down and up 10 lines at a time.
(defun evil-big-down()
  (interactive)
  (evil-next-line 10)
  (evil-scroll-line-to-center nil))

(defun evil-big-up()
  (interactive)
  (evil-previous-line 10)
  (evil-scroll-line-to-center nil))

(evil-ex-define-cmd "q[uit]" 'kill-this-buffer)
(evil-ex-define-cmd "wq" 'my-evil-save-and-close)
(define-key evil-normal-state-map "ZZ" 'my-evil-save-and-close)

;; very annoying. theoretically a nice way to page up and down.
;; (define-key evil-normal-state-map (kbd  "SPC") 'evil-big-down)
;; (define-key evil-normal-state-map (kbd "DEL") 'evil-big-up)

;; Make HJKL keys work in special buffers
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings occur-mode 'emacs)

;; Evil Keys
;; (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;; (define-key evil-normal-state-map ",w" 'save-buffer) ; save
;; (define-key evil-normal-state-map ",a" 'ack-and-a-half)
;; (define-key evil-normal-state-map ",g" 'magit-status)
;; (define-key evil-normal-state-map ",j" 'dired-jump)
;; (define-key evil-normal-state-map ",," 'evil-buffer)
;; (define-key evil-normal-state-map ",f" 'find-file)
;; (define-key evil-normal-state-map ",F" 'fiplr-find-file)
;; (define-key evil-normal-state-map ",b" 'bs-show)
;; (define-key evil-normal-state-map ",B" 'ibuffer)
;; (define-key evil-normal-state-map ",x" 'execute-extended-command)
;; (define-key evil-normal-state-map ",d" 'kill-this-buffer)
;; (define-key evil-normal-state-map ",q" 'kill-buffer-and-window)
(define-key evil-normal-state-map "U" 'undo-tree-redo)

;; ;; RSPEC, requires rspec-mode
;; (evil-declare-key 'normal ruby-mode-map (kbd ",ss") 'rspec-verify-single)
;; (evil-declare-key 'normal ruby-mode-map (kbd ",sv") 'rspec-verify)
;; (evil-declare-key 'normal ruby-mode-map (kbd ",sa") 'rspec-verify-all)
;; (evil-declare-key 'normal ruby-mode-map (kbd ",st") 'rspec-toggle-example-pendingness)
;; (evil-declare-key 'normal ruby-mode-map (kbd ",sg") 'rspec-toggle-spec-and-target)
;; (evil-declare-key 'normal rspec-mode-map (kbd ",ss") 'rspec-verify-single)
;; (evil-declare-key 'normal rspec-mode-map (kbd ",sv") 'rspec-verify)
;; (evil-declare-key 'normal rspec-mode-map (kbd ",sa") 'rspec-verify-all)
;; (evil-declare-key 'normal rspec-mode-map (kbd ",st") 'rspec-toggle-example-pendingness)
;; (evil-declare-key 'normal rspec-mode-map (kbd ",sg") 'rspec-toggle-spec-and-target)


(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;; undo
(setq evil-want-fine-undo t)


;; Different cursors for different states.
;; (setq evil-emacs-state-cursor '("red" box))
;; (setq evil-normal-state-cursor '("lawn green" box))
;; (setq evil-visual-state-cursor '("orange" box))
;; (setq evil-insert-state-cursor '("deep sky blue" bar))
;; (setq evil-replace-state-cursor '("red" bar))
;; (setq evil-operator-state-cursor '("red" hollow))
