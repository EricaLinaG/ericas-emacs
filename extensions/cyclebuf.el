;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cycle-bufs
;;
;; Purpose: cycle and create buffers contextually. Cycle through shell
;;          cider, ipython buffers. or File buffers, or *---* buffers.
;;
;;          cycle through buffers similar to the current buffer.
;;
;;          find the current window for a group, cycle through that
;;          group in that window.
;;
;;          This iteration of the code is a layer on top of bs.el
;;          Buffer Select which currently ships with emacs.
;;
;;   This could use a bit more refactoring. I'm wondering if it would
;;   be better to look for modes than buffer names. It seems likely.
;;   it's getting better slowly. Some configurations aren't quite right.
;;
;; Author: Eric Gebhart  e.a.gebhart@gmail.com
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bs)
(require 'cider)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cb-shell-command 'eshell
  "Pick the shell you want cycle-shells to invoke.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Buffer groups. These are groups of modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Change to defvars.



(setq cb-buffer-groups '())

(add-to-list 'cb-buffer-groups '("shells"
                                 shell-mode
                                 eshell-mode
                                 term-mode
                                 cider-repl-mode
                                 inferior-python-mode
                                 inferior-ruby-mode))

(add-to-list 'cb-buffer-groups '("shell-only"
                                 shell-mode
                                 eshell-mode
                                 term-mode))

(add-to-list 'cb-buffer-groups '("cider-only"
                                 cider-repl-mode))

(add-to-list 'cb-buffer-groups '("cb-show"
                                 bs-mode
                                 ibuffer-mode))


(add-to-list 'cb-buffer-groups '("dired"
                                 dired-mode))

(setq shell-buffer-modes '(shell-mode
                           eshell-mode
                           term-mode
                           cider-mode
                           inferior-python-mode
                           inferior-ruby-mode))

(setq shell-only-buffer-modes '(shell-mode
                                eshell-mode
                                term-mode))


;;;;;;;;;;;;;;; regexes. Could probably be mostly eliminated.

(setq emacs-buffer-regexps '("^\\*.*"))

(setq emacs-buffer-subset-regexps '("^\\*compile-log.*"
                                    "^\\*scratch.*"
                                    "^\\*messages.*"
                                    "^\\*grep.*"
                                    "^\\*info.*"))

(setq shell-buffer-regexps '("\\*eshell.*"
                             "\\*shell.*"
                             "\\*Python.*"
                             "\\*cider.*"
                             "\\*terminal.*"
                             "\\*ansi-term.*"))

(setq shell-only-buffer-regexps '("\*shell.*"
                                  "\*terminal.*"
                                  "\*ansi-term.*"
                                  "\*eshell.*"))

;;;;;;;;;;;;;;; CB show modes for cycling through configurations.

(setq cb-show-modes '(;"ibuffer"
                      ;"eproject"
                      "files"
                      "dired"
                      "*buffers"
                      "*shells"
                      "all"))

(setq cb-current-show-mode "all")

;; Some of these configurations are not %100 the emacs and intern
;; need some work. But they do mostly work. Just not as cleanly as I'd like.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; BS Configurations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; this is the default as set in bs.el.
(setq bs-configurations
  '(("all" nil nil nil nil nil)
    ("files" nil nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
    ("files-and-scratch" "^\\*scratch\\*" nil nil bs-visits-non-file
     bs-sort-buffer-interns-are-last)
    ("all-intern-last" nil nil nil nil bs-sort-buffer-interns-are-last)))
;"*List of all configurations you can use in the Buffer Selection Menu.
;A configuration describes which buffers appear in Buffer Selection Menu
;and describes the order of buffers.  A configuration is a list with
;six elements.  The first element is a string and describes the configuration.
;The following five elements represent the values for Buffer Selection Menu
;configurations variables `bs-dont-show-regexp', `bs-dont-show-function',
;`bs-must-show-regexp', `bs-must-show-function' and `bs-buffer-sort-function'.
;By setting these variables you define a configuration."
;  :group 'bs-appearence
;  :type '(repeat sexp)



(add-to-list 'bs-configurations
             '("all" nil nil nil nil nil))

(add-to-list 'bs-configurations
             '("*buffers"
               nil
               nil
                nil
                cb-emacs-buffers-filter
                nil))

(add-to-list 'bs-configurations
             '("*buffers-subset"
               nil
               (lambda (buf)
                 (or (cb-name-match (buffer-name buf) shell-buffer-regexps)
                     (bs-visits-non-file buf)
                     (not (cb-shell-mode-filter buf)) ))
               nil cb-emacs-subset-buffers nil))

(add-to-list 'bs-configurations
                 '("channels" nil nil "^[^#]" nil nil))

(add-to-list 'bs-configurations
                 '("targets" nil nil nil
                   (lambda (buf)
                      (with-current-buffer buf
                        (not (erc-default-target)))) nil))

(add-to-list 'bs-configurations
                 '("SQL" nil nil nil
                   (lambda (buf)
                      (with-current-buffer buf
                         (not (memq major-mode
                                    '(sql-interactive-mode sql-mode))))) nil))

(add-to-list 'bs-configurations
                 '("dired" nil nil nil
                   (lambda (buf)
                     (with-current-buffer buf
                       (not (eq major-mode 'dired-mode)))) nil))

(add-to-list 'bs-configurations
                 '("shells" nil nil nil
                   cb-shell-mode-filter
                    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Configuration Helpers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ToDo - clean up this mess...

(defun gettest ()
  "Test a get."
  (interactive)
  (message "%s" (cb-get-buffer-group-modes "shells")))

(defun cb-get-buffer-group-modes (name)
  "Buffer group modes for NAME."
  (cdr (assoc name cb-buffer-groups)))

(defun test-name-match ()
  "Test a name match."
  (interactive)
  (message "%s" (cb-name-match (buffer-name) emacs-buffer-subset-regexps)))

(defun cb-emacs-subset-buffers (buf)
  "Emacs subset buffers for BUF."
  (cb-name-match (buffer-name) emacs-buffer-subset-regexps))

(defun cb-emacs-buffers (buf)
  "Emacs buffers for BUF."
  (cb-name-match (buffer-name) emacs-buffer-regexps))

(defun cb-name-match (name regexps)
  "Return non-nil if buffer NAME is a match one of the `REGEXPS'."
  (let ((rl (delete "" regexps)))
    (while (and rl (car rl) (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (not (null rl))))

(defun test-emacs-buffer-filter ()
  "Test Emacs buffer filter."
  (interactive)
  (message "%s" (cb-emacs-buffers-filter (current-buffer))))

(defun cb-emacs-buffers-filter (buf)
  "Define Emacs buffer filter for BUF."
  (cb-*-buffers-filter buf emacs-buffer-regexps shell-buffer-regexps))

(defun cb-*-buffers-filter (buf match-regexps ignore-regexps)
  "Filter BUF with MATCH-REGEXPS and IGNORE-REGEXPS."
  (cond ((cb-name-match (buffer-name buf) ignore-regexps) t)
        ((not (bs-visits-non-file buf)) t)
        ((cb-mode-match buf (cb-get-buffer-group-modes "dired")) t)
        ((cb-name-match (buffer-name buf) match-regexps) nil)))

(defmacro cb-mode-filter (modes)
  "Macro to filter by MODES."
  `(lambda (buf)
     (with-current-buffer buf
       (not (memq major-mode ,modes)))))

(defun cb-no-shell-mode-filter (buf)
  "Filter BUF by excluding SHELL MODES."
  (not (cb-shell-mode-filter buf)))

(defun cb-shell-mode-filter (buf)
  "FILTER BUF by shell modes."
  (with-current-buffer buf
    (not (cb-mode-match buf (cb-get-buffer-group-modes "shells")))))

(defun cb-show-mode-filter (buf)
  "Filter BUF by CB-SHOW modes."
  (with-current-buffer buf
    (not (cb-mode-match buf (cb-get-buffer-group-modes "cb-show")))))

(defun testmodematch ()
  "Test the mode match."
  (interactive)
  (message "%s" (cb-mode-match (current-buffer) (cb-get-buffer-group-modes "cb-show"))))


;; (defun cb-mode-match(buf modes)
;;   "Check if the buf matches a major-mode."
;;   (memq (buffer-local-value 'major-mode buf) modes))

(defun cb-mode-match (buf modes)
  "Return non-nil if BUF name is a match one of the `regexps' MODES."
  (let ((rl (delete "" modes)))
    (while (and rl (car rl) (not (equal (buffer-local-value 'major-mode buf) (car rl))))
      (setq rl (cdr rl)))
    (not (null rl))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Core buffer and window manipulation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun cb-get-buffers-window (buffers regexps)
  "Find a window with a buffer in BUFFERS where the name is a match one of the REGEXPS."
  (if (cb-name-match (car buffers) regexps)
      (car (get-buffer-window-list (car buffers)))
    (if (cdr buffers)
        (cb-get-buffers-window-mode (cdr buffers) regexps)
      nil)))

(defun cb-get-buffers-window-mode (buffers modes)
  "Find a window with a buffer in BUFFERS and the mode is a match one of the MODES."
  (if (and (not (null buffers))  (cb-mode-match (car buffers) modes))
      (car (get-buffer-window-list (car buffers)))
    (if (cdr buffers)
        (cb-get-buffers-window-mode (cdr buffers) modes)
      nil)))

(defun cb-get-buffers-window-func (buffers func)
  "Find a window from BUFFERS where the mode is a matche one of the modes in FUNC."
  (if (and (not (null buffers))  (funcall func  (car buffers)))
      (let ((win (car (get-buffer-window-list (car buffers)))))
        (if (null win)
            (if (cdr buffers)
                (cb-get-buffers-window-func (cdr buffers) func)
              nil)
          win))))

(defun cb-find-window (bs-config regexps)
  "Find and select a window from BS-CONFIG that has a buffer in REGEXPS.
I.E, From the bs buffer list that matches one of the buffer regex patterns.
return t if current window was selected."
    ;setup bs with the right configuration.
    (setq bs-cycle-configuration-name bs-config)
    ; get the buffer-list, and look for an open window.
    ; if we have one select it and return t if it's the one we had.
    (let ((bufs (bs-buffer-list (buffer-list)))
          (current (selected-window)))
      (let  ((win (cb-get-buffers-window bufs regexps)))
        (when (not (null win))
          (select-window win))
        (equal (selected-window) current))))

(defun cb-find-window-with-mode (bs-config cb-buffer-group)
  "Find a window buffer in CB-BUFFER-GROUP from the BS-CONFIG list.
That matches one of the buffer regex patterns.  Return t if current window was selected."
  (bs-set-configuration bs-config)
  (setq bs-default-configuration bs-current-configuration)
  (setq bs-cycle-configuration-name bs-config)
  (let ((modes (cb-get-buffer-group-modes cb-buffer-group))
        (bufs (bs-buffer-list (buffer-list))))
    (cb-get-buffers-window-mode bufs modes)))

(defun cb-find-any-window-with-mode (cb-buffer-group)
  "Find a window in CB-BUFFER-GROUP.
That has a buffer from the bs buffer list that matches one of the buffer
modes in the group.  Return t if current window was selected."
  (let ((modes (cb-get-buffer-group-modes cb-buffer-group))
        (bufs  (buffer-list)))
    (cb-get-buffers-window-mode bufs modes)))

(defun cb-get-any-window-with-mode (bs-config cb-buffer-group)
  "Find and select any window from BS-CONFIG in CB-BUFFER-GROUP.
Including current by mode."
  (let  ((win (if (null bs-config)
                  (cb-find-any-window-with-mode cb-buffer-group)
                (cb-find-window-with-mode bs-config cb-buffer-group))))
    (when (not (null win))
      (select-window win))
    win))

(defun cb-find-window-with-func (bs-config func)
  "Find a window that has a buffer from the BS-CONFIG list using FUNC.
That matches one of the buffer regex patterns.  Return t if current window was selected."
  (bs-set-configuration bs-config)
  (setq bs-default-configuration bs-current-configuration)
  (setq bs-cycle-configuration-name bs-config)
  (let ((bufs (bs-buffer-list (buffer-list))))
    (cb-get-buffers-window-func bufs func)))

(defun cb-find-any-window-with-func (func)
  "Find a window with FUNC.
That has a buffer from the bs buffer list that matches one of the buffer
modes in the group.  Return t if current window was selected."
  (let ((bufs  (buffer-list)))
    (cb-get-buffers-window-func bufs func)))

(defun cb-get-any-window-with-func (bs-config func)
  "Find and select any window from BS-CONFIG including current by FUNC."
  (let  ((win (if (null bs-config)
                  (cb-find-any-window-with-func func)
                (cb-find-window-with-func bs-config func))))
    (when (not (null win))
      (select-window win))
    win))

(defun cb-get-window-with-mode (bs-config buffer-group)
  "Get a window from BS-CONFIG with mode from BUFFER-GROUP.
Return t if it is the current window."
  (let ((current (selected-window))
        (win (cb-find-window-with-mode bs-config buffer-group)))
    (when (not (null win))
      (select-window win))
    (equal  (selected-window) current)))

(defun cb-find-buffer-mode (bufferlist modes)
  "Look for a buffer in BUFFERLIST for a match to a list of MODES."
  (cond
   ;; first do-again-test: stop-condition
   ((not bufferlist) nil)

   ;; found it. return non-nil
   ((cb-mode-match (car bufferlist) modes) 1)
   ;;   recursively call shorter list with next-step expression
   (t  (cb-find-buffer-mode (cdr bufferlist) modes))))

(defun cb-find-buffer-match (bufferlist regexps)
  "Look for a buffer in BUFFERLIST for a match to a list of REGEXPS."
  (cond
   ;; first do-again-test: stop-condition
   ((not bufferlist) nil)

   ;; found it. return non-nil
   ((cb-name-match (car bufferlist) regexps) 1)
   ;;   recursively call shorter list with next-step expression
   (t  (cb-find-buffer-match (cdr bufferlist) regexps))))

(defun cb-find-mode-buffer (configuration buffer-group)
  "Find a buffer from CONFIGURATION in a BUFFER-GROUP."
  (bs-set-configuration configuration)
  (setq bs-cycle-configuration-name configuration)
  (let ((modes (cb-get-buffer-group-modes buffer-group))
        (buffers (bs-buffer-list (buffer-list))))
    (cb-find-buffer-mode buffers modes)))

(defun cb-find-buffer-with-mode (bufferlist buffer-group)
  "Find a buffer in BUFFERLIST for a match in the BUFFER-GROUP modes.
For when the two lists may be different."
  (let ((modes (cb-get-buffer-group-modes buffer-group))
        (bufs (bs-buffer-list (buffer-list)))
        (current (selected-window)))
    (cond
     ((cb-mode-match (car bufferlist) (cb-get-buffer-group-modes buffer-group)) 1)
     ;;   recursively call shorter list with next-step expression
     ((cdr bufferlist) (cb-find-buffer-with-mode (cdr bufferlist) buffer-group))
     (t nil))))

(defun cb-next-buffer (configuration)
  "Generic next buffer based on a CONFIGURATION."
  (setq bs-cycle-configuration-name configuration)
  (bs-cycle-next))

(defun cb-prev-buffer (configuration)
  "Generic previous buffer based on CONFIGURATION."
  (setq bs-cycle-configuration-name configuration)
  (bs-cycle-previous))

(defun cb-next-buffer-group-window (bs-config buffer-group)
  "Next window from BUFFER-GROUP.
if possible.  Otherwise just get the next BS-CONFIG buffer.
In the current window."
  (interactive)
  (setq bs-cycle-configuration-name bs-config)
  (when  (cb-get-window-with-mode bs-config buffer-group)
    (bs-cycle-next)))

;;;;;;;;;; miscellaneous.

(defun cb-switch-to-previous-buffer ()
  "Switch back to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun cb-swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; shell, cider and dired functions

(defun cb-next-shell-window ()
  "Next Shell window in buffer list."
  (interactive)
  (cb-get-shell) ; create a new shell if we don't have one.
  (cb-next-buffer-group-window "shells" "shells"))

(defun cb-next-dired-window ()
  "Move to dired mode window if possible.  Call dired if no windows visible.
Give next dired buffer as needed."
  (interactive)
  (let ((current (selected-window))
        (win (cb-get-any-window-with-mode "dired" "dired")))
    (if (null win)
        (call-interactively 'ido-dired)
      (when (equal (selected-window) current)
        (cb-next-dired-buffer)))))

(defun cb-next-files-window ()
  "Move to file window if possible.  Give next files buffer as needed."
  (interactive)
  (let ((current (selected-window))
        (win (cb-get-any-window-with-func
              "files"
              (lambda (buf) (not (bs-visits-non-file buf))))))
    (if (null win)
        (cb-next-files-buffer)
      (when (equal (selected-window) current)
        (cb-next-files-buffer)))))

(defun cb-get-shell ()
  "Look for a shell buffer and create one if not found."
  (interactive)
  (when (not (cb-find-buffer-match
              (mapcar (function buffer-name)
                      (buffer-list))
              shell-only-buffer-regexps))
    ;; start a shell if a primary shell doesn't exist.
    ;; shell, eshell, ansi-term
    (funcall cb-shell-command)))

(defun cb-get-cider()
  "Look for a cider repl buffer and create one if not found."
  (interactive)
  (when (null (cb-find-buffer-with-mode (buffer-list) "cider-only"))
    ;; start a shell if a primary shell doesn't exist.
    (cider-jack-in)))

(defun cb-next-cider-window()
  "Next cider repl or create a new one.
Cycle through all shells and Repls.  Try to do it in a window
already inhabited by one of the shell/REPL buffers."
  (interactive)
  (cb-get-cider) ; create a REPL if we don't have one.
  (cb-next-shell-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; NEXT and PREVIOUS BUFFER wrappers.

(defun cb-previous-dired-buffer ()
  "Previous dired buffer."
  (interactive)
  (cb-prev-buffer "dired "))

(defun cb-next-dired-buffer ()
  "Next dired buffer."
  (interactive)
  (cb-next-buffer "dired "))

(defun cb-previous-shell-buffer ()
  "Previous shell or repl buffer."
  (interactive)
  (cb-prev-buffer "shells"))

(defun cb-next-shell-buffer ()
  "Next shell or repl buffer."
  (interactive)
  (cb-next-buffer "shells"))

(defun cb-previous-emacs-buffer ()
  "Previous *buffer*."
  (interactive)
  (cb-prev-buffer "*buffers"))

(defun cb-next-emacs-buffer ()
  "Next *buffer*."
  (interactive)
  (cb-next-buffer "*buffers"))

(defun cb-previous-intern-buffer ()
  "Previous *buffer* - limited list."
  (interactive)
  (cb-prev-buffer "*buffers-subset"))

(defun cb-previous-files-buffer ()
  "Previous file buffer."
  (interactive)
  (cb-prev-buffer "files"))

(defun cb-next-files-buffer ()
  "Next file buffer."
  (interactive)
  (cb-next-buffer "files"))

(defun cb-previous-all-buffer ()
  "Previous file buffer."
  (interactive)
  (cb-prev-buffer "all"))

(defun cb-next-all-buffer ()
  "Next file buffer."
  (interactive)
  (cb-next-buffer "all"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; contextual cycling

(defun cb-testthisbufferconf ()
  "Test this buffer configuration."
  (interactive)
  (message "%s" (cb-this-buffer-configuration)))

(defun cb-this-buffer-configuration ()
  "Get the right configuration to use for contextual cycling."
  (cond
   ((not (cb-emacs-buffers-filter (current-buffer))) "*buffers")
   ((cb-mode-match (current-buffer) (cb-get-buffer-group-modes "shells")) "shells")
   ((cb-mode-match (current-buffer) (cb-get-buffer-group-modes "dired")) "dired")
   ((not (bs-visits-non-file (current-buffer))) "files")
   (t "all")))

(defun cb-next-buffer-contextual ()
  "Cycle through the buffers that match the current buffer."
  (interactive)
  (cb-next-buffer (cb-this-buffer-configuration)))

(defun cb-previous-buffer-contextual ()
  "Cycle through the buffers that match the current buffer."
  (interactive)
  (cb-prev-buffer (cb-this-buffer-configuration)))


(defun rename-uniquely-shell ()
  "Rename *shell* uniquely, get a new shell, If not a shell then just rename uniquely."
  (interactive)
  (if (string-match "^\\*shell\\*$"  (buffer-name))
      (progn
        (rename-uniquely)
        (shell))
    (rename-uniquely)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; BS Show and Ibuffer functions.

(defun cb-bs-show ()
  "Just bring up \"bs-show\" with the current show mode setting."
  (interactive)
  ;;(setq bs-cycle-configuration-name cb-current-show-mode)
  ;;(bs--configuration-name-for-prefix-arg cb-current-show-mode)
  (bs--show-with-configuration cb-current-show-mode))

(defun cb-show ()
  "Bring up whatever needs to be displayed for the current show mode."
  (interactive)
  (let ((func
         (cond ((equal cb-current-show-mode "ibuffer") 'ibuffer)
               ((equal cb-current-show-mode "eproject") 'eproject-ibuffer) ; eproject doesn't work here.
               (t 'cb-bs-show))))
    (funcall func)))

(defun cb-next-show ()
  "Switch to the next show-mode, and call the appropriate buffer function."
  (interactive)
  (cb-next-show-mode)
  (cb-show)
  )


(defun cb-next-show-window ()
  "Move to dired mode window if possible.  Call dired if no windows visible.
Give next dired buffer as needed."
  (interactive)
  (let ((current (selected-window))
        (win (cb-get-any-window-with-mode nil "cb-show")))
    (if (null win)
        (cb-show)
      (when (equal (selected-window) current)
        (cb-next-show)))))

(defun cb-next-show-mode ()
  "Move curren show mode to the next mode in the list."
  (interactive)
  (setq cb-current-show-mode (cb-next-from-loop cb-current-show-mode cb-show-modes)))

(defun cb-next-from-loop (match somelist)
  "Find the next MATCH entry in SOMELIST after the one given.  Wrap around to the beginning as needed."
  (let ( (next-thing (cb-get-next-from-list match somelist)))
    (if (not next-thing)
        (car somelist)
      next-thing)))

(defun cb-get-next-from-list (match somelist)
  "Find the next MATCH in SOMELIST after the one given."
  (if (string-match match (car somelist))
      (if (cdr somelist)
          (car (cdr somelist))
        nil)
    (cb-get-next-from-list match (cdr somelist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Kill buffers, windows and frames...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cb-kill ()
  "Kill/delete the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun cb-kill-delete-window ()
  "Kill buffer and all windows in it."
  (interactive)
  (let (buffer)
    (setq buffer (current-buffer))
    (delete-windows-on buffer)
    (kill-buffer buffer)))

(defun cb-kill-delete-frame ()
  "Kill buffer and the frame it's in."
  (interactive)
  (let (buffer)
    (setq buffer (current-buffer))
                                        ;        (delete-windows-on buffer)
    (kill-buffer buffer)
    (delete-frame)))

(provide 'cyclebuf)

;;; cyclebuf.el ends here
