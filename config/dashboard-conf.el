;;; package --- Summary
;;; Commentary:
;;; Code:

;; (defun my/dashboard-banner ()
;;   """Set a dashboard banner including information on package initialization
;;    time and garbage collections."""
;;   (setq dashboard-banner-logo-title
;;         (format "Emacs ready in %.2f seconds with %d garbage collections."
;;                 (float-time (time-subtract after-init-time before-init-time)) gcs-done)))

;; (use-package dashboard
;;              :init
;;              (add-hook 'after-init-hook 'dashboard-refresh-buffer)
;;              (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
;;              :config
;;              (setq dashboard-startup-banner 'logo)
;;              (dashboard-setup-startup-hook))


;; (setq initial-scratch-message (concat initial-scratch-message ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;; This Emacs is Powered by `HELM' using\n;; emacs program \"emacs\".\n;; This is a minimal `helm' configuration to discover `helm' or debug it.\n;; You can retrieve this minimal configuration in \"/tmp/helm-cfg.el\".\n;; ;; Some original Emacs commands are replaced by their `helm' counterparts:\n\n;; - `find-file'(C-x C-f)            =>`helm-find-files'\n;; - `occur'(M-s o)                  =>`helm-occur'\n;; - `list-buffers'(C-x C-b)         =>`helm-buffers-list'\n;; - `completion-at-point'(M-tab)    =>`helm-lisp-completion-at-point'[1]\n;; - `apropos-command'(C-h a)        =>`helm-apropos'\n;; - `dabbrev-expand'(M-/)           =>`helm-dabbrev'\n;; - `execute-extended-command'(M-x) =>`helm-M-x'\n\n ;; Some other Emacs commands are \"helmized\" by `helm-mode'.\n;; [1] Coming with emacs-24.4, `completion-at-point' is \"helmized\" by `helm-mode'\n ;; which provides Helm completion in many places like `shell-mode'.\n;; Find context help for most Helm commands with `C-h m'.\n ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n ;;; See: http://tuhdo.github.io/helm-intro.html\n\n"))

(require 'projectile)
(require 'dashboard)

;; replace so that dashboard always starts. Sometimes I pass a title, evals or files.
(defun dashboard-setup-startup-hook ()
  "Setup post initialization hooks.
If a command line argument is provided, assume a filename and skip displaying
Dashboard."
  (interactive)
  ;;(when (< (length command-line-args) 2))
  (add-hook 'after-init-hook (lambda ()
                               ;; Display useful lists of items
                               (dashboard-insert-startupify-lists)))
  (add-hook 'emacs-startup-hook (lambda ()
                                  (switch-to-buffer dashboard-buffer-name)
                                  (goto-char (point-min))
                                  (redisplay)
                                  (run-hooks 'dashboard-after-initialize-hook))))


;;(dashboard-setup-startup-hook)
(setq dashboard-projects-backend 'projectile)
;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs")
;; Set the banner
(setq dashboard-startup-banner ['logo])
;; Value can be
;; - nil to display no banner
;; - 'official which displays the official emacs logo
;; - 'logo which displays an alternative emacs logo
;; - 1, 2 or 3 which displays one of the text banners
;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

(setq show-week-agenda-p t)
(setq dashboard-items '((recents . 15) (agenda . 5)))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-startup-banner 'logo)

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
;; (setq dashboard-show-shortcuts nil)
;; To customize which widgets are displayed, you can use the following snippet

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

;; This will add the recent files, bookmarks,
;; projects, org-agenda and registers widgets to your dashboard each displaying 5 items.

;; To add your own custom widget is pretty easy,
;; define your widget’s callback function and add it to `dashboard-items` as such:

;; (defun dashboard-insert-custom (list-size)
;;   (insert "Custom text"))
;; (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
;; (add-to-list 'dashboard-items '(custom) t)

;; To show info about the packages loaded and the init time:

(setq dashboard-set-init-info t)

;; Also, the message can be customized like this:

;; (setq dashboard-init-info "Erica Rocks!")

;; A randomly selected footnote will be displayed. To disable it:

;; (setq dashboard-set-footer nil)

;; To customize it and customize its icon;

(setq dashboard-footer-messages '("Erica Rocks!"))

(setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'font-lock-keyword-face))

;; To use it with counsel-projectile or persp-projectile

;; (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
;; Or

;; (setq dashboard-projects-switch-function 'projectile-persp-switch-project)

;; To display today’s agenda items on the dashboard, add agenda to dashboard-items:

;; (add-to-list 'dashboard-items '(agenda) t)
;; To show agenda for the upcoming seven days set the variable dashboard-week-agenda to t.

;; (setq dashboard-week-agenda t)

;; By default org-agenda entries are filter by time,
;; only showing those task with DEADLINE, SCHEDULE-TIME or TIMESTAMP .
;; To show all agenda entries (except DONE)

(setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)

;; To have an extra filter, MATCH parameter is exposed as
;; dashboard-match-agenda-entry variable, by default is nil

(dashboard-setup-startup-hook)
