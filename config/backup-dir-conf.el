(require 'backup-dir)

(setq
 root-dir (file-name-as-directory "~/")
 tmp-dir (file-name-as-directory (concat root-dir "tmp"))
 autosaves-dir (file-name-as-directory (concat tmp-dir  "autosaves"))
 backups-dir  (file-name-as-directory (concat tmp-dir  "backups")))

;; create tmp dirs if necessary
(make-directory autosaves-dir t)
(make-directory backups-dir t)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(make-variable-buffer-local 'backup-inhibited)
(setq bkup-backup-directory-info
      `((t ,backups-dir ok-create full-path prepend-name)))

(setq auto-save-file-name-transforms `((".*" ,(concat autosaves-dir "\\1") t)))
(setq backup-by-copying t)
;; (setq backup-directory-alist `((".*" . ,backups-dir)))
(setq backup-directory-alist `(("." . ,backups-dir)))
(setq auto-save-list-file-name (concat autosaves-dir "autosave-list"))

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq tramp-bkup-backup-directory-info bkup-backup-directory-info)

;; another way.
;; ;; Put autosave files (ie #foo#) in one place, *not*
;; ;; scattered all over the file system!
;; (defvar autosave-dir
;;   (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

;; (make-directory autosave-dir t)

;; (defun auto-save-file-name-p (filename)
;;   (string-match "^#.*#$" (file-name-nondirectory filename)))

;; (defun make-auto-save-file-name ()
;;   (concat autosave-dir
;;           (if buffer-file-name
;;               (concat "#" (file-name-nondirectory buffer-file-name) "#")
;;             (expand-file-name
;;              (concat "#%" (buffer-name) "#")))))

;; ;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; ;; list contains regexp=>directory mappings; filenames matching a regexp are
;; ;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
;; (defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
;; (setq backup-directory-alist (list (cons "." backup-dir)))
