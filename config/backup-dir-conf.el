(require 'backup-dir)

(setq
 root-dir (file-name-as-directory "~/.emacs.d")
 tmp-dir (file-name-as-directory (concat root-dir "tmp"))
 autosaves-dir (file-name-as-directory (concat tmp-dir  "autosaves"))
 backups-dir  (file-name-as-directory (concat tmp-dir  "backups")))

;; create tmp dirs if necessary
(make-directory autosaves-dir t)
(make-directory backups-dir t)


(make-variable-buffer-local 'backup-inhibited)
(setq bkup-backup-directory-info
      `((t ,backups-dir ok-create full-path prepend-name)))

(setq auto-save-file-name-transforms `((".*" ,(concat autosaves-dir "\\1") t)))
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,backups-dir)))
(setq auto-save-list-file-name (concat autosaves-dir "autosave-list"))

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq tramp-bkup-backup-directory-info bkup-backup-directory-info)
