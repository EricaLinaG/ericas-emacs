;;; org-roam-conf.el --- configure org-roam
;;; Commentary:
;;; Code:

(require 'org-roam)
;; (make-directory "~/Documents/org/org-roam")

(setq org-roam-directory (file-truename "~/Documents/org/org-roam"))

;; The file-truename function is only necessary when you use symbolic
;; links inside org-roam-directory: Org-roam does not resolve symbolic
;; links. One can however instruct Emacs to always resolve symlinks, at a
;; performance cost:

(setq find-file-visit-truename t)

;; Next, we setup Org-roam to run functions on file changes to maintain cache
;; consistency. This is achieved by running M-x org-roam-db-autosync-mode. To
;; ensure that Org-roam is available on startup, place this in your Emacs
;; configuration:

(org-roam-db-autosync-mode)

(require 'org-roam-timestamps)

(org-roam-timestamps-mode)

(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(defun my-org-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))
