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
