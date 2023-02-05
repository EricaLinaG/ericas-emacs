;;; install --- Installs packages for the first time.

;;; Commentary:
;;; Give a script to run to install all the packages the first time.
;;; Skips packages that are already installed.

;;; This will only work if run from the directory it is in.
;;; We change the user-emacs-directory to here so that Emacs will install
;;; its packages here.

;;; Install only looks for missing packages.  It does not check for upgrades.


;;; Code:

;;; Because when we get here, emacs is still pointing at ~/.emacs.d and we
;;; need it to point here. I'm not yet sure why it doesn't point here.
;;; chemacs must not set it when we use --script which means we can
;;; just run this with vanilla emacs and fool it to point here so
;;; our packages get installed.

;; trick emacs to be here, instead of .emacs.d
(setq user-emacs-directory default-directory)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; this is all we need. We just want to install all the packages.
(setq load-path
      (append (list
               (expand-file-name "./early-packages" user-emacs-directory)
	       )
	      load-path))

(load "mypackages")

(install-mypackages)

(provide 'install)
;;; install.el ends here
