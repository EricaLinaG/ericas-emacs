;;; update --- Update packages, install missing packages.

;;; Commentary:
;;; Give a script to run to install all the packages the first time.
;;; This will only work if run from the directory it is in.
;;; We change the user-emacs-directory to here so that Emacs will install
;;; its packages here.

;;; The only difference between this and install.el is that install only looks
;;; for missing packages.  If they are installed it doesn't touch them.
;;; This takes a little more time, but checks to see if anything needs updating.
;;; install is faster if repeating an install step a cause de failures.


;;; Code:

;;; Because when we get here, emacs is still pointing at ~/.emacs.d and we
;;; need it to point here. I'm not yet sure why it doesn't point here.
;;; chemacs must not set it when we use --script which means we can
;;; just run this with vanilla emacs and fool it to point here so
;;; our packages get installed.

(shell-command "git pull origin master")

;; Trick emacs to be here, instead of .emacs.d
;; Requires being here or using emacs --chdir <here> to work.
(setq user-emacs-directory default-directory)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(setq load-path
      (append (list
               (expand-file-name "./early-packages" user-emacs-directory)
	       )
	      load-path))

(load "mypackages")
(update-mypackages)

(provide 'update)
;;; update.el ends here
