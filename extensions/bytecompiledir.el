;;; package --- Summary
;;; Commentary:
;;; Code
;;;
;;; bytecompiledir.el
;;;
;;; Author: Jukka Partanen <jtp@cs.hut.fi>
;;;
;;; Created      : Sat Sep  4 18:47:25 1993 jtp
;;; Last modified: Fri Sep 30 17:55:15 1994 too
;;;

(defvar byte-compile-file-regexp ".+"
  "The files matching this regexp (and suffixed with .el) are
byte-compiled and loaded by byte-compile-directory.")

(defun byte-compile-if-newer (file &optional dest)
  (let ((file-el (expand-file-name (concat file ".el")))
	(file-elc (expand-file-name (concat (or dest file) ".elc"))))
    (if (file-newer-than-file-p file-el file-elc)
	(progn
	  (byte-compile-file file-el)
	  (if dest
	      (rename-file (concat file-el "c") file-elc t))))))

(defun byte-compile-directory (dir &optional destdir dontload)
  "byte-compile the elisp files in DIR to DESTDIR and load the files
if DONTLOAD is nil."
  (interactive "DDirectory: ")
  (let ((files (directory-files dir nil
				(concat byte-compile-file-regexp "\.el$")))
	name-el dest-el name dest)
    (while files
      (setq name-el (concat (file-name-as-directory dir) (car files)))
      (setq name (substring name-el 0 (- (length name-el) 3)))
      (if destdir
	  (progn
	    (setq dest-el (concat (file-name-as-directory destdir) (car files)))
	    (setq dest (substring dest-el 0 (- (length dest-el) 3))))
	(setq dest nil))
      (byte-compile-if-newer name dest)
      (or dontload (load (or dest name)))
      (setq files (cdr files))))
  nil)

(provide 'bytecompiledir)
