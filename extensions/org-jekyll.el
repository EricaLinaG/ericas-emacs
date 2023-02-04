;;; Lazily expanded to export markdown Juillet, 2022, Eric Gebhart
;;;
;;;
;;; org-jekyll.el --- Export jekyll-ready posts form org-mode entries
;;;
;;; Author: Juan Reyero
;;; Version: 0.4
;;; Keywords: hypermedia
;;; Package-Requires: ((org "8.0"))
;;; Homepage: http://juanreyero.com/open/org-jekyll/
;;; Repository: http://github.com/juanre/org-jekyll
;;; Public clone: git://github.com/juanre/org-jekyll.git
;;;
;;; Commentary:
;;;
;;; Extract subtrees from your org-publish project files that have
;;; a :blog: keyword and an :on: property with a timestamp, and
;;; export them to a subdirectory _posts of your project's publishing
;;; directory in the year-month-day-title.html format that Jekyll
;;; expects.  Properties are passed over as yaml front-matter in the
;;; exported files.  The title of the subtree is the title of the
;;; entry.  The title of the post is a link to the post's page.
;;;
;;; Look at http://orgmode.org/worg/org-tutorials/org-jekyll.html for
;;; more info on how to integrate org-mode with Jekyll, and for the
;;; inspiration of the main function down there.
;;;
;;; Code:

(require 'ox-html)
(require 'ox-md)
(require 'ox-gfm)
(require 'cl)


;; Example Front yaml for a post.
;; ---
;; layout: post
;; title: "Fression, Pail and Cascalog"
;; description: "Using Fressian with Pails and Cascalog in Clojure."
;; Date: 2014-01-14 20:58:33
;; category: clojure
;; tags: [Fressian, Pail, Cascalog, Clojure, Big data]
;; ---

(defvar org-jekyll-categories '()
  "Post category alist for jekyll")
(defvar org-jekyll-tags '()
  "Post tags alist for jekyll")


;; haven't gotten this to work. It seems like there is shadowing somehow.
;; evaluation is always a void function.

;; org-gfm-export-as-markdown
;; org-md-export-as-markdown
;; org-html-export-as-html

;; (defvar org-jekyll-md-export-func nil
;;   "Specify which markdown export function to use.")

;; this goes elsewhere, in the config.  C-h v confirms it is set.
;; (setq org-jekyll-md-export-func 'org-gfm-export-as-markdown)

(defvar org-jekyll-export-type 'markdown
  "Specify which type of export, html or markdown.")


;; Front yaml for a post.
;; ---
;; layout: post
;; publish-to-project: eg-com
;; title: "Fression, Pail and Cascalog"
;; description: "Using Fressian with Pails and Cascalog in Clojure."
;; Date: 2014-01-14 20:58:33
;; category: clojure
;; tags: [Fressian, Pail, Cascalog, Clojure, Big data]
;; ---

(defun blog-on ()
  "Set a blog tag keyword and add an On property timestamp,
   add appropriate properties needed by jekyll. Date, title, description,
   category and tags."
  (progn
    (org-toggle-tag "blog" "on")
    (org-set-property "on" (format-time-string (org-time-stamp-format t t)))
    (org-set-property "date" (format-time-string (org-time-stamp-format t t)))
    (org-set-property "publish-to-project"
                      (completing-read "project : "
                                       (mapcar 'car org-publish-project-alist)
                                       nil 'confirm nil nil nil))
    (org-set-property "title" (read-string "title: " ))
    (org-set-property "description" (read-string "description: " ))
    (org-set-property "category"
                      (completing-read "category : " org-jekyll-categories
                                       nil 'confirm nil nil nil))
    (org-set-property "blogtags"
                      (replace-regexp-in-string
                       "\)" "\]"
                       (replace-regexp-in-string
                        "\(" "\["
                        (format "%s"
                                (completing-read-multiple
                                 "tags : " org-jekyll-tags
                                 nil 'confirm nil nil nil)))))))

;; over-ride the default markdown src block function which is worthless.
;; ox-gfm has this already, so just use that.
(defun org-md-example-block (example-block _content info)
  "Transcode element EXAMPLE-BLOCK as ```lang ...```."
  (format "```%s\n%s\n```"
          (org-element-property :language example-block)
          (org-remove-indentation
           (org-export-format-code-default example-block info))))

(defun org-jekyll-export-entry (project)
  "Do the export type that we want according to org-jekyll-export-type.
If not html then markdown."
  (if (eq org-jekyll-export-type 'html)
      (org-jekyll-export-html-entry project)
    (org-jekyll-export-md-entry project)))

(defun org-jekyll-export-current-entry-project ()
  "export post entry with a prompt to determine the project"
  (let* ((props (org-entry-properties nil 'standard))
         (project (cdr (or (assoc "publish-to-project" props)
                           (assoc "PUBLISH-TO-PROJECT" props)))))
    (org-jekyll-export-entry
     (car (remove-if      ;;Get the whole project entry
           (lambda (prj) (not (equal (car prj) project)))
           org-publish-project-alist)))))

(defun org-jekyll-export-current-blog-project ()
  "export post entry with a prompt to determine the project"
  (let* ((props (org-entry-properties nil 'standard))
         (project (cdr (or (assoc "publish-to-project" props)
                           (assoc "PUBLISH-TO-PROJECT" props)))))
    (org-jekyll-export-blog
     (car (remove-if      ;;Get the whole project entry
           (lambda (prj) (not (equal (car prj) project)))
           org-publish-project-alist)))))

(defun org-jekyll-export-md-entry (project)
  "Export an org entry PROJECT to markdown."
  (let* ((props (org-entry-properties nil 'standard))
         (time (cdr (or (assoc "on" props)
                        (assoc "ON" props))))
         (lang (cdr (or (assoc "lang" props)
                        (assoc "LANG" props))))
         (category (if org-jekyll-category
                       (cdr (assoc org-jekyll-category props))
                     nil))
         (yaml-front-matter (copy-alist props)))
    (unless (assoc "layout" yaml-front-matter)
      (push '("layout" . "post") yaml-front-matter))
    (when time
      (let* ((heading (org-get-heading t))
             (title (replace-regexp-in-string "[:=\(\)\?]" ""
                                              (replace-regexp-in-string
                                               "[ \t]" "-" heading)))
             (str-time (and (string-match "\\([[:digit:]\-]+\\) " time)
                            (match-string 1 time)))
             (to-file (format "%s-%s.md" str-time
                              (org-jekyll-sanitize-string title project)))
             (org-buffer (current-buffer))
             (yaml-front-matter (cons (cons "title" heading)
                                      yaml-front-matter))
             output)
        (org-narrow-to-subtree)
        (let ((level (- (org-reduced-level (org-outline-level)) 1))
              (top-level org-html-toplevel-hlevel)
              (contents (buffer-substring (point-min) (point-max)))
              (site-root (org-jekyll-site-root project)))
          ;; Without the promotion the header with which the headline
          ;; is exported depends on the level.  With the promotion it
          ;; fails when the entry is not visible (ie, within a folded
          ;; entry).
          (dotimes (n level nil) (org-promote-subtree))
          (setq output
                (replace-regexp-in-string
                 (format "<h%d id=\"sec-1\">\\(.+\\)</h%d>"
                         top-level top-level)
                 (format
                  "<h%d id=\"sec-1\"><a href=\"%s{{ page.url }}\">\\1</a></h%d>"
                  top-level site-root top-level)
                 (with-current-buffer
                     (org-gfm-export-as-markdown)
                   (buffer-string))))
          (set-buffer org-buffer)
          (delete-region (point-min) (point-max))
          (insert contents)
          (save-buffer))
        (widen)
        (with-temp-file (ensure-directories-exist
                         (expand-file-name
                          to-file (org-jekyll-publish-dir project category)))
          (when yaml-front-matter
            (insert "---\n")
            (mapc (lambda (pair)
                    (insert
                     (format "%s: %s\n"
                             (replace-regexp-in-string "blogtags" "tags"
                                                       (car pair))
                             (cdr pair))))
                  yaml-front-matter)
            (if (and org-jekyll-localize-dir lang)
                (mapc (lambda (line)
                        (insert (format "%s\n" line)))
                      (org-jekyll-slurp-yaml (concat org-jekyll-localize-dir
                                                     lang ".yml"))))
            (insert "---\n\n"))
          (insert output))))))

;; (defun org-jekyll-export-current-entry-project ()
;;   "export post entry with a prompt to determine the project"
;;   (let ((project-name
;;          (completing-read "project : " (mapcar 'car org-publish-project-alist)
;;                           nil 'confirm nil nil nil)))
;;     (org-jekyll-export-entry
;;      (car (remove-if
;;            (lambda (prj) (not (equal (car prj) project-name)))
;;            org-publish-project-alist)))))

;; (defun org-jekyll-export-blog-project ()
;;   "export blog with a prompt to determine the project"
;;   (let ((project-name
;;          (completing-read "project : " (mapcar 'car org-publish-project-alist)
;;                           nil 'confirm nil nil nil)))
;;     (org-jekyll-export-blog
;;      (remove-if
;;       (lambda (prj) (not (equal (car prj) project-name)))
;;       org-publish-project-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org-jekyll-category nil
  "Specify a property which, if defined in the entry, is used as
  a category: the post is written to category/_posts. Ignored if
  nil. Use \"lang\" if you want to send posts in different
  languages to different directories.")

(defvar org-jekyll-lang-subdirs nil
  "Make it an assoc list indexed by language if you want to
bypass the category subdir definition and build blog subdirs per
language.")

(defvar org-jekyll-localize-dir nil
  "If non-nil and the lang property is set in the entry,
   org-jekyll will look for a lang.yml file in this directory and
   include it in the front matter of the exported entry.")

(defvar org-jekyll-new-buffers nil
  "Buffers created to visit org-publish project files looking for blog posts.")

(defun org-jekyll-publish-dir (project &optional category)
  "Where does the project go, by default a :blog-publishing-directory
   entry in the org-publish-project-alist."
  (princ category)
  (if org-jekyll-lang-subdirs
      (let ((pdir (plist-get (cdr project) :blog-publishing-directory))
            (langdir (cdr (assoc category org-jekyll-lang-subdirs))))
        (if langdir
            (concat pdir (cdr (assoc category org-jekyll-lang-subdirs))
                    "_posts/")
          (let ((ppdir (plist-get (cdr project) :blog-publishing-directory)))
            (unless ppdir
              (setq ppdir (plist-get (cdr project) :publishing-directory)))
            (concat ppdir
                    (if category (concat category "/") "")
                    "_posts/"))))
    (let ((pdir (plist-get (cdr project) :blog-publishing-directory)))
      (unless pdir
        (setq pdir (plist-get (cdr project) :publishing-directory)))
      (concat pdir
              (if category (concat category "/") "")
              "_posts/"))))

(defun org-jekyll-site-root (project)
  "Site root, like http://yoursite.com, from which blog
  permalinks follow.  Needed to replace entry titles with
  permalinks that RSS agregators and google buzz know how to
  follow.  Looks for a :site-root entry in the org-publish-project-alist."
  (or (plist-get (cdr project) :site-root)
      ""))


(defun org-get-jekyll-file-buffer (file)
  "Get a buffer visiting FILE.  If the buffer needs to be
  created, add it to the list of buffers which might be released
  later.  Copied from org-get-agenda-file-buffer, and modified
  the list that holds buffers to release."
  (let ((buf (org-find-base-buffer-visiting file)))
    (if buf
        buf
      (progn (setq buf (find-file-noselect file))
             (if buf (push buf org-jekyll-new-buffers))
             buf))))

(defun org-jekyll-slurp-yaml (fname)
  (remove "---" (if (file-exists-p fname)
                    (split-string (with-temp-buffer
                                    (insert-file-contents fname)
                                    (buffer-string))
                                  "\n" t))))

(defun ensure-directories-exist (fname)
  (let ((dir (file-name-directory fname)))
    (unless (file-accessible-directory-p dir)
      (make-directory dir t)))
  fname)

(defun org-jekyll-sanitize-string (str project)
  (if (plist-get (cdr project) :jekyll-sanitize-permalinks)
      (progn (setq str (downcase str))
             (dolist (c '(("á" . "a")
                          ("é" . "e")
                          ("í" . "i")
                          ("ó" . "o")
                          ("ú" . "u")
                          ("à" . "a")
                          ("è" . "e")
                          ("ì" . "i")
                          ("ò" . "o")
                          ("ù" . "u")
                          ("ñ" . "n")
                          ("ç" . "s")
                          ("\\$" . "S")
                          ("€" . "E")))
               (setq str (replace-regexp-in-string (car c) (cdr c) str)))
             (replace-regexp-in-string "[^abcdefghijklmnopqrstuvwxyz-]" ""
                                       (replace-regexp-in-string " +" "-" str)))
    str))





(defun org-jekyll-export-html-entry (project)
  (let* ((props (org-entry-properties nil 'standard))
         (time (cdr (or (assoc "on" props)
                        (assoc "ON" props))))
         (lang (cdr (or (assoc "lang" props)
                        (assoc "LANG" props))))
         (category (if org-jekyll-category
                       (cdr (assoc org-jekyll-category props))
                     nil))
         (yaml-front-matter (copy-alist props)))
    (unless (assoc "layout" yaml-front-matter)
      (push '("layout" . "post") yaml-front-matter))
    (when time
      (let* ((heading (org-get-heading t))
             (title (replace-regexp-in-string "[:=\(\)\?]" ""
                                              (replace-regexp-in-string
                                               "[ \t]" "-" heading)))
             (str-time (and (string-match "\\([[:digit:]\-]+\\) " time)
                            (match-string 1 time)))
             (to-file (format "%s-%s.html" str-time
                              (org-jekyll-sanitize-string title project)))
             (org-buffer (current-buffer))
             (yaml-front-matter (cons (cons "title" heading)
                                      yaml-front-matter))
             html)
        (org-narrow-to-subtree)
        (let ((level (- (org-reduced-level (org-outline-level)) 1))
              (top-level org-html-toplevel-hlevel)
              (contents (buffer-substring (point-min) (point-max)))
              (site-root (org-jekyll-site-root project)))
          ;; Without the promotion the header with which the headline
          ;; is exported depends on the level.  With the promotion it
          ;; fails when the entry is not visible (ie, within a folded
          ;; entry).
          (dotimes (n level nil) (org-promote-subtree))
          (setq html
                (replace-regexp-in-string
                 (format "<h%d id=\"sec-1\">\\(.+\\)</h%d>"
                         top-level top-level)
                 (format
                  "<h%d id=\"sec-1\"><a href=\"%s{{ page.url }}\">\\1</a></h%d>"
                  top-level site-root top-level)
                 (with-current-buffer
                     (org-html-export-as-html nil t t t
                                              '(:tags nil
                                                      :table-of-contents nil))
                   (buffer-string))))
          (set-buffer org-buffer)
          (delete-region (point-min) (point-max))
          (insert contents)
          (save-buffer))
        (widen)
        (with-temp-file (ensure-directories-exist
                         (expand-file-name
                          to-file (org-jekyll-publish-dir project category)))
          (when yaml-front-matter
            (insert "---\n")
            (mapc (lambda (pair)
                    (insert (format "%s: %s\n" (car pair) (cdr pair))))
                  yaml-front-matter)
            (if (and org-jekyll-localize-dir lang)
                (mapc (lambda (line)
                        (insert (format "%s\n" line)))
                      (org-jekyll-slurp-yaml (concat org-jekyll-localize-dir
                                                     lang ".yml"))))
            (insert "---\n\n"))
          (insert html))))))

; Evtl. needed to keep compiler happy:
(declare-function org-publish-get-project-from-filename "org-publish"
                  (filename &optional up))

;;;###autoload
(defun org-jekyll-export-current-entry ()
  (interactive)
  (save-excursion
    (let ((project (org-publish-get-project-from-filename buffer-file-name)))
      (org-back-to-heading t)
      (org-jekyll-export-entry project))))

;;;###autoload
(defun org-jekyll-export-blog ()
  "Export all entries in project files that have a :blog: keyword
and an :on: datestamp.  Property drawers are exported as
front-matters, outline entry title is the exported document
title. "
  (interactive)
  (save-excursion
    (setq org-jekyll-new-buffers nil)
    (let ((project (org-publish-get-project-from-filename (buffer-file-name))))
     (mapc
      (lambda (jfile)
        (if (string= (file-name-extension jfile) "org")
            (with-current-buffer (org-get-jekyll-file-buffer jfile)
              ;; It fails for non-visible entries, CONTENT visibility
              ;; mode ensures that all of them are visible.
              (message (concat "org-jekyll: publishing " jfile ))
              (org-content)
              (org-map-entries (lambda () (org-jekyll-export-entry project))
                               "blog|BLOG"))))
      (org-publish-get-base-files project)))
    (org-release-buffers org-jekyll-new-buffers)))

;;;###autoload
(defun org-jekyll-export-project (project-name)
  "Export all entries in project files that have a :blog: keyword
and an :on: datestamp.  Property drawers are exported as
front-matters, outline entry title is the exported document
title. "
  (interactive)
  (save-excursion
    (setq org-jekyll-new-buffers nil)
    (let ((project (assoc project-name org-publish-project-alist)))
     (mapc
      (lambda (jfile)
        (if (string= (file-name-extension jfile) (plist-get (cdr project)
                                                            :base-extension))
            (with-current-buffer (org-get-jekyll-file-buffer jfile)
              ;; It fails for non-visible entries, CONTENT visibility
              ;; mode ensures that all of them are visible.
              (message (concat "org-jekyll: publishing " jfile ))
              (org-content)
              (org-map-entries (lambda () (org-jekyll-export-entry project))
                               "blog|BLOG"))))
      (org-publish-get-base-files project)))
    (org-release-buffers org-jekyll-new-buffers)))

(provide 'org-jekyll)

;;; org-jekyll.el ends here
