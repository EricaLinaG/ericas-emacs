(require 'projectile)

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-enable-idle-timer t)

(defadvice find-tag-at-point (before auto-visit-tags)
  "Load default TAGS file from home directory if needed"
  (visit-tags-table (concat (projectile-project-root) "TAGS")))
(ad-activate 'find-tag-at-point)

;;; persepective integration.
;;(require 'perspective)
;;(persp-mode)

;; ;;(require 'persp-projectile)
;;(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)

;;(global-set-key (kbd "C-c C-p") 'projectile-persp-switch-project)

;; ;;fiplr integration.  fuzzy matching of files in project.
;; (require 'fiplr)
;; (setq fiplr-root-markers '(".git"))
;; (setq fiplr-ignored-globs '((directories (".git" "target" ".svn" "log" "tmp"))
;; 			    (files (".keep" ".DS_Store" ".class" "TAGS*" "*.jpg" "*.elc" "*.png" "*.zip" "*~"))))


(require 'find-file-in-project)
(add-to-list 'ffip-patterns "*.cljs")
(add-to-list 'ffip-patterns "*.clj")
(add-to-list 'ffip-patterns "*.haml")
(add-to-list 'ffip-patterns "*.html.erb")
(add-to-list 'ffip-patterns "*.erb")
(add-to-list 'ffip-patterns "*.go")
(add-to-list 'ffip-patterns "*.java")
(add-to-list 'ffip-patterns "*.js")
(add-to-list 'ffip-patterns "*.css")
(add-to-list 'ffip-patterns "*Gemfile")
(add-to-list 'ffip-patterns "*yml")

(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories ".bundle")
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "coverage")
     (add-to-list 'grep-find-ignored-directories "log")))
