(require 'origami)

(global-origami-mode t)

;; Add namespace/def to regex so specs will hide, among other things.
(defun origami-clj-parser (create)
  (origami-lisp-parser create "(\\(def\\|\\w*\/\\w*def\\)\\(\\w\\|-\\)*\\s-*\\(\\s_\\|[:]*\\w\\|[?!]\\)*\\([ \\t]*\\[.*?\\]\\)?"))
