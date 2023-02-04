;;; package --- clj-refactor
;;; Commentary:

(require 'clj-refactor)

;;; Code:
(defun my-clj-refactor-clojure-mode-hook ()
  "Set up the refactor hook."
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clj-refactor-clojure-mode-hook)
;;; clj-refactor-conf.el ends here
