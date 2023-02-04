(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'company-etags)
(add-to-list 'company-etags-modes 'clojure-mode)
