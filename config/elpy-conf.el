(require 'elpy)
(require 'py-autopep8)
(require 'python-black)
;;;;;;;;;;;;;;;;;;;;;
;;  ELPY

(elpy-enable)

;; (setq elpy-rpc-backend "rope")
;; (setq elpy-rpc-backend "jedi")

;; Enable autopep8
;;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;;(setq py-autopep8-options '("--max-line-length=100"))

;; Enable Black instead.
(add-hook 'python-mode-hook 'python-black-on-save-mode)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;;; use python interpreter
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")

;; Use Ipython
;;(setq python-shell-interpreter "ipython"
;;      python-shell-interpreter-args "--simple-prompt --pprint")

(provide 'elpy-conf)
;;; elpy-conf.el ends here
