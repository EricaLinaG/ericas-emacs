;;(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'flycheck-mode)
;; OR for flymake support:
(add-hook 'haskell-mode-hook 'flymake-mode)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(add-hook 'haskell-mode-hook 'dante-mode)
;;;(flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
