(require 'git-gutter)

(global-git-gutter-mode +1)

(setq git-gutter:modified-sign " ~")
(setq git-gutter:added-sign " +")
(setq git-gutter:deleted-sign " -")

;; These should be set in the theme. But maybe not.
;;(set-face-background 'git-gutter:unchanged "#102030")
;;(set-face-background 'git-gutter:modified "#102030")
;;(set-face-foreground 'git-gutter:modified "purple")
;;(set-face-foreground 'git-gutter:added "green")
;;(set-face-foreground 'git-gutter:deleted "red")

;;(setq git-gutter:separator-sign "|")
;;(set-face-foreground 'git-gutter:separator "yellow")
