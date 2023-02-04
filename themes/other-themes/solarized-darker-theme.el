(require 'solarized)
(require 'solarized-dark-theme)

(deftheme solarized-darker "A darker variant of the Solarized dark colour theme")

(setq solarized-high-contrast-mode-line t)

(create-solarized-theme 'dark 'solarized-dark)

(custom-theme-set-faces
 'solarized-darker
 '(default ((((class color) (min-colors 89)) (:background "#001b26" :foreground "gray70"))))
 '(mode-line ((((class color) (min-colors 89)) (:box (:line-width 1 :color "#2aa198")))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:box (:line-width 1 :color "#839496" :style nil)))))
 '(fringe ((((class color) (min-colors 89)) (:foreground "gray70" :background "#001b26"))))
 )

(provide-theme 'solarized-darker)
