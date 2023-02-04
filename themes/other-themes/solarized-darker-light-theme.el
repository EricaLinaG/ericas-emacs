(require 'solarized)
(require 'solarized-light-theme)

(deftheme solarized-darker-light "The darker variant of the Solarized light colour theme")

;(setq solarized-high-contrast-mode-line nil)

(create-solarized-theme 'light 'solarized-light)

(custom-theme-set-faces
 'solarized-darker-light
 '(default ((((class color) (min-colors 89)) (:background "#ddd6c3" :foreground "#456b63"))))
 '(mode-line ((((class color) (min-colors 89)) (:box (:line-width 1 :color "#69cabf")))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:box (:line-width 1 :color "#073642" :style nil)))))
 '(fringe ((((class color) (min-colors 89)) (:foreground "#456b63" :background "#ddd6c3"))))
 )

(provide-theme 'solarized-darker-light)
