(require 'palette-theme)
(require 'palette-solarized-darker)

(deftheme solarized-darker2 "A darker variant of the Solarized dark color theme")


(setq solarized-high-contrast-mode-line nil)
(create-theme 'dark 'solarized-darker2 palette-solarized-darker)

(provide-theme 'solarized-darker2)
