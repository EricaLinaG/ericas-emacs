(require 'palette-theme)
(require 'palette-solarized)

(deftheme solarized-dark2 "Another dark variant of the Solarized color theme")


(setq solarized-high-contrast-mode-line nil)
(create-theme 'dark 'solarized-dark2 palette-solarized)

(provide-theme 'solarized-dark2)
