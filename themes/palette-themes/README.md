# Prelude

ez-theme is a fork of solarized emacs. It uses palettes to create
themes in the same way as the original solarized for emacs with some changes.
New colors specifically for modeline control and different rules for colorization
of the modeline when those colors are not present. Even when using the original 
solarized color palette, this results in a slightly different looking theme.
The solarized variable for high contrast modeline is ignored since exact control
over the modeline is now possible.

This also makes it very easy to create new themes by providing just a color palette.
Included so far is a solarized-darker palette and a midnight-blue palette. 

If the color palette is chosen carefully it is possible to create both light and dark
themes from the same palette. But that is not necessary.

Everything else is here because of those who came before me.
                [Eric Gebhart](http://github/EricGebhart) 04/08/2014

-----------

Pertinent Excerpts from the original solarized theme package.

--------------

ez-theme is a fork of solarized for emacs.

Solarized for Emacs is an Emacs port of the [Solarized theme for vim](http://ethanschoonover.com/solarized),
developed by Ethan Schoonover.

Solarized for Emacs is tested only under Emacs 24, but should be
working under Emacs 23 as well. The theme is implemented in terms of
customizations and `deftheme` and does not require the
`color-theme-package`.

# Customisations

If you don't like low-contrast modeline or fringe, you can `customize` them:

```emacs
;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)
```

Note that these need to be set **before** `load-theme` is invoked for Solarized.

# Contributors

- [Thomas Frössman](http://t.jossystem.se)
- [Eric Gebhart](http://github/EricGebhart)

(Add yourself to the list)
