; Provide the solarized darker palette.

(setq palette-solarized-darker
      '(
        ("base03" . "#001b26")
        ;; background highlight dark
        ("base02" . "#073642")
        ;; emphasized content
        ("base01" . "#586e75")
        ;; primary content
        ("base00" . "#456b63")
        ("base0"  . "#b8b8b8")
        ;; comments
        ("base1"  . "#93a1a1")
        ;; background highlight light
        ("base2"  . "#eee8d5")
        ;; background light
        ("base3"  . "#c6d6c3")

        ;;modeline active dark - optional
        ("base4"  . "#456b63")
        ;;modeline active light
        ("base04"  . "#586e75")
        ;;modeline box
        ("base5"  . "#c6d6c3")
        ;;modeline box light
        ("base05"  . "#001b26")

        ;; Solarized accented colors
        ("yellow" . "#b58900")
        ("orange" . "#cb4b16")
        ("red"    . "#dc322f")
        ("magenta" . "#d33682")
        ("violet" . "#6c71c4")
        ("blue"   . "#268bd2")
        ("cyan"   . "#2aa198")
        ("green"  . "#859900")

        ;; Darker and lighter accented colors
        ;; Only use these in exceptional circumstances!
        ("yellow-d" . "#7B6000")
        ("yellow-l" . "#DEB542")
        ("orange-d" . "#8B2C02")
        ("orange-l" . "#F2804F")
        ("red-d"    . "#990A1B")
        ("red-l"    . "#FF6E64")
        ("magenta-d" . "#93115C")
        ("magenta-l" . "#F771AC")
        ("violet-d" . "#3F4D91")
        ("violet-l" . "#9EA0E5")
        ("blue-d"   . "#00629D")
        ("blue-l"   . "#69B7F0")
        ("cyan-d"   . "#00736F")
        ("cyan-l"   . "#69CABF")
        ("green-d"  . "#546E00")
        ("green-l"  . "#B4C342")))


(provide 'palette-solarized-darker)
