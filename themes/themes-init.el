(require 'bytecompiledir)
;;(byte-compile-directory "~/elisp/themes/palette-themes")
;;(byte-compile-directory "~/elisp/themes/other-themes")


(add-to-list 'custom-theme-load-path "~/elisp/themes/other-themes")
(add-to-list 'custom-theme-load-path "~/elisp/themes/deviant")
(add-to-list 'custom-theme-load-path "~/elisp/themes/color-themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq custom-safe-themes t)

