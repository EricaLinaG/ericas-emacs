;;; Package --- Summary
;;; Commentary:

(require 'google-translate)
;;(require 'google-translate-default-ui)
;;(global-set-key "\C-ct" 'google-translate-at-point)
;;(global-set-key "\C-cT" 'google-translate-query-translate)

;;; Code:

(setq google-translate-default-source-language "fr")
(setq google-translate-default-target-language "en")

(global-set-key (kbd "C-c r") 'google-translate-at-point-reverse)
(global-set-key (kbd "C-c R") 'google-translate-query-translate-reverse)

(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key "\C-ct" 'google-translate-smooth-translate)
(setq google-translate-translation-directions-alist
      '(("fr" . "en") ("en" . "fr")
        ("en" . "it") ("it" . "en")
        ("fr" . "it") ("it" . "fr")
        ("en" . "es") ("es" . "en")
        ("it" . "es") ("es" . "it")
        ("ps" . "en") ("en" . "ps")) )

(setq google-translate-preferable-input-methods-alist '((nil . ("en"))
                                                        (french-prefix . ("fr"))))

(setq google-translate-enable-ido-completion t)
(setq google-translate-show-phonetic t)

;; google-translate-listen-program
;; google-translate-output-destination
;; google-translate-enable-ido-completion
;; google-translate-pop-up-buffer-set-focus

(provide 'google-translate-conf)
;;; google-translate-conf ends here
