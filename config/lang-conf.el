(require 'ispell)
(require 'flyspell)
(require 'langtool)

;; dictionaries for hunspell can be found here.
;; the otx and ipx must be unzipped and placed in /Library/Spelling
;; https://wiki.openoffice.org/wiki/Dictionaries
;; firefox and mozilla dictionaries may also be used.

;;; Code:

;;(setq input-method-verbose-flag t) ;; nil, t, complex-only or default
(setq input-method-highlight-flag t) ;; t nil.

;;; Ispell and Fly Spell.

(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                js-mode-hook
                R-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))

;;; F8 to check a word, M-F8 to check the next one.
;; (global-set-key (kbd "<f8>") 'ispell-word)
;; (global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)


;; You can make ispell.el use Hunspell with
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))


;; Enable Lang tool.

;; home brew installs languagetool in /usr/local/bin, but the jar is in the Cellar.
(setq langtool-language-tool-jar "/usr/share/java/languagetool/languagetool-commandline.jar"
      langtool-java-bin "/usr/bin/java"
      langtool-mother-tongue "en"
      ;; turn off some rules.
      langtool-disabled-rules '("WHITESPACE_RULE"
                                "EN_UNPAIRED_BRACKETS"
                                "COMMA_PARENTHESIS_WHITESPACE"
                                "EN_QUOTES"))

;;* Default language is detected by LANG/LC_ALL environment variable.
;;Please set `langtool-default-language` if you need to change default value.
;; (setq langtool-default-language "en-US")

(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")

      ;;These settings are optional:

      ;; Key binding if you desired.

      ;; (global-set-key "\C-x4w" 'langtool-check)
      ;; (global-set-key "\C-x4W" 'langtool-check-done)
      ;; (global-set-key "\C-x4l" 'langtool-switch-default-language)
      ;; (global-set-key "\C-x44" 'langtool-show-message-at-point)
      ;; (global-set-key "\C-x4c" 'langtool-correct-buffer)


      ;; Otherwise, invoke `M-x langtool-check` with `C-u` (universal-argument)


      ;; ## Usage:
      ;; To check current buffer and show warnings.

      ;; M-x langtool-check

      ;; Check with different language. You can complete supported language
      ;; with C-i/TAB

      ;; C-u M-x langtool-check

      ;; * To correct marker follow LanguageTool suggestions.

      ;; M-x langtool-correct-buffer

      ;; * Go to warning point you can see a report from LanguageTool.
      ;; Otherwise:

      ;; M-x langtool-show-message-at-point


      ;; * To check current buffer and show warnings.
      ;; * Show LanguageTool report automatically by `popup'
      ;; This idea come from:
      ;; http://d.hatena.ne.jp/LaclefYoshi/20150912/langtool_popup

      (defun langtool-autoshow-detail-popup (overlays)
        (when (require 'popup nil t)
          ;; Do not interrupt current popup
          (unless (or popup-instances
                      ;; suppress popup after type `C-g' .
                      (memq last-command '(keyboard-quit)))
            (let ((msg (langtool-details-error-message overlays)))
              (popup-tip msg)))))

(setq langtool-autoshow-message-function
      'langtool-autoshow-detail-popup)

;; * To finish checking. All langtool marker is removed.
;; M-x langtool-check-done






;; Commands are: langtool-check-buffer,langtool-correct-buffer
