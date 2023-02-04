;;; Package --- Summary
;;; Commentary:

(require 'google-translate)
(require 'thingatpt)

;; this should really be a single function with an Alist.

;;; Code:
(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.  Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings and comments get checked.  All other buffers get `flyspell-mode' to check all text.  If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
	(if (derived-mode-p 'prog-mode)
	    (progn
	      (message "Flyspell on (code)")
	      (flyspell-prog-mode))
	  ;; else
	  (progn
	    (message "Flyspell on (text)")
	    (flyspell-mode 1)))
	;; I tried putting (flyspell-buffer) here but it didn't seem to work
	)))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
	(message "Flyspell off")
	(flyspell-mode -1))
                                        ; else - flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))


(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "deutsch8") "english" "deutsch8")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(defun french-input-dict ()
  "This function is to set the input method and dictionary to French.
The input method is French-prefix and the Ispell dictionary to
French recommended."
  (interactive)
  (set-input-method 'french-prefix)
  (ispell-change-dictionary  "fr_FR_Recommended"))

(defun english-input-dict ()
  "This function is to set the input method and dictionary to English.
The input method is deactivated and the Ispell dictionary to english."
  (interactive)
  (deactivate-input-method)
  (ispell-change-dictionary "english"))

(setq input-methods-and-dictionaries
      '(("english"
         (input-method . nil)
         (dictionary  . "english")
         (lang-tool . "en-US"))
        ("french"
         (input-method . french-prefix)
         (dictionary  .  "fr_FR_Recommended")
         (lang-tool . "fr"))
        ))

;;(setq langtool-default-language "en-US")
(defun set-input-and-dictionary ()
  "Set the input method and dictionary."
  (interactive)
  (let* ((lang
          (ido-completing-read
           (format "Switch to input language: (%s) "
                   (mapconcat #'(lambda (var) (car var))
                              input-methods-and-dictionaries "/"))
           (mapcar #'(lambda (var) (car var)) input-methods-and-dictionaries)))
         (lang-vars (cdr (assoc lang input-methods-and-dictionaries))))
    (if lang-vars
        (progn
          (ispell-change-dictionary (cdr (assoc 'dictionary lang-vars)))
          (if (cdr (assoc 'input-method lang-vars))
              (activate-input-method (cdr (assoc 'input-method lang-vars)))
            (deactivate-input-method))
          (if (cdr (assoc 'lang-tool lang-vars))
              (setq langtool-default-language (cdr (assoc 'lang-tool lang-vars)))
            (setq langtool-default-language nil)))
      (error (format "No language found: (%s)" lang)))))

(defun google-translate-smooth-translate-sentence-at-point ()
  "Translate the sentence at point using translation directions.

Make a prompt in minibuffer for a text to translate. Default text
is word at point.

In case of `google-translate-translation-directions-alist' is
empty list then after inputed translating text prompts for source
language and then for target languages.

In case of `google-translate-translation-directions-alist' is not
empty list takes current translation direction and makes
appropriate translation. Current translation direction indicates
in the minibuffers' prompt.

A current translation direction could be changed directly in the
minibuffer by means of key bindings such as C-n and C-p for
changing to the next translation direction and to the previous
one respectively."
  (interactive)

  (setq google-translate-translation-direction-query
        (sentence-at-point))

  (setq google-translate-current-translation-direction 0)

  (let* ((text (google-translate-query-translate-using-directions))
         (source-language (google-translate--current-direction-source-language))
         (target-language (google-translate--current-direction-target-language)))
    (when (null source-language)
      (setq source-language (google-translate-read-source-language)))
    (when (null target-language)
      (setq target-language (google-translate-read-target-language)))
    (google-translate-translate source-language target-language text)))

(provide 'language)
;;; language.el ends here
