;; Emacs custom shell that I used to customize the eshell prompt on my Mac running macOS Big Sur
;; I copied this file in my .emacs.d/eshell/ directory and load the file in my profile file in the same directory
;; Note: You must install packages `dash`, `s`, `magit`, and `all-the-icons` and then run `M-x all-the-icons-install-fonts`
;; I did so using MELPA and it worked fine
;; Please see https://github.com/domtronn/all-the-icons.el for usage of icons

(require 'dash)
(require 's)
(require 'cl)
(require 'magit)
(require 'all-the-icons)

;; Use 'prepend for the NS and Mac ports or Emacs will crash.
(set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'prepend)
(set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'prepend)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
         (lambda () (when ,FORM
                 (-> ,ICON
                     (concat esh-section-delim ,FORM)
                     (with-face ,@PROPS))))))

(defun esh-acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

(defun esh-prompt-func ()
  "Build `eshell-prompt-function'"
  (concat esh-header
          (-reduce-from 'esh-acc "" eshell-funcs)
          "\n"
          eshell-prompt-string))

(esh-section esh-dir
             (all-the-icons-material "folder")  ;  (faicon folder)
             (abbreviate-file-name (eshell/pwd))
             '(:foreground "gold"))

(esh-section esh-python
             (all-the-icons-faicon "py")  ;  (python icon)
             pyvenv-virtual-env-name)

(esh-section esh-git
             (all-the-icons-octicon "git-branch")  ;  (git icon)
             (magit-get-current-branch)
             '(:foreground "pink"))

(esh-section esh-clock
             (all-the-icons-wicon "time-2")  ;  (clock icon)
             (format-time-string "%H:%M | %a %b %d %Y" (current-time))
             '(:foreground "forest green"))

;; Below I implement a "prompt number" section
(setq esh-prompt-num 0)

(add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))

(advice-add 'eshell-send-input :before
            (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))

(esh-section esh-num
             (all-the-icons-material "list") ;  (list icon)
             (number-to-string esh-prompt-num)
             '(:foreground "brown"))

;; Separator between esh-sections
(setq esh-sep " | ")  ; or " | "

;; Separator between an esh-section icon and form
(setq esh-section-delim " ")

;; Eshell prompt header
(setq esh-header "\n┌─")  ; or "\n┌─"

;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
;; your login, these can be the same.
(setq eshell-prompt-regexp "└─> ")   ; or "└─> "
(setq eshell-prompt-string "└─> ")   ; or "└─> "

;; Choose which eshell-funcs to enable
(setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))

;; Enable the new eshell prompt
(setq eshell-prompt-function 'esh-prompt-func)
