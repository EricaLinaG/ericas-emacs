
;; popwin should be deprecated. It no longer plays nice with
;; display-buffer in window.el.  Since emacs 24.7.  popwin is best avodided.

(setq display-buffer-alist '(("\\*shell" 
			      (display-buffer-reuse-window display-buffer-same-window))
			     ("\\*help" 
			      (display-buffer-reuse-window display-buffer-in-side-window)
			      (side . right)
			      (window-width . 80))))

;; (setq display-buffer-alist '(("*R"
;;                               (display-buffer-reuse-window display-buffer-in-side-window)
;;                               (side . right)
;;                               (slot . -1)
;;                               (window-width . 0.5)
;;                               (reusable-frames . nil))
;; 			     ("*Help"
;;                               (display-buffer-reuse-window display-buffer-in-side-window)
;;                               (side . right)
;;                               (slot . 1)
;;                               (window-width . 0.5)
;;                               (reusable-frames . nil))))


;; (setq popwin:special-display-config
;;       '(("*Help*"  :height 30)
;; 	(" *transient*" :original t)
;;         ("*Completions*" :noselect t)
;;         ("*Messages*" :noselect t :height 30)
;;         ("*Apropos*" :noselect t :height 30)
;;         ("*compilation*" :noselect t)
;;         ("*Backtrace*" :height 30)
;;         ("*Messages*" :height 30)
;;         ("*Occur*" :noselect t)
;;         ("*Ido Completions*" :noselect t :height 30)
;;         ;;("*magit-commit*" :noselect nil :height 40 :width 80 :stick t)
;;         ;;("*magit-diff*" :noselect t :height 40 :width 80)
;;         ;;("*magit-edit-log*" :noselect t :height 15 :width 80)
;;         ("\\*ansi-term\\*.*" :regexp t :height 30)
;;         ("*shell*" :height 30)
;;         (".*overtone.log" :regexp t :height 30)
;;         ("*gists*" :height 30)
;;         ("*sldb.*":regexp t :height 30)
;;         ("*cider-error*" :height 30 :stick t)
;;         ("*cider-doc*" :height 30 :stick t)
;;         ("*cider-src*" :height 30 :stick t)
;;         ("*cider-result*" :height 30 :stick t)
;;         ("*cider-macroexpansion*" :height 30 :stick t)
;;         ("*Kill Ring*" :height 30)
;;         ("*Compile-Log*" :height 30 :stick t)
;;         ("*git-gutter:diff*" :height 30 :stick t)))


