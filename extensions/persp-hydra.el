;;; Package --- Summary
;;; Commentary:
;;; hydra for perspective
;;; Code:

(require 'hydra)
(require 'perspective)

;; Should make one that takes a directory, a list of files,
;; and a list of executables. Essentially Xmonad topics
(defun layout-3 (dir &optional file)
  "Change to DIR directory and open 3 windows then load the FILE if given."
  (if dir
      (cd dir)
    (cd (read-directory-name "Default Directory:")))
  (split-window-right)
  (split-window-right)
  (split-window-right)
  (windmove-right)
  (if file
      (find-file file)
    (find-file (read-file-name
                "Find file: " nil default-directory
                (confirm-nonexistent-file-or-buffer))) ))

(defhydra hydra-persp (:hint nil)
  "
     Perspectives
     Currently: %s(persp-names)

  ^Navigation^       ^Actions^        ^Buffers^
--^-^---------------^-^--------------^-^---------------
  _n_: next         _s_: Switch      _a_: add
  _p_: previous     _r_: rename      _R_: Remove
  _#_: switch #     _k_: kill        _A_: Set
                  _i_: import      _b_: switch
    <-arrows->    _m_: merge       _B_: scratch
      1-9         _u_: unmerge     _g_: global add
                  _S_: save
  _q_: quit       _l_: load

  ^Windows^
  _v_: vert       _x_: horiz

  ^Layouts^
  _P_: Prompt
  _e_: emacs dev  _E_: emacsn      _X_: xmonad
  _Q_: qmk        _D_: BD          _m_: Music
  _T_: 3D
"
  ("q" nil)
  ("a" persp-add-buffer :exit t)

  ("e" (layout-3 "~/Emacsn/dev/" "README.org"))
  ("X" (layout-3 "~/.xmonad/" "xmonad.hs"))
  ("E" (layout-3 "~/Emacsn/" "README.org"))
  ("T" (layout-3 "~/play/3D" "README.org"))
  ("Q" (layout-3 "~/play/qmk-firmware/users/ericgebhart" "README.org"))
  ("M" (layout-3 "/home/Music" "README.org"))
  ("D" (layout-3 "/home/BD" "README.org"))
  ("P" (layout-3 nil))

  ("v" split-window-right)
  ("x" split-window-below)
  ;; ("t" transpose-frame "'")
  ;; ("d" hfj/persp-kill-current)
  ;; ("l" hfj/pick-layout :exit t)
  ;; ("L" hfj/pick-predefined-layout :exit t)

  ("N" persp-next)
  ("r" persp-rename :exit t)
  ("n" persp-next)
  ("p" persp-prev)

  ("s" persp-switch)
  ("R" persp-remove-buffer)
  ("k" persp-kill)
  ("r" persp-rename :exit t)
  ("a" persp-add-buffer :exit t)
  ("A" persp-set-buffer)
  ("b" persp-switch-to-buffer)
  ("B" persp-switch-to-scratch-buffer)
  ("i" persp-import)
  ("n" persp-next)
  ("<right>" persp-next)
  ("p" persp-prev)
  ("<left>" persp-prev)
  ("m" persp-merge)
  ("u" persp-unmerge)
  ("g" persp-add-buffer-to-frame-global)
  ("S" persp-state-save)
  ("l" persp-state-load)
  ("#" persp-switch-by-number)
  ("1" '(persp-switch-by-number 1))
  ("2" '(persp-switch-by-number 2))
  ("3" '(persp-switch-by-number 3))
  ("4" '(persp-switch-by-number 4))
  ("5" '(persp-switch-by-number 5))
  ("6" '(persp-switch-by-number 6))
  ("7" '(persp-switch-by-number 7))
  ("8" '(persp-switch-by-number 8))
  ("9" '(persp-switch-by-number 9))
  )


;; from the real perspective.el keymap
;; (define-key perspective-map (kbd "1") (lambda () (interactive) (persp-switch-by-number 1)))
;; (define-key perspective-map (kbd "2") (lambda () (interactive) (persp-switch-by-number 2)))
;; (define-key perspective-map (kbd "3") (lambda () (interactive) (persp-switch-by-number 3)))
;; (define-key perspective-map (kbd "4") (lambda () (interactive) (persp-switch-by-number 4)))
;; (define-key perspective-map (kbd "5") (lambda () (interactive) (persp-switch-by-number 5)))
;; (define-key perspective-map (kbd "6") (lambda () (interactive) (persp-switch-by-number 6)))
;; (define-key perspective-map (kbd "7") (lambda () (interactive) (persp-switch-by-number 7)))
;; (define-key perspective-map (kbd "8") (lambda () (interactive) (persp-switch-by-number 8)))
;; (define-key perspective-map (kbd "9") (lambda () (interactive) (persp-switch-by-number 9)))
;; (define-key perspective-map (kbd "0") (lambda () (interactive) (persp-switch-by-number 10)))

(provide 'persp-hydra)
;;; persp-hydra.el ends here
