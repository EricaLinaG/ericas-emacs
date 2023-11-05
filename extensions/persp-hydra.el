;;; Package --- Summary
;;; Commentary:
;;; hydra for perspective
;;; Code:

(require 'hydra)
(require 'perspective)

;; Should make one that takes a directory, a list of files,
;; and a list of executables. Essentially Xmonad topics
(defun layout-3 (dir name &optional file)
  "Change to DIR directory and open 3 windows then load the FILE if given."
  (persp-switch name)
  (if dir
      (cd dir)
    (cd (read-directory-name "Default Directory:")))
  (split-window-right)
  (split-window-right)
  (windmove-right)
  (if file
      (find-file file)
    (find-file (read-file-name
                "Find file: " nil default-directory
                (confirm-nonexistent-file-or-buffer))) ))

(defun layout-0 (dir name &optional file)
  "Create perspective of NAME, Change to DIR directory and open 0 windows then load the FILE if given."
  (persp-switch name)
  (if dir
      (cd dir)
    (cd (read-directory-name "Default Directory:")))
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

  ^Create new perspective.^
  _c_: Custom
  _e_: Emacs dev  _E_: Emacsn      _X_: Xmonad
  _Q_: myQMK      _D_: BD          _M_: Music
  _T_: 3D         _z_: SPR         _Z_: Emacsn stable
"
  ("q" nil)
  ("a" persp-add-buffer :exit t)

  ("c" (layout-3 nil nil))
  ("e" (layout-3 "~/Emacsn/dev/" "Emacsn dev" "README.org") :exit t)
  ("Z" (layout-3 "~/Emacsn/stable/" "Emacsn stable" "README.org") :exit t)
  ("X" (layout-3 "~/.xmonad/"    "Xmonad" "xmonad.HS") :exit t)
  ("E" (layout-3 "~/Emacsn/"     "Emacsn" "README.org") :exit t)
  ("T" (layout-3 "~/play/3D"     "3D"     "README.org"))
  ("Q" (layout-3 "~/play/myQMK" "myQMK" "README.org") :exit t)
  ("M" (progn (layout-0 "/home/Music"   "Music"  "README.org")
              (mpd/start-music-daemon)
              (split-window-right)
              (emms-browser)
              (turn-off-evil-mode)
              (split-window-right)
              (emms-playlist-mode-go)) :exit t)
  ("D" (layout-3 "/home/BD"      "BD"     "README.org"))
  ("z" (layout-3 "~/play/SPR/"   "Emacsn" "README.org"))

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

  ("s" persp-switch :exit t)
  ("R" persp-remove-buffer)
  ("k" persp-kill)
  ("r" persp-rename :exit t)
  ("a" persp-add-buffer :exit t)
  ("A" persp-set-buffer)
  ("b" persp-switch-to-buffer)
  ("B" persp-switch-to-scratch-buffer)
  ("i" persp-import)
  ("<right>" persp-next)
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
