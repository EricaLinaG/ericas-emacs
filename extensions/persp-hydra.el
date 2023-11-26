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

(defun emms-persp ()
  "Start mpd, start emms,start playlist and lock it to the queue."
  (interactive)
  (mpd/start-music-daemon)
  (split-window-right)
  (emms-browser)
  ;; (turn-off-evil-mode)
  (split-window-right)
  (emms-playlist-mode-go)
  (emms-lock-queue))

(defhydra hydra-persp (:hint nil)
  "
     Perspectives
     Currently: %s(persp-names)

  ^Navigation^      ^Actions^        ^Buffers^
--^-^---------------^-^--------------^-^---------------
  _n_: next         _s_: Switch      _a_: add
  _p_: previous     _r_: rename      _R_: Remove
  _#_: switch #     _k_: kill        _A_: Set
                  _i_: import      _b_: switch
    <-arrows->    _m_: merge       _B_: scratch
      1-9         _u_: unmerge     _g_: global add
                  _S_: save
  _q_: quit         _l_: load

  ^Windows^
  _v_: vert       _x_: horiz

  ^Create new perspective by topic.^
  _t c_: Custom
  _t E_: Emacsn      _t d_: Emacsn dev   _t s_: Emacsn stable
  _t q_: myQMK       _t b_: BD           _t m_: Music
  _t S_: Scad 3D     _t z_: SPR          _t x_: Xmonad
  _t e_: Emms dev    _t M_: Mail
"
  ("q" nil)
  ("a" persp-add-buffer :exit t)

  ("t c" (layout-3 nil nil))
  ("t d" (layout-3 "~/Emacsn/dev/" "Emacsn dev" "README.org") :exit t)
  ("t s" (layout-3 "~/Emacsn/stable/" "Emacsn stable" "README.org") :exit t)
  ("t x" (layout-3 "~/.xmonad/"    "Xmonad" "xmonad.hs") :exit t)
  ("t E" (layout-3 "~/Emacsn/"     "Emacsn" "README.org") :exit t)
  ("t S" (layout-3 "~/play/3D"     "3D"     "README.org"))
  ("t q" (layout-3 "~/play/myQMK" "myQMK" "README.org") :exit t)
  ("t M" (progn (layout-3 "~/" "Email" nil) (mu4e) :exit t))
  ("t m" (progn (layout-0 "/home/Music"   "Music"  "README.org")
                (emms-persp))
   :exit t)
  ("t b" (layout-3 "/home/BD"       "BD"     "README.org"))
  ("t z" (layout-3 "~/play/SPR/"    "Emacsn" "README.org"))
  ("t e" (layout-3 "~/play/emms/" "Emms dev" "README"))

  ("v" split-window-right)
  ("x" split-window-below)

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
