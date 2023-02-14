;;; Package --- Summary
;;; Commentary:
;;; hydra for perspective
;;; Code:

(require 'hydra)
(require 'perspective)

(defhydra hydra-persp (:hint nil)
  "
     Perspectives
     Currently: %s(persp-names)

  ^Navigation^       ^Actions^        ^Buffers^
--^-^---------------^-^--------------^-^---------------
  _n_: next         _S_: Switch      _a_: add
  _p_: previous     _r_: rename      _R_: Remove
  _#_: switch #     _k_: kill        _A_: Set
                  _i_: import      _b_: switch
    <-arrows->    _m_: merge       _B_: scratch
      1-9         _u_: unmerge     _g_: global add
                  _s_: save
  _c_: cancel       _l_: load

"
  ("a" persp-add-buffer :exit t)

  ;; ("d" hfj/persp-kill-current)
  ;; ("l" hfj/pick-layout :exit t)
  ;; ("L" hfj/pick-predefined-layout :exit t)

  ("N" persp-next)
  ("r" persp-rename :exit t)
  ("n" persp-next)
  ("p" persp-prev)

  ("c" nil)
  ("S" persp-switch)
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
  ("s" persp-state-save)
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
