;;; Package --- Summary
;;; Commentary:
;;; hydra for perspective
;;; Code:

(defhydra hydra-persp (:hint nil)
  "
Layouts %s(hydra-perse-names)

^Navigation^      ^Selection^       ^Actions^        ^Buffers^
^-^---------------^-^---------------^-^--------------^-^------------
_n_: next         _l_: choose       _d_: delete      _a_: add buffer
_p_: previous     _L_: predefined   _r_: rename      _R_: Remove
_c_: cancel       _#_: switch #     _k_: kill        _A_: Set
                                    _i_: import      _b_: switch
                                    _m_: merge       _B_: scratch
                                    _u_: unmerge     _g_: global add
                                    _s_: save        _g_: global add
                                    _l_: load        _g_: global add

"
  ("a" persp-add-buffer :exit t)

  ;; ("d" hfj/persp-kill-current)
  ;; ("l" hfj/pick-layout :exit t)
  ;; ("L" hfj/pick-predefined-layout :exit t)

  ("r" persp-rename :exit t)
  ("n" persp-next)
  ("p" persp-prev)

  ("c" nil)
  ("S" 'persp-switch)
  ("R" 'persp-remove-buffer)
  ("k" 'persp-kill)
  ("r" 'persp-rename :exit t)
  ("a" 'persp-add-buffer: exit t)
  ("A" 'persp-set-buffer)
  ("b" 'persp-switch-to-buffer)
  ("B" 'persp-switch-to-scratch-buffer)
  ("i" 'persp-import)
  ("n" 'persp-next)
  ("<right>" 'persp-next)
  ("p" 'persp-prev)
  ("<left>" 'persp-prev)
  ("m" 'persp-merge)
  ("u" 'persp-unmerge)
  ("g" 'persp-add-buffer-to-frame-global)
  ("s" 'persp-state-save)
  ("l" 'persp-state-load)
  ("#" 'persp-switch-by-number)

  ("1" hfj/persp-switch-to-1 :exit t)
  ("2" hfj/persp-switch-to-2 :exit t)
  ("3" hfj/persp-switch-to-3 :exit t)
  ("4" hfj/persp-switch-to-4 :exit t)
  ("5" hfj/persp-switch-to-5 :exit t)
  ("6" hfj/persp-switch-to-6 :exit t)
  ("7" hfj/persp-switch-to-7 :exit t)
  ("8" hfj/persp-switch-to-8 :exit t)
  ("9" hfj/persp-switch-to-9 :exit t)
  ("0" hfj/persp-switch-to-10 :exit t))

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
