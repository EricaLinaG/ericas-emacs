;;;package --- Summary
;;;Commentary
;;;Code
(require 'lsp-mode)

;; automatic generation of hydra from a list.
;; from Sacha Chua
;; bindings:
;; key	function	column
;; <	lispy-barf
;; A	lispy-beginning-of-defun
;; j	lispy-down
;; Z	lispy-edebug-stop
;; B	lispy-ediff-regions
;; G	lispy-goto-local
;; h	lispy-left
;; N	lispy-narrow
;; y	lispy-occur
;; o	lispy-other-mode
;; J	lispy-outline-next
;; K	lispy-outline-prev
;; P	lispy-paste
;; l	lispy-right
;; I	lispy-shifttab
;; >	lispy-slurp
;; SPC	lispy-space
;; xB	lispy-store-region-and-buffer
;; u	lispy-undo
;; k	lispy-up
;; v	lispy-view
;; V	lispy-visit
;; W	lispy-widen
;; D	pop-tag-mark
;; x	see
;; L	unbound
;; U	unbound
;; X	unbound
;; Y	unbound
;; H	lispy-ace-symbol-replace	Edit
;; c	lispy-clone	Edit
;; C	lispy-convolute	Edit
;; n	lispy-new-copy	Edit
;; O	lispy-oneline	Edit
;; r	lispy-raise	Edit
;; R	lispy-raise-some	Edit
;; \	lispy-splice	Edit
;; S	lispy-stringify	Edit
;; i	lispy-tab	Edit
;; xj	lispy-debug-step-in	Eval
;; xe	lispy-edebug	Eval
;; xT	lispy-ert	Eval
;; e	lispy-eval	Eval
;; E	lispy-eval-and-insert	Eval
;; xr	lispy-eval-and-replace	Eval
;; p	lispy-eval-other-window	Eval
;; q	lispy-ace-paren	Move
;; z	lispy-knight	Move
;; s	lispy-move-down	Move
;; w	lispy-move-up	Move
;; t	lispy-teleport	Move
;; Q	lispy-ace-char	Nav
;; –	lispy-ace-subword	Nav
;; a	lispy-ace-symbol	Nav
;; b	lispy-back	Nav
;; d	lispy-different	Nav
;; f	lispy-flow	Nav
;; F	lispy-follow	Nav
;; g	lispy-goto	Nav
;; xb	lispy-bind-variable	Refactor
;; xf	lispy-flatten	Refactor
;; xc	lispy-to-cond	Refactor
;; xd	lispy-to-defun	Refactor
;; xi	lispy-to-ifs	Refactor
;; xl	lispy-to-lambda	Refactor
;; xu	lispy-unbind-variable	Refactor
;; M	lispy-multiline	Other
;; xh	lispy-describe	Other
;; m	lispy-mark-list	Other

;; #+begin_src emacs-lisp :var bindings=bindings :colnames yes:
;; (eval
;;  (append
;;   '(defhydra my/lispy-cheat-sheet (:hint nil :foreign-keys run)
;;      ("<f14>" nil :exit t))
;;   (cl-loop for x in bindings
;;            unless (string= "" (elt x 2))
;;            collect
;;            (list (car x)
;;                  (intern (elt x 1))
;;                  (when (string-match "lispy-\\(?:eval-\\)?\\(.+\\)"
;;                                      (elt x 1))
;;                    (match-string 1 (elt x 1)))
;;                  :column
;;                  (elt x 2)))))
;; (with-eval-after-load "lispy"
;;   (define-key lispy-mode-map (kbd "<f14>") 'my/lispy-cheat-sheet/body))


(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))


(defhydra hydra-mu4e-headers (:color blue :hint nil)
  "
 ^General^   | ^Search^           | _!_: read    | _#_: deferred  | ^Switches^
-^^----------+-^^-----------------| _?_: unread  | _%_: pattern   |-^^------------------
_n_: next    | _s_: search        | _r_: refile  | _&_: custom    | _O_: sorting
_p_: prev    | _S_: edit prev qry | _u_: unmk    | _+_: flag      | _P_: threading
_]_: n unred | _/_: narrow search | _U_: unmk *  | _-_: unflag    | _Q_: full-search
_[_: p unred | _b_: search bkmk   | _d_: trash   | _T_: thr       | _V_: skip dups
_y_: sw view | _B_: edit bkmk     | _D_: delete  | _t_: subthr    | _W_: include-related
_R_: reply   | _{_: previous qry  | _m_: move    |-^^-------------+-^^------------------
_C_: compose | _}_: next query    | _a_: action  | _|_: thru shl  | _`_: update, reindex
_F_: forward | _C-+_: show more   | _A_: mk4actn | _H_: help      | _;_: context-switch
_o_: org-cap | _C--_: show less   | _*_: *thing  | _q_: quit hdrs | _j_: jump2maildir "

  ;; general
  ("n" mu4e-headers-next)
  ("p" mu4e-headers-previous)
  ("[" mu4e-select-next-unread)
  ("]" mu4e-select-previous-unread)
  ("y" mu4e-select-other-view)
  ("R" mu4e-compose-reply)
  ("C" mu4e-compose-new)
  ("F" mu4e-compose-forward)
  ("o" my/org-capture-mu4e)                  ; differs from built-in

  ;; search
  ("s" mu4e-headers-search)
  ("S" mu4e-headers-search-edit)
  ("/" mu4e-headers-search-narrow)
  ("b" mu4e-headers-search-bookmark)
  ("B" mu4e-headers-search-bookmark-edit)
  ("{" mu4e-headers-query-prev)              ; differs from built-in
  ("}" mu4e-headers-query-next)              ; differs from built-in
  ("C-+" mu4e-headers-split-view-grow)
  ("C--" mu4e-headers-split-view-shrink)

  ;; mark stuff
  ("!" mu4e-headers-mark-for-read)
  ("?" mu4e-headers-mark-for-unread)
  ("r" mu4e-headers-mark-for-refile)
  ("u" mu4e-headers-mark-for-unmark)
  ("U" mu4e-mark-unmark-all)
  ("d" mu4e-headers-mark-for-trash)
  ("D" mu4e-headers-mark-for-delete)
  ("m" mu4e-headers-mark-for-move)
  ("a" mu4e-headers-action)                  ; not really a mark per-se
  ("A" mu4e-headers-mark-for-action)         ; differs from built-in
  ("*" mu4e-headers-mark-for-something)

  ("#" mu4e-mark-resolve-deferred-marks)
  ("%" mu4e-headers-mark-pattern)
  ("&" mu4e-headers-mark-custom)
  ("+" mu4e-headers-mark-for-flag)
  ("-" mu4e-headers-mark-for-unflag)
  ("t" mu4e-headers-mark-subthread)
  ("T" mu4e-headers-mark-thread)

  ;; miscellany
  ("q" mu4e~headers-quit-buffer)
  ("H" mu4e-display-manual)
  ("|" mu4e-view-pipe)                       ; does not seem built-in any longer

  ;; switches
  ("O" mu4e-headers-change-sorting)
  ("P" mu4e-headers-toggle-threading)
  ("Q" mu4e-headers-toggle-full-search)
  ("V" mu4e-headers-toggle-skip-duplicates)
  ("W" mu4e-headers-toggle-include-related)

  ;; more miscellany
  ("`" mu4e-update-mail-and-index)           ; differs from built-in
  (";" mu4e-context-switch)
  ("j" mu4e~headers-jump-to-maildir)

  ("." nil))


(defhydra hydra-ediff (:color blue :hint nil)
  "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                  _c_urrent file
"
  ("b" ediff-buffers)
  ("B" ediff-buffers3)
  ("=" ediff-files)
  ("f" ediff-files)
  ("F" ediff-files3)
  ("c" ediff-current-file)
  ("r" ediff-revision)
  ("l" ediff-regions-linewise)
  ("w" ediff-regions-wordwise))


(defhydra hydra-hs (:idle 1.0)
  "
Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_oggle    _n_ext line
_d_ hide block    _a_ show block              _p_revious line
_l_ hide level

_SPC_ cancel
"
  ("s" hs-show-all)
  ("h" hs-hide-all)
  ("a" hs-show-block)
  ("d" hs-hide-block)
  ("t" hs-toggle-hiding)
  ("l" hs-hide-level)
  ("n" forward-line)
  ("p" (forward-line -1))
  ("SPC" nil)
  )

;;(global-set-key (kbd "C-c @") 'hydra-hs/body) # Example binding



(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                      :hint nil)
  "
Git gutter:
  _t_: next hunk        _s_tage hunk     _q_uit
  _n_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _s_: last hunk        set start _R_evision
"
  ("t" git-gutter:next-hunk)
  ("n" git-gutter:previous-hunk)
  ("s" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("h" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
   :color blue))


(defhydra hydra-flycheck
  (:pre (flycheck-list-errors)
        :post (quit-windows-on "*Flycheck errors*")
        :hint nil)
  "Errors"
  ("f" flycheck-error-list-set-filter "Filter")
  ("j" flycheck-next-error "Next")
  ("k" flycheck-previous-error "Previous")
  ("gg" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q" nil))

(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

(defhydra hydra-projectile (:color teal
                                   :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))


(defhydra hydra-helm-like-unite (:hint nil
                                       :color pink)
  "
Nav ^^^^^^^^^        Mark ^^          Other ^^       Quit
^^^^^^^^^^------------^^----------------^^----------------------
_K_ ^ ^ _k_ ^ ^     _m_ark           _v_iew         _i_: cancel
^↕^ _h_ ^✜^ _l_     _t_oggle mark    _H_elp         _o_: quit
_J_ ^ ^ _j_ ^ ^     _U_nmark all     _d_elete
^^^^^^^^^^                           _f_ollow: %(helm-attr 'follow)
"
  ;; arrows
  ("h" helm-beginning-of-buffer)
  ("j" helm-next-line)
  ("k" helm-previous-line)
  ("l" helm-end-of-buffer)
  ;; beginning/end
  ("g" helm-beginning-of-buffer)
  ("G" helm-end-of-buffer)
  ;; scroll
  ("K" helm-scroll-other-window-down)
  ("J" helm-scroll-other-window)
  ;; mark
  ("m" helm-toggle-visible-mark)
  ("t" helm-toggle-all-marks)
  ("U" helm-unmark-all)
  ;; exit
  ("<escape>" keyboard-escape-quit "" :exit t)
  ("o" keyboard-escape-quit :exit t)
  ("i" nil)
  ;; sources
  ("}" helm-next-source)
  ("{" helm-previous-source)
  ;; rest
  ("H" helm-help)
  ("v" helm-execute-persistent-action)
  ("d" helm-persistent-delete-marked)
  ("f" helm-follow-mode))

;; (define-key helm-map (kbd "<escape>") 'helm-like-unite/body)
;; (define-key helm-map (kbd "C-k") 'helm-like-unite/body)
;; (define-key helm-map (kbd "C-o") 'helm-like-unite/body)


(defhydra dh-hydra-markdown-mode (:hint nil)
  "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code

Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4

Lists             C-c C-x    _m_: insert item

Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down

Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference

"


  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("c" markdown-insert-code)

  ("h" markdown-insert-header-dwim)
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("l" markdown-promote)
  ("r" markdown-demote)
  ("d" markdown-move-down)
  ("u" markdown-move-up)

  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("R" markdown-insert-reference-link-dwim :color blue)
  )


;;(global-set-key [f9] 'dh-hydra-markdown-mode/body)

(defhydra hydra-window-orig (:color red :hint nil)
  "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _S_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
  ("h" windmove-left)
  ("t" windmove-down)
  ("n" windmove-up)
  ("s" windmove-right)
  ("H" hydra-move-splitter-left)
  ("T" hydra-move-splitter-down)
  ("N" hydra-move-splitter-up)
  ("S" hydra-move-splitter-right)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("v" split-window-right)
  ("x" split-window-below)
                                        ;("t" transpose-frame "'")
  ;; winner-mode must be enabled
  ("u" winner-undo)
  ("r" winner-redo) ;;Fixme, not working?
  ("o" delete-other-windows :exit t)
  ("a" ace-window :exit t)
  ("f" new-frame :exit t)
  ("S" ace-swap-window)
  ("da" ace-delete-window)
  ("dw" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t)
  ("q" nil)
                                        ;("i" ace-maximize-window "ace-one" :color blue)
                                        ;("b" ido-switch-buffer "buf")
  ("m" headlong-bookmark-jump))

(defun hydra-universal-argument (arg)
  (interactive "P")
  (setq prefix-arg (if (consp arg)
                       (list (* 4 (car arg)))
                     (if (eq arg '-)
                         (list -4)
                       '(4)))))

(defhydra hydra-window2 (:hint nil)
  "window"
  ("h" windmove-left "left")
  ("t" windmove-down "down")
  ("n" windmove-up "up")
  ("s" windmove-right "right")
  ("a" (lambda () (interactive) (ace-window t)))
  ("u" hydra-universal-argument "universal")
  ("S" (lambda () (interactive) (ace-window 4)) "swap")
  ("d" (lambda () (interactive) (ace-window 16)) "delete"))

(defhydra hydra-winmove (:color red :hint nil)
  "
 Move or jump to windows and buffers
 Move^^        ^Jump^
 --------------------------------------------------
  _h_ Left        _j_ Jump Window
  _t_ UP          _b_ Buffer jump
  _n_ Down        _B_ Buffer other window
  _s_ Right
  _o_ Other window
  --------------------------------------------------
  _S_wap window  _d_elete window  _a_ce Delete window
"

  ("h"   windmove-left)
  ("t"   windmove-down)
  ("n"   windmove-up)
  ("s"   windmove-right)
  ("o"   other-window :exit t)
  ("j"   ace-window :exit t)
  ("b"   ace-jump-buffer :exit t)
  ("B"   ace-jump-buffer-other-window :exit t)

  ("S"   ace-swap-window :exit t)
  ("d"   delete-window :exit t)
  ("a"   ace-delete-window :exit t)

  ;; ("S"   (lambda () (interactive) (ace-window 4)) swap)
  ;; ("d"   (lambda () (interactive) (ace-window 16)))

  ("q" nil))

;; note that splitter control doesn´t work when using golden ratio.
(defhydra hydra-window (:color red :hint nil)
  "
 Move/jump windows and buffers
 ------------------------------------------------------------------
 Move:     _h_ Left   _t_ Up   _n_ Down   _s_ Right   _o_ other window
 Splitter: _H_ Left   _T_ Up   _N_ Down   _S_ Right   _q_ cancel
 ------------------------------------------------------------------
 Jump^                    Split        Delete             Swap
 ------------------------------------------------------------------
 _j_ Jump Window           _v_vert      _O_  Other windows    _s_ swap
 _b_ Buffer jump           _x_:horz     _da_ Ace              _a_ Ace
 _B_ Buffer other window              _dw_ Window
                         _|_right     _db_ Buffer
                         ___below     _df_ Frame
 New:    _n_ew window    _N_ew Frame
 Winner: _u_ndo          _r_edo
 ------------------------------------------------------------------
  "

  ("h"   windmove-left)
  ("t"   windmove-down)
  ("n"   windmove-up)
  ("s"   windmove-right)
  ("o"   other-window :exit t)
  ("dw"   delete-window :exit t)

  ("j"   ace-window  :exit t)
  ("a"   ace-swap-window :exit t)
  ("b"   ace-jump-buffer :exit t)
  ("B"   ace-jump-buffer-other-window :exit t)
  ;;  ("s"   (lambda () (interactive) (ace-window 4)) swap)
  ;;  ("d"   (lambda () (interactive) (ace-window 16)))

  ("q" nil)

  ;; splitters and splitting
  ("H" hydra-move-splitter-left)
  ("T" hydra-move-splitter-down)
  ("N" hydra-move-splitter-up)
  ("S" hydra-move-splitter-right)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("v" split-window-right)
  ("x" split-window-below)
                                        ;("t" transpose-frame "'")
  ;; winner-mode must be enabled
  ("u" winner-undo)
  ("r" winner-redo) ;;Fixme, not working?
  ("O" delete-other-windows :exit t)
  ("N" new-frame :exit t)

  ("da" ace-delete-window :exit t)
  ("dw" delete-window :exit t)
  ("db" kill-this-buffer :exit t)
  ("df" delete-frame :exit t)
                                        ;("i" ace-maximize-window "ace-one" :color blue)
                                        ;("b" ido-switch-buffer "buf")
                                        ;("m" headlong-bookmark-jump)
  )

;; (require 'eaf)
;; (require 'eaf-browser)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-markdown-previewer)



;; (defhydra hydra-eaf (:color teal)
;;   "
;; Application Framework
;;              Browser     Buffer    PDF    Anything
;; ------------------------------------------------------
;; Open:        _u_:rl       _b_:uffer          _o_:pen
;; At point:    _U_:rl
;; W/history:   _h_:istory              _p_:df
;; "
;;   ("o"  eaf-open)
;;   ("b"  eaf-open-this-buffer)
;;   ("u"  eaf-open-browser)
;;   ("U"  eaf-open-url-at-point)
;;   ("h"  eaf-open-browser-with-history)
;;   ("p"  eaf-open-pdf-from-history))


(defhydra hydra-goto (:color blue :hint nil)
  "
Goto:
^Char^              ^Word^                ^org^                    ^search^
^^^^^^^^---------------------------------------------------------------------------
_c_: 2 chars        _w_: word by char     _h_: headline in buffer  _o_: helm-occur
_C_: char           _W_: some word        _a_: heading in agenda   _p_: helm-swiper
_L_: char in line   _s_: subword by char  _q_: swoop org buffers   _f_: search forward
^  ^                _S_: some subword     ^ ^                      _b_: search backward
-----------------------------------------------------------------------------------
_B_: helm-buffers       _l_: avy-goto-line
_m_: helm-mini          _i_: ace-window
_R_: helm-recentf

_n_: Navigate           _._: mark position _/_: jump to mark
"
  ("c" avy-goto-char-2)
  ("C" avy-goto-char)
  ("L" avy-goto-char-in-line)
  ("w" avy-goto-word-1)
  ;; jump to beginning of some word
  ("W" avy-goto-word-0)
  ;; jump to subword starting with a char
  ("s" avy-goto-subword-1)
  ;; jump to some subword
  ("S" avy-goto-subword-0)

  ("l" avy-goto-line)
  ("i" ace-window)

  ("h" helm-org-headlines)
  ("a" helm-org-agenda-files-headings)
  ("q" helm-multi-swoop-org)

  ("o" helm-occur)
  ("p" swiper-helm)

  ("f" isearch-forward)
  ("b" isearch-backward)

  ("." org-mark-ring-push :color red)
  ("/" org-mark-ring-goto :color blue)
  ("B" helm-buffers-list)
  ("m" helm-mini)
  ("R" helm-recentf)
  ("n" hydra-navigate/body))

;;(global-set-key (kbd "s-g") 'goto/body)





(defhydra hydra-language (:color blue :hint nil)
  "
language:
FlySpell                       Hydras
---------------------------------------------------------------------------
_s_: ispell word                _l_: LangTool
_S_: spell next highlighted     _i_: Input language
_f_: flyspell toggle on/off     _t_: Translate
"
  ("s" ispell-word)
  ("S" flyspell-check-next-highlighted-word)
  ("f" flyspell-toggle)

  ("l" hydra-langtool/body)
  ("i" hydra-langinput/body)
  ("t" hydra-langtranslate/body)
  )

(defhydra hydra-langtool (:color blue :hint nil)
  "
langtool:
-----------------------------------------------------------------------------------
_c_: check
_C_: Check Buffer
_f_: fix/Correct Buffer
_m_: Message at point
_l_: Language change
_d_: done? check
"
  ("C" langtool-check-buffer)
  ("c" langtool-check)
  ("f" langtool-correct-buffer)
  ("m" langtool-show-message-at-point)
  ("l" langtool-switch-default-language)
  ("d" langtool-check-done)

  )

(defhydra hydra-langinput (:color blue :hint nil)
  "
language input method and dictionary:
-----------------------------------------------------------------------------------
_i_: Input method & dict        _f_: french input & dict.
_d_: describe input method      _e_: english input & dict
_r_: Reset language environment _I_: Toggle input method

"
  ("i" set-input-and-dictionary)
  ("d" describe-input-method)
  ("r" reset-language-environment)
  ;;("I" set-input-method)  ;;  French-prefix, etc.
  ("f" french-input-dict)  ;;  French-prefix, etc.
  ("e" english-input-dict)  ;;  French-prefix, etc.
  ("I" toggle-input-method) ;; C-\ by default.

  )

(defhydra hydra-langtranslate (:color blue :hint nil)
  "
language translate:
-----------------------------------------------------------------------------------
_t_: translate sentence & point       _T_: Translate
"
  ("t" #'google-translate-smooth-translate-sentence-at-point)
  ("T" #'google-translate-smooth-translate)
  )



(defhydra hydra-describe (:color blue
                                 :hint nil)
  "
Describe Something: (q to quit)
_a_ all help for everything screen
_b_ bindings
_B_ personal bindings
_c_ char
_C_ coding system
_f_ function
_F_ flycheck checker
_i_ input method
_k_ key briefly
_K_ key
_l_ language environment
_L_ mode lineage
_m_ major mode
_M_ minor mode
_n_ current coding system briefly
_N_ current coding system full
_o_ lighter indicator
_O_ lighter symbol
_p_ package
_P_ text properties
_s_ symbol
_t_ theme
_v_ variable
_w_ where is something defined
"
  ("b" describe-bindings)
  ("B" describe-personal-keybindings)
  ("C" describe-categories)
  ("c" describe-char)
  ("C" describe-coding-system)
  ("f" describe-function)
  ("F" flycheck-describe-checker)
  ("i" describe-input-method)
  ("K" describe-key)
  ("k" describe-key-briefly)
  ("l" describe-language-environment)
  ("L" help/parent-mode-display)
  ("M" describe-minor-mode)
  ("m" describe-mode)
  ("N" describe-current-coding-system)
  ("n" describe-current-coding-system-briefly)
  ("o" describe-minor-mode-from-indicator)
  ("O" describe-minor-mode-from-symbol)
  ("p" describe-package)
  ("P" describe-text-properties)
  ("q" nil)
  ("a" help)
  ("s" describe-symbol)
  ("t" describe-theme)
  ("v" describe-variable)
  ("w" where-is))

;; (global-set-key (kbd "M-i") nil)
;; (global-set-key (kbd "M-i") #'help/hydra/left/describe/body)

(defun help/parent-mode-display ()
  "Display this buffer's mode hierarchy."
  (interactive)
  (let ((ls (parent-mode-list major-mode)))
    (princ ls)))

;; https://oremacs.com/2016/04/04/hydra-doc-syntaxhttps://oremacs.com/2016/04/04/hydra-doc-syntax//
(require 'org-agenda)

;; (define-key org-agenda-mode-map
;;   "v" 'hydra-org-agenda-view/body)

(defun org-agenda-cts ()
  (let ((args (get-text-property
               (min (1- (point-max)) (point))
               'org-last-args)))
    (nth 2 args)))

(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g? _a_: arch-trees
_w_: ?w? week       _[_: inactive      _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?    _r_: report=?r?
_m_: ?m? month      _e_: entry =?e?    _D_: diary=?D?
_y_: ?y? year       _q_: quit          _L__l__c_: ?l?"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view
   (if (eq 'day (org-agenda-cts))
       "[x]" "[ ]"))
  ("w" org-agenda-week-view
   (if (eq 'week (org-agenda-cts))
       "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view
   (if (eq 'fortnight (org-agenda-cts))
       "[x]" "[ ]"))
  ("m" org-agenda-month-view
   (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view
   (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode
   (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode
   (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode
   (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode
   (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid
   (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary
   (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("["
   (let ((org-agenda-include-inactive-timestamps t))
     (org-agenda-check-type t 'timeline 'agenda)
     (org-agenda-redo)))
  ("q" (message "Abort") :exit t))


;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

(defhydra hydra-org-clock (:color blue :hint nil)
  "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit   _g_oto entry
        _c_ontinue   _q_uit   _d_isplay
        _o_ut        ^ ^      _r_eport
      "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands")))

;; Timer^^        ^Clock^         ^Capture^
;; --------------------------------------------------
;; s_t_art        _i_ clock in    _c_apture
;; _s_top        _o_ clock out   _l_ast jump
;; _r_eset        _j_ jump
;; _p_rint
;; Clock:  _i_ In     _o_ Out    _c_ Cancel  _j_ Jump  _I_ In Current

(defhydra hydra-org-timer (:color blue :hint nil)
  "
Timer:  _s_ Start  _S_ Stop   _r_ Reset   _p_ Print
"
  ("s" org-timer-start)
  ("S" org-timer-stop)
  ;; Need to be at timer
  ("r" org-timer-set-timer)
  ;; Print timer value to buffer
  ("p" org-timer)

  ;; ("i" (org-clock-in '(4)))
  ;; ("I" (org-clock-in))
  ;; ("o" org-clock-out)
  ;; ("q" org-clock-cancel)
  ;; ;; Visit the clocked task from any buffer
  ;; ("j" org-clock-goto)

  )


(defun refile (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))

(defhydra hydra-org-refile (:foreign-keys run)
  "Refile"
  ("t" (refile "DT.org" "Dancing Tango") "Refile to Dancing Tango book.")
  ("j" (refile "journal.org" "Journal") "Journal")
  ("p" (refile "projects.org" "Projects") "Projects")
  ("l" org-refile-goto-last-stored "Jump to last refile")
  ("r" org-refile "Refile")
  ("q" nil "cancel"))


(defun org-insert-datetime-stamp ()
  "Insert a timestamp with time at current point"
  (org-insert-time-stamp (current-time) WITH-HM))

(require 'org-jekyll)

(defhydra hydra-org (:hint nil)
  "
  Org:
  Move:    _h_ Left    _t_ Up   _n_ Down   _s_ Right   _r_ refile
  -----------------------------------------------------------------
  Capture: _c_ Capture   _l_ last capture
  Blog:    _b_ On        _p_ Publish Entry   _P_ Publish blog
  hydras:  _R_ Refile    _C_ Clock           _T_ Timer
  "
  ("h" org-metaleft)
  ("t" org-metadown)
  ("n" org-metaup)
  ("s" org-metaright)
  ("r" org-refile :exit t)

  ("c" org-capture :exit t)
  ("l" org-capture-goto-last-stored :exit t)

  ("b" (blog-on) :exit t)
  ("P" (org-jekyll-export-blog-project) :exit t)
  ("p" (org-jekyll-export-current-entry-project) :exit t)

  ("R" hydra-org-refile/body :exit t)
  ("C" hydra-org-clock/body :exit t)
  ("T" hydra-org-timer/body :exit t)
  ("q" nil)
  )

;; (defadvice org-insert-heading (after add-id-stuff activate)
;;   (template-blog-post))

;; (defun template-blog-post ()
;;   (insert "\n:PROPERTIES:\n:ON: "
;;           (format-time-string "%Y-%m-%dT%H:%M:%S")
;;           "\n:BLOG:  \n:END:"))
