(require 'hydra)

(defun mympd-update-dir()
  (emms-player-mpd-update-directory "/home/Music/Music"))

(defun emms-status ()
  "Format information on the state of Emms."
  (format "Queue locked: %s\nRandom: %s  Repeat: %s\nFilter: %s\n%s\n%s\n"
          emms-queue-lock
          emms-random-playlist
          emms-repeat-track
          (emf-current-meta-filter)
          (emf-format-stack)
          (mapconcat #'identity (emms-browser-search-crumbs) "\n")))

;; (("↑" . "<up>")
;;  ("↓" . "<down>")
;;  ("→" . "<right>")
;;  ("←" . "<left>")
;;  ("⌫" . "DEL")
;;  ("⌦" . "<deletechar>")
;;  ("⏎" . "RET"))

(defhydra hydra-emms (;; :color teal
                      :hint nil)
  "
                         EMMS

 _/_ :Lock: _\\_ Queue: %s`emms-queue-lock
 Repeat _T_: Track: %`emms-repeat-track   _L_: List: %`emms-repeat-playlist
 Random _?_ shuffle: %`emms-random-playlist
---------------------------------------------------------
 Filter ring:   _←_ %s(emf-current-ring-filter-name) _→_
 Filter stack:
 %s(emf-format-stack)
 Search stack:
 %s(emf-format-search-stack)
---------------------------------------------------------
    Windows         MPD          Cache
---------------------------------------------------------
    _p_: Playlist  _C_: Connect   _R_: Reset
    _b_: Browse    _K_: Kill      _N_: Mpd Update & Reset
    _l_: p lists   _S_: Start     _F_: Cache set From mpd
    _s_: Streams   _U_: Update    _A_: Add Dir Tree
                 _D_: update dir
---------------------------------------------------------
    Filters: %s(emf-current-meta-filter-name)
    Ring:   _←_ %s(emf-current-ring-filter-name) _→_
    _f d_ dump     _f k_ keep current for session

    Stack:
    _f P_ pop/quit   _f r_ rotate/swap
    _f c_ clear      _f R_ rotate/swap pop
    _f S_ squash     _f h_ hard cache to search stack

   Selection:
    _f p_ push       _f s_ smash
    _f o_ or         _f a_ and          _f n_ and Not
---------------------------------------------------------
    Temporal marks:
    _n_: Now Add   _<_: Previous  _>_: Next  _c_: Clear

    Volume:           Volume mode:
    _u_: up  _d_: down    _+_: plus  _-_: minus

    Playlist mark mode:
    _m_ enable       _M_ disable
---------------------------------------------------------
    Playlists:
    _P_: Play start  _r_: Resume play  _s_: Stop
    _W_: Save        _N_: New          _a_: Add/Load

  "
  ("q" nil)
  ("P" emms-start)
  ("b" emms-browser)
  ("p" emms-playlist-mode-go)
  ("l" emms-metaplaylist-mode-go)
  ("s" emms-streams)
  ("R" emms-cache-reset)
  ("N" emms-player-mpd-update-all-reset-cache)
  ("F" emms-cache-set-from-mpd-all)
  ("A" emms-add-directory-tree)
  ("a" emms-add-playlist)
  ("D" my-mpd-update-dir)
  ("U" emms-player-mpd-update-all)
  ("m" emms-mark-mode)
  ("M" emms-mark-mode-disable)
  ("r" emms-player-mpd-play)
  ("N" emms-playlist-new)
  ("W" emms-playlist-save)
  ("L" `emms-toggle-repeat-playlist)
  ("T" `emms-toggle-repeat-track)
  ("?" `emms-toggle-random-playlist)
  ("/" emms-lock-queue)
  ("\\" emms-unlock-queue)

  ("C" emms-player-mpd-connect)
  ("S" mpd/start-music-daemon)
  ("K" mpd/kill-music-daemon)
  ;;("p" emms-player-mpd-play)
  ("s" emms-stop)
  ("n" emms-bookmarks-add)
  ("<" emms-bookmarks-previous)
  (">" emms-bookmarks-next)
  ("c" emms-bookmarks-clear)
  ("d" emms-volume-lower)
  ("u" emms-volume-raise)
  ("+" emms-volume-mode-plus)
  ("-" emms-volume-mode-minus)
  ("f d" emf-status)
  ("f P" emf-pop)
  ("f r" emf-swap) ; rotate ?
  ("f R" emf-swap-pop) ; rotate-eject, ,pop-previous
  ("f S" emf-squash)
  ("f k" emf-keep)
  ("f h" emf-hard-cache)
  ("f c" emf-clear)
  ("<right>" emf-next-ring-filter)
  ("<left>" emf-previous-ring-filter)
  ("f p" emf-select-push)
  ("f s" emf-select-smash)
  ("f o" emf-select-or)
  ("f a" emf-select-and)
  ("f n" emf-select-and-not)
  )

(defhydra hydra-emms-playlists (;; :color teal
                                :hint nil)
  "
                 EMMS Playlists/playing

 _/_ :Lock: _\\_ Queue: %s`emms-queue-lock
 Repeat _T_: Track: %`emms-repeat-track   _L_: List: %`emms-repeat-playlist
 Random _?_ shuffle: %`emms-random-playlist
---------------------------------------------------------
    Playlists:
    _P_: Play start  _r_: Resume play  _s_: Stop
    _w_: Save        _n_: New          _a_: Add/Load

    Mark mode:
    _m_ enable       _M_ disable
---------------------------------------------------------
    Temporal marks:
    _n_: Now Add   _<_: Previous  _>_: Next  _c_: Clear

    Volume:           Volume mode:
    _u_: up  _d_: down    _+_: plus  _-_: minus
---------------------------------------------------------

  "
  ("q" nil)
  ("P" emms-start)
  ("m" emms-mark-mode)
  ("M" emms-mark-mode-disable)
  ("r" emms-player-mpd-play)
  ("n" emms-playlist-new)
  ("w" emms-playlist-save)
  ("L" `emms-toggle-repeat-playlist)
  ("T" `emms-toggle-repeat-track)
  ("?" `emms-toggle-random-playlist)
  ("/" emms-lock-queue)
  ("\\" emms-unlock-queue)

  ("s" emms-stop)
  ("n" emms-bookmarks-add)
  ("<" emms-bookmarks-previous)
  (">" emms-bookmarks-next)
  ("c" emms-bookmarks-clear)
  ("d" emms-volume-lower)
  ("u" emms-volume-raise)
  ("+" emms-volume-mode-plus)
  ("-" emms-volume-mode-minus)
  )

(defhydra hydra-emms-filters (;; :color teal
                              :hint nil)
  "
                    EMMS Filters
---------------------------------------------------------
 Ring:   _←_ %s(emf-current-ring-filter-name) _→_

 Current:  %s(emf-current-meta-filter-name)
 Stack:
 %s(emf-format-stack)
 Search:
 %s(emf-format-search-stack)
---------------------------------------------------------
    _d_ dump     _k_ keep current for session

    Stack:
    _P_ pop/quit           _r_ rotate/swap
    _c_ clear              _R_ swap pop
    _S_ squash

   Selection:
    _p_ push       _s_ smash
    _o_ or         _a_ and          _n_ and Not
  "
  ("q" nil)
  ("d" emf-status)
  ("P" emf-pop)
  ("r" emf-swap) ; rotate ?
  ("R" emf-swap-pop) ; rotate-eject, ,pop-previous
  ("S" emf-squash)
  ("k" emf-keep)
  ("c" emf-clear)
  ("<right>" emf-next-ring-filter)
  ("<left>" emf-previous-ring-filter)
  ("p" emf-select-push)
  ("s" emf-select-smash)
  ("o" emf-select-or)
  ("a" emf-select-and)
  ("n" emf-select-and-not)
  )

(defhydra hydra-emms-filter-ring (:hint nil)
  "
                  EMMS Filter Ring
---------------------------------------------------------
          _←_ %s(emf-current-ring-filter-name) _→_

  Filter stack:
  %s(emf-format-stack)
  Search stack:
  %s(emf-format-search-stack)
  "
  ("q" nil)
  ("<right>" emf-next-ring-filter)
  ("<left>" emf-previous-ring-filter)
  )

(defhydra hydra-emms-cache (;; :color teal
                            :hint nil)
  "
                         EMMS Cache

   MPD               Cache
   ---------------------------------------------------------
   _C_: Connect     _R_: Reset
   _K_: Kill        _N_: Mpd Update & Reset
   _S_: Start       _F_: Cache set From mpd
   _U_: Update      _A_: Add Dir Tree
               _D_: update dir
  "
  ("q" nil)
  ("R" emms-cache-reset)
  ("N" emms-player-mpd-update-all-reset-cache)
  ("F" emms-cache-set-from-mpd-all)
  ("A" emms-add-directory-tree)
  ("D" my-mpd-update-dir)
  ("U" emms-player-mpd-update-all)
  ("C" emms-player-mpd-connect)
  ("S" mpd/start-music-daemon)
  ("K" mpd/kill-music-daemon)
  )

(defhydra hydra-emms-windows (:color blue
                                     :hint nil)
  "
           EMMS Windows
------------------------------------------
    _b_: Browse              _p_: Playlist
    _m_: Meta play lists     _s_: Streams
  "
  ("q" nil)
  ("b" emms-browser)
  ("p" emms-playlist-mode-go)
  ("m" emms-metaplaylist-mode-go)
  ("s" emms-streams)
  )

(defhydra hydra-emms-temporal-marks (:color blue
                                            :hint nil)
  "
           EMMS Temporal marks:
    --------------------------------------------------
    _n_: Now Add   _←_ previous/next _→_   _c_: Clear
   "
  ("q" nil)
  ("n" emms-bookmarks-add)
  ("<" emms-bookmarks-previous)
  (">" emms-bookmarks-next)
  ("c" emms-bookmarks-clear)
  )

(defhydra hydra-emms-compact (:color blue
                                     :hint nil)
  "
                         EMMS

 _/_ :Lock: _\\_ Queue: %s`emms-queue-lock
 Repeat _T_: Track: %`emms-repeat-track   _L_: List: %`emms-repeat-playlist
 Random _?_ shuffle: %`emms-random-playlist
 Volume: _↑__↓_
---------------------------------------------------------
 Filter ring:   _←_ %s(emf-current-ring-filter-name) _→_
 Filter stack:
 %s(emf-format-stack)
 Search stack:
 %s(emf-format-search-stack)
---------------------------------------------------------
    _w_ windows              _c_ cache/mpd
---------------------------------------------------------
    _f_ filters              _r_ ring
    _p_ playlists/playing    _t_ temporal marks
  "
  ("q" nil)
  ("L" `emms-toggle-repeat-playlist)
  ("T" `emms-toggle-repeat-track)
  ("?" `emms-toggle-random-playlist)
  ("/" emms-lock-queue)
  ("\\" emms-unlock-queue)
  ("<down>" emms-volume-lower)
  ("<up>" emms-volume-raise)
  ("<right>" emf-next-ring-filter)
  ("<left>" emf-previous-ring-filter)

  ("p" hydra-emms-playlists/body)
  ("c" hydra-emms-cache/body)
  ("w" hydra-emms-windows/body)
  ("r" hydra-emms-filter-ring/body)
  ("f" hydra-emms-filters/body)
  ("t" hydra-emms-temporal-marks/body)
  )
