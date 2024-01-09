;;; Emms-Hydras --- Hyfas for Emms.
;;; Commentary:
;; A set of hydras to show the status andshow the status and  access the features of Emms.
;;
;;; Code

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

;; The oversized hydra.
(defhydra hydra-emms-monolithic (;; :color teal
                                 :hint nil)
  "
                         EMMS

 _/_ :Lock: _\\_ Queue: %s`emms-queue-lock
 Repeat _T_: Track: %`emms-repeat-track   _L_: List: %`emms-repeat-playlist
 Random _?_ shuffle: %`emms-random-playlist
---------------------------------------------------------
 Current Filter: %s(emf-current-meta-filter-name)
 Filter ring:   _←_ %s(emf-current-ring-filter-name) _→_
 Filter stack:
 %s(emf-format-stack)
 Cache stack:
 %s(emf-format-cache-stack)
                  _c C_ Clear All!!
---------------------------------------------------------
                 Information/show
    _i s_ filter status
    _i c_ cache stack        _i S_ cache stash

    _i f_ filters,           _i m_ filter menu,
    _i F_ filter-factories   _i r_ filter ring.
---------------------------------------------------------
    Windows         MPD          Cache
---------------------------------------------------------
    _p_: Playlist  _C_: Connect   _R_: Reset
    _b_: Browse    _K_: Kill      _N_: Mpd Update & Reset
    _l_: p lists   _S_: Start     _F_: Cache set From mpd
    _s_: Streams   _U_: Update    _A_: Add Dir Tree
                 _D_: update dir
---------------------------------------------------------
                     Cache stack:
          Push:                     Stash:
    _c h_ hard-filter/push    _c z_ Stash and pop
    _c p_ Push from stash     _c Z_ Stash current

    _c P_ Pop/Quit            _c r_ Rotate/Swap
    _c c_ Clear               _c R_ Swap and Pop
    _c S_ Squash
---------------------------------------------------------
                  Filter Stack:
    _f P_ pop/quit           _f r_ rotate/swap
    _f c_ clear              _f R_ rotate/swap pop
    _f S_ squash

       The current filter:  _f h_ hard filter to cache stack
    _f p_ push       _f s_ smash        _f k_ keep current
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

  ("i s" emf-status-print)
  ("i c" emf-show-cache-stack)
  ("i S" emf-show-cache-stash)
  ("i f" emf-show-filters)
  ("i m" emf-show-filter-menu)
  ("i F" emf-show-filter-factories)
  ("i r" emf-show-filter-ring)

  ("f P" emf-pop)
  ("f r" emf-swap) ; rotate ?
  ("f R" emf-swap-pop) ; rotate-eject, ,pop-previous
  ("f S" emf-squash)
  ("f k" emf-keep)
  ("f h" emf-hard-filter)
  ("f c" emf-clear)
  ("c C" emf-clear-all)
  ("c p" emf-push-cache)
  ("c z" emf-stash-pop-cache)
  ("c Z" emf-stash-cache)
  ("c P" emf-pop-cache)
  ("c h" emf-hard-filter)
  ("c r" emf-swap-cache) ; rotate ?
  ("c P" emf-pop-cache)
  ("c h" emf-hard-filter)
  ("c d" emf-push-db-cache)
  ("c r" emf-swap-cache) ; rotate ?
  ("c R" emf-swap-pop-cache) ; rotate-eject, ,pop-previous
  ("c S" emf-squash-caches)
  ("c c" emf-clear-caches)
  ("<right>" emf-next-ring-filter)
  ("<left>" emf-previous-ring-filter)
  ("f p" emf-push)
  ("f s" emf-smash)
  ("f o" emf-or)
  ("f a" emf-and)
  ("f n" emf-and-not)
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
 Ring:             _←_ %s(emf-current-ring-filter-name) _→_
                   _!_ clear ring filter.

 Current:  %s(emf-current-meta-filter-name)
 Filter stack:
 %s(emf-format-stack)
 Cache stack:
 %s(emf-format-cache-stack)
                  _c C_ Clear All!!
---------------------------------------------------------
                    Information
    _i s_ Status
    _i c_ Cache Stack        _i S_ Cache Stash

    _i f_ Filters,           _i m_ Filter menu tree
    _i F_ Filter Factories   _i r_ Filter Ring.
---------------------------------------------------------
                   Cache stack
        Push                       Stash
    _c h_ hard-filter/push      _c Z_ Stash current
    _c p_ Push from stash       _c z_ Stash and pop

    _c P_ Pop/Quit              _c r_ Rotate/Swap
    _c c_ Clear                 _c R_ Swap and Pop
    _c S_ Squash
---------------------------------------------------------
                  Filter Stack
    _P_ Pop/Quit               _r_ Rotate/Swap
    _C_ Clear                  _R_ Swap and Pop
    _S_ Squash
---------------------------------------------------------
             The current filter     _h_ hard filter
    _p_ Push New   _s_ Smash        _k_ keep current filter
    _o_ Or         _a_ And          _n_ And Not
  "
  ("q" nil)
  ("i s" emf-status-print)
  ("i c" emf-show-cache-stack)
  ("i S" emf-show-cache-stash)
  ("i f" emf-show-filters)
  ("i m" emf-show-filter-menu)
  ("i F" emf-show-filter-factories)
  ("i r" emf-show-filter-ring)

  ("P" emf-pop)
  ("r" emf-swap) ; rotate ?
  ("R" emf-swap-pop) ; rotate-eject, ,pop-previous
  ("S" emf-squash)
  ("k" emf-keep)
  ("C" emf-clear)
  ("h" emf-hard-filter)

  ("c C" emf-clear-all)
  ("c p" emf-push-cache)
  ("c z" emf-stash-pop-cache)
  ("c Z" emf-stash-cache)
  ("c P" emf-pop-cache)
  ("c h" emf-hard-filter)
  ("c r" emf-swap-cache) ; rotate ?
  ("c R" emf-swap-pop-cache) ; rotate-eject, ,pop-previous
  ("c S" emf-squash-caches)
  ("c c" emf-clear-caches)

  ("<right>" emf-next-ring-filter)
  ("<left>" emf-previous-ring-filter)

  ("!" emf-clear-ring-filter)
  ("p" emf-push)
  ("s" emf-smash)
  ("o" emf-or)
  ("a" emf-and)
  ("n" emf-and-not)
  )


(defhydra hydra-emms-filter-ring (:hint nil)
  "
                  EMMS Filter Ring
---------------------------------------------------------
                  _←_ %s(emf-current-ring-filter-name) _→_
                  _!_ clear ring filter.

  Filter stack:
  %s(emf-format-stack)
  Cache stack:
  %s(emf-format-cache-stack)
  "
  ("q" nil)
  ("<right>" emf-next-ring-filter)
  ("<left>" emf-previous-ring-filter)
  ("!" emf-clear-ring-filter)
  )

(defhydra hydra-emms-cache-stack (;; :color teal
                                  :hint nil)
  "
                    EMMS Cache stack
---------------------------------------------------------
 Ring:             _←_ %s(emf-current-ring-filter-name) _→_

 Current:  %s(emf-current-meta-filter-name)
 Filter stack:
 %s(emf-format-stack)
 Cache stack:
 %s(emf-format-cache-stack)
---------------------------------------------------------
            Push                         Stash
    _p_ Push from stash           _z_ Stash and pop
    _h_ hard-filter/push          _Z_ Stash current

    _P_ Pop/Quit                  _r_ Rotate/Swap
    _c c_ Clear                   _R_ Swap and Pop
    _S_ Squash
  "
  ("q" nil)
  ("<right>" emf-next-ring-filter)
  ("<left>" emf-previous-ring-filter)
  ("!" emf-clear-ring-filter)
  ("p" emf-push-cache)
  ("z" emf-stash-pop-cache)
  ("Z" emf-stash-cache)
  ("P" emf-pop-cache)
  ("h" emf-hard-filter)
  ("r" emf-swap-cache) ; rotate ?
  ("R" emf-swap-pop-cache) ; rotate-eject, ,pop-previous
  ("S" emf-squash-caches)
  ("c c" emf-clear-caches)
  )

(defhydra hydra-emms-search (:color teal
                                    :hint nil)
  "
                    EMMS search
---------------------------------------------------------
    _a_: Album artist           _t_: Title
    _A_: Artist                 _r_: Album/record
    _c_: Composer               _T_: Titles
    _p_: Performer              _n_: Names
    _e_: Everything, all text   _N_: Names and Titles
---------------------------------------------------------
  "
  ("q" nil)
  ("a" emf-search-by-album-artist)
  ("A" emf-search-by-artist)
  ("c" emf-search-by-composer)
  ("p" emf-search-by-performer)
  ("e" emf-search-by-all-text)
  ("t" emf-search-by-title)
  ("r" emf-search-by-album)
  ("T" emf-search-by-titles)
  ("n" emf-search-by-names)
  ("N" emf-search-by-names-and-titles.)
  )

(defhydra hydra-emms-mpd (;; :color teal
                          :hint nil)
  "
                    EMMS MPD

       MPD                    Cache
---------------------------------------------------------
    _c_: Connect             _r_: Reset
    _k_: Kill                _n_: Mpd Update & Reset
    _s_: Start               _f_: Cache set From mpd
    _u_: Update              _a_: Add Dir Tree
                           _d_: update dir
---------------------------------------------------------
  "
  ("q" nil)
  ("r" emms-cache-reset)
  ("n" emms-player-mpd-update-all-reset-cache)
  ("f" emms-cache-set-from-mpd-all)
  ("a" emms-add-directory-tree)
  ("d" my-mpd-update-dir)
  ("u" emms-player-mpd-update-all)
  ("c" emms-player-mpd-connect)
  ("s" mpd/start-music-daemon)
  ("k" mpd/kill-music-daemon)
  ;;("p" emms-player-mpd-play)
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

(defhydra hydra-emms-info (;; :color teal
                           :hint nil)
  "
                    EMMS Info

 _/_ :Lock: _\\_ Queue: %s`emms-queue-lock
 Repeat _T_: Track: %`emms-repeat-track   _L_: List: %`emms-repeat-playlist
 Random _?_ shuffle: %`emms-random-playlist
---------------------------------------------------------
 Ring:   _←_ %s(emf-current-ring-filter-name) _→_
                   _!_ clear ring filter.

 Current:  %s(emf-current-meta-filter-name)
 Filter stack:
 %s(emf-format-stack)
 Cache stack:
 %s(emf-format-cache-stack)
---------------------------------------------------------
    _s_ filter and cache status
    _c_ cache stack                _S_ cache stash

    _f_ filters,                   _m_ filter menu,
    _F_ filter-factories           _r_ filter ring
  "
  ("q" nil)
  ("L" `emms-toggle-repeat-playlist)
  ("T" `emms-toggle-repeat-track)
  ("?" `emms-toggle-random-playlist)
  ("/" emms-lock-queue)
  ("\\" emms-unlock-queue)

  ("s" emf-status-print)
  ("c" emf-show-cache-stack)
  ("S" emf-show-cache-stash)
  ("f" emf-show-filters)
  ("m" emf-show-filter-menu)
  ("F" emf-show-filter-factories)
  ("r" emf-show-filter-ring)
  ("<right>" emf-next-ring-filter)
  ("<left>" emf-previous-ring-filter)
  ("!" emf-clear-ring-filter)
  )

(defhydra hydra-emms (:color blue
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
 Cache stack:
 %s(emf-format-cache-stack)
---------------------------------------------------------
    _i_ info            _w_ windows        _m_ MPD/Cache

    _c_ cache stack     _f_ filters        _r_ ring

    _s_ search

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

  ("s" hydra-emms-search/body)
  ("m" hydra-emms-mpd/body)
  ("i" hydra-emms-info/body)
  ("p" hydra-emms-playlists/body)
  ("c" hydra-emms-cache-stack/body)
  ("w" hydra-emms-windows/body)
  ("r" hydra-emms-filter-ring/body)
  ("f" hydra-emms-filters/body)
  ("t" hydra-emms-temporal-marks/body)
  )
