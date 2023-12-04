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
          (emf-print-stack)
          (mapconcat #'identity (emms-browser-search-crumbs) "\n")))

;; (("↑" . "<up>")
;;  ("↓" . "<down>")
;;  ("→" . "<right>")
;;  ("←" . "<left>")
;;  ("⌫" . "DEL")
;;  ("⌦" . "<deletechar>")
;;  ("⏎" . "RET"))

(defun my-emms-status ()
  "Message the status of emms."
  (interactive)
  (message (emms-status)))

(defun crumb-text ()
  "Format the search stack crumbs."
  (interactive)
  (format "\t%s" (mapconcat #'identity (emms-browser-search-crumbs) "\n\t")))

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
 %s(emf-print-stack)
 Search stack:
 %s(crumb-text)
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
    _f S_ Status     _f k_ keep current for session

    Stack:
    _f q_ pop/quit   _f r_ rotate/swap
    _f c_ clear      _f R_ rotate/swap pop
    _f f_ squash/flatten

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
  ("U" emms-player-mrd-update-all)
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
  ("f S" emf-status)
  ("f q" emf-pop)
  ("f r" emf-swap) ; rotate ?
  ("f R" emf-swap-pop) ; rotate-eject, ,pop-previous
  ("f f" emf-squash)
  ("f k" emf-keep)
  ("f c" emf-clear)
  ("<right>" emf-next-ring-filter)
  ("<left>" emf-previous-ring-filter)
  ("f p" emf-select-push)
  ("f s" emf-select-smash)
  ("f o" emf-select-or)
  ("f a" emf-select-and)
  ("f n" emf-select-and-not)
  )
