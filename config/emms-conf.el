;;; emms-conf.el --- My emms configuration           -*- lexical-binding: t; -*-
;;; Commentary:
;; Copyright (C) 2023

;; Author: <ericalinagebhart@gmail.com>
;; Keywords:
;;; Code:

(require 'emms-setup)
(require 'emms-browser)
(require 'emms-filters)
;;(require 'emms-soundcloud)
;;(require 'emms-info-mediainfo)

(emms-all)

;; libre-fm                       ; dead, dead as a door nail.
;;(emms-librefm-scrobbler-enable)

;; ;; notifications
;; (require 'emms-dbus)
;; (emms-dbus-enable)

;; (emms-default-players)  ;; doesnt work. Get error symbolp
;; doing this because default players has errors
(setq emms-player-list '(emms-player-mpd
                         emms-player-mpg321
                         emms-player-vlc
                         emms-player-mpv
                         emms-player-ogg123
                         emms-player-mplayer))

;; I modified the info-mpd to have album artist. Seems to have worked.
;; I dont see a reason metaflac or native wouldnt work, but they don't.
(add-to-list 'emms-info-functions 'emms-info-metaflac)
(add-to-list 'emms-info-functions 'emms-info-mp3info)
(add-to-list 'emms-info-functions 'emms-info-ogginfo)
;; (add-to-list 'emms-info-functions 'emms-info-libtag)
;;(add-to-list 'emms-info-functions 'emms-info-mediainfo)
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-info-functions 'emms-info-native)

;; Attempt to get some reasonable tag data
;; tell metaflac to give us everything
(add-to-list 'emms-info-metaflac-options "--show-tag=ALBUMARTIST")
(add-to-list 'emms-info-metaflac-options "--show-tag=ALBUMARTISTSORT")
(add-to-list 'emms-info-metaflac-options "--show-tag=COMMENT")
(add-to-list 'emms-info-metaflac-options "--show-tag=RATING")
(add-to-list 'emms-info-metaflac-options "--show-tag=DESCRIPTION")
(add-to-list 'emms-info-metaflac-options "--show-tag=COMPOSER")
;; (add-to-list 'emms-info-metaflac-options "--show-all-tags")

;; Start the browse tree with Album Artist
(setq emms-browser-default-browse-type 'info-albumartist)

(setq emms-volume-change-function #'emms-volume-mpd-change)
(setq mms-volume-change-amount 2) ;; default is 2
(global-set-key (kbd "C-c +") 'emms-volume-mode-plus)
(global-set-key (kbd "C-c -") 'emms-volume-mode-minus)

(setq emms-seek-seconds 5)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(setq mpc-host "localhost:6600")
(setq emms-player-mpd-music-directory "/home/Music/Music")

;; the default is album artist which combines them.
;; simple, albumartist, use-directory-name
(setq emms-browser-get-track-field-function 'emms-browser-get-track-field-simple)
(setq emms-browser-search-cache t)

;; There needs to be a view which contains the tree node map, the formats,
;; and the track sort routine.
;;
;; construct the browser tree Album Artist, genre, artist, track.
(setq emms-browser-tree-node-map emms-browser-tree-node-map-AAgAt)

(setq-default
 emms-source-file-default-directory "/home/Music/Music"

 ;; native, m3u, pls, nil for prompt
 emms-source-playlist-default-format nil
 emms-playlist-mode-center-when-go t
 emms-playlist-default-major-mode 'emms-playlist-mode
 emms-show-format "NP: %s"

 emms-player-mpv-environment '("PULSE_PROP_media.role=music")
 emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display" "--force-window=no" "--vo=null"))

(require 'emms-history)
(emms-history-load) ;; persistent playlists
;; (require 'helm-emms)

;; Formats
;; Need to be able to switch formats based on browse type.
;; (setq emms-browser-info-title-format "%i%-30t %-8g %y %a")
;; (setq emms-browser-playlist-info-title-format "%i%-30t %-8g %o : %a %y")

;; Sorting
;; For tango music we want everything in order by year.
(defun emms-sort-year-p (a b)
  "Sort two tracks by year."
  (let ((year-a (string-to-number (or (emms-track-get a 'info-year) "0")))
        (year-b (string-to-number (or (emms-track-get b 'info-year) "0"))))
    (< year-a year-b)))

(setq emms-browser-track-sort-function 'emms-sort-year-p)

;; Covers
(setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
(setq emms-browser-thumbnail-small-size 64)
(setq emms-browser-thumbnail-medium-size 128)


;; Filters
(setq emf-multi-filter-save-file "~/emms-filters.el")

;;        factory      Name        arguments
(setq tango-filters
      '(("Year range" "1900-1929" 1900 1929)
        ("Year range" "1929-1937" 1929 1937)
        ("Year range" "1937-1942" 1937 1942)
        ("Year range" "1940-1946" 1940 1946)
        ("Year range" "1946-1958" 1946 1958)
        ("Year range" "1958-"     1958 3000)

        ("Directory" "tangotunes" "tangotunesflac")

        ("Genre" "Vals"    "vals")
        ("Genre" "Tango"   "tango")
        ("Genre" "Milonga" "milonga")

        ("Multi-filter"
         "1900-1937"
         (("1900-1929" "1929-1937")))

        ("Multi-filter"
         "1937-1946"
         (("1937-1942" "1940-1946")))

        ("Multi-filter"
         "Vals | milonga"
         (("Vals" "Milonga")))

        ("Multi-filter"
         "Vals 1900-1929"
         (("Vals") ("1900-1929")))

        ("Multi-filter"
         "Not vals"
         ((:not "Vals")))

        ("Multi-filter"
         "Vals or milonga 1900-1937"
         (("Vals" "Milonga")
          ("1900-1929" "1929-1937")))
        )
      )

(emf-make-filters tango-filters)

;; Also need to add these to my tango filters since I
;; didnt create them.
(setq my-filter-ring '("Tango" "Vals" "Milonga"))

;; Add my own filter selection menu with tango filters in it.
(emf-add-to-filter-menu-from-filter-list "Tango" tango-filters)
(emf-add-to-filter-menu "Tango" my-filter-ring)

;; need silence and cortinas...
(emf-make-filter-ring my-filter-ring)

;; extras - start and stop mpd. A function to give to Perspective.
(defun mpd/start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  ;;(emms-player-mpd-update-all)
  (emms-player-mpd-connect)
  ;;(emms-cache-set-from-mpd-all)
  (message "MPD Started!"))

(defun mpd/kill-music-daemon ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD Killed!"))

;; Keymaps
;; Add to browser keymap
(general-define-key
 :keymaps 'emms-browser-mode-map
 ;; fixing the default map. number choices suck.
 "b a" 'emms-browse-by-albumartist
 "b A" 'emms-browse-by-artist
 "b t" 'emms-browse-by-album
 "b g" 'emms-browse-by-genre
 "b y" 'emms-browse-by-year
 "b c" 'emms-browse-by-composer
 "b p" 'emms-browse-by-performer
 "s g" 'emms-browser-search-by-albumartist-artist-genre

 "t" 'hydra-persps/body
 "h" 'hydra-emms/body
 "H" 'hydra-emms-monolithic/body
 "m" 'emms-metaplaylist-mode-go
 "P" 'hydra-persp/body)

(general-define-key
 ;; NOTE: keymaps specified with :keymaps must be quoted
 :keymaps 'emms-playlist-mode-map
 "A" 'emms-add-playlist
 "h" 'hydra-emms/body
 "H" 'hydra-emms-monolithic/body
 "m" 'emms-metaplaylist-mode-go
 "t" 'hydra-emms-persps/body
 "P" 'hydra-persp/body)

(general-define-key
 :keymaps 'emms-metaplaylist-mode-map
 "SPC" 'emms-metaplaylist-mode-goto
 "c" 'emms-metaplaylist-mode-goto-current
 "h" 'hydra-emms/body
 "H" 'hydra-emms-monolithic/body
 "t" 'hydra-emms-persps/body
 "P" 'hydra-persp/body)

;; a handy function
(defun emms-browser-cache-to-tracks (cache)
  "Return a list of tracks from the CACHE given."
  (let (tracks)
    (maphash (lambda (_k track)
               (push track tracks))
             cache)
    tracks))

;;; Provides emms-conf.el
;;; emms-conf.el ends here
