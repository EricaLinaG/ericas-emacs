(require 'emms-setup)
(require 'emms-soundcloud)
(require 'emms-info-mediainfo)

(setq emms-player-list '(emms-player-vlc)
      emms-info-functions '(emms-info-native))

(require 'emms-player-simple)
(require 'emms-player-mpd)
(emms-all)

;; libre-fm
;;(emms-librefm-scrobbler-enable)

;; ;; notifications
;; (require 'emms-dbus)
;; (emms-dbus-enable)

;;(emms-default-players)
;; (require 'emms-player-mplayer)
;; (require 'emms-player-vlc)
;; (require 'emms-player-mpv)

(require 'emms-source-file)
(require 'emms-source-playlist)

;; (add-to-list 'emms-player-list '(emms-player-mpg321
;;                                  emms-player-ogg123
;;                                  ;; emms-player-mplayer
;;                                  ;; emms-player-vlc
;;                                  ;; emms-player-mpv
;;                                  emms-player-mpd))

(setq emms-player-list '(emms-player-mpd)
      emms-info-functions '(emms-info-mpd))

;;(add-to-list 'emms-info-functions 'emms-info-native)
;;(add-to-list 'emms-info-functions 'emms-info-mediainfo)
;;(add-to-list 'emms-info-functions 'emms-info-mpd)

;;(setq emms-volume-change-function #'emms-volume-mpd-change)

(setq emms-seek-seconds 5)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(setq mpc-host "localhost:6600")
(setq emms-player-mpd-music-directory "/home/Music/Music")

;; covers
(setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
(setq emms-browser-thumbnail-small-size 64)
(setq emms-browser-thumbnail-medium-size 128)
;; filters
(emms-browser-make-filter "all" #'ignore)
(emms-browser-make-filter "recent"
                          (lambda (track) (< 30
                                        (time-to-number-of-days
                                         (time-subtract (current-time)
                                                        (emms-info-track-file-mtime track))))))
(emms-browser-set-filter (assoc "all" emms-browser-filters))

(setq emms-browser-info-title-format "%i%n - %a - %A %y")
(setq emms-browser-playlist-info-title-format
      emms-browser-info-title-format)

(setq-default
 emms-source-file-default-directory "/home/Music/Music"

 emms-source-playlist-default-format 'm3u
 emms-playlist-mode-center-when-go t
 emms-playlist-default-major-mode 'emms-playlist-mode
 emms-show-format "NP: %s"

 emms-player-mpv-environment '("PULSE_PROP_media.role=music")
 emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display" "--force-window=no" "--vo=null"))

(require 'emms-history)
(emms-history-load) ;; persistent playlists
(require 'helm-emms)

;; key bindings.
;; ("s-m p" . emms)
;; ("s-m b" . emms-smart-browse)
;; ("s-m r" . emms-player-mpd-update-all-reset-cache)
;; ("<XF86AudioPrev>" . emms-previous)
;; ("<XF86AudioNext>" . emms-next)
;; ("<XF86AudioPlay>" . emms-pause)
;; ("<XF86AudioStop>" . emms-stop)

(defun mpd/start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (mpd/update-database)
  (emms-player-mpd-connect)
  ;;(emms-cache-set-from-mpd-all)
  (message "MPD Started!"))
(global-set-key (kbd "s-m c") 'mpd/start-music-daemon)


(defun mpd/kill-music-daemon ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process ""killall nil nil nil "mpd")
  (message "MPD Killed!"))
(global-set-key (kbd "s-m k") 'mpd/kill-music-daemon)

(defun mpd/update-database ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))
(global-set-key (kbd "s-m u") 'mpd/update-database)
