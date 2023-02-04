(require 'emms-setup)

(setq emms-player-list '(emms-player-vlc)
      emms-info-functions '(emms-info-native))

(require 'emms-player-mpd)
(emms-all)

;; this package doesnt compile  17/01/23
;;(require 'emms-mark-ext)
(require 'helm-emms)
(require 'emms-info-mediainfo)
(add-to-list 'emms-info-functions 'emms-info-mediainfo)
(require 'emms-soundcloud)

;; this package doesnt compile  17/01/23
;; This plugin provides control functions (e.g. ab-loop, speed, fullscreen).
;; (require 'emms-player-simple-mpv-control-functions)

;; Usage:

;; ;; An example of setting like emms-player-mplayer.el
;; ;; `emms-player-my-mpv' is defined in this case.
;; (define-emms-simple-player-mpv my-mpv '(file url streamlist playlist)
;;   (concat "\\`\\(http[s]?\\|mms\\)://\\|"
;;           (apply #'emms-player-simple-regexp
;;                  "aac" "pls" "m3u"
;;                  emms-player-base-format-list))
;;   "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

;; (emms-player-simple-mpv-add-to-converters
;;  'emms-player-my-mpv "." '(playlist)
;;  (lambda (track-name) (format "--playlist=%s" track-name)))

;; (add-to-list 'emms-player-list 'emms-player-my-mpv)

;;The following example configuration files are available:

;; + emms-player-simple-mpv-e.g.time-display.el
;; + emms-player-simple-mpv-e.g.playlist-fname.el
;; + emms-player-simple-mpv-e.g.hydra.el


(setq emms-seek-seconds 5)
(setq emms-player-list '(emms-player-mpd))
(setq emms-info-functions '(emms-info-mpd))
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6601")

;; key bindings.
;; ("s-m p" . emms)
;; ("s-m b" . emms-smart-browse)
;; ("s-m r" . emms-player-mpd-update-all-reset-cache)
;; ("<XF86AudioPrev>" . emms-previous)
;; ("<XF86AudioNext>" . emms-next)
;; ("<XF86AudioPlay>" . emms-pause)
;; ("<XF86AudioStop>" . emms-stop)

(setq mpc-host "localhost:6601")

(defun mpd/start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (mpd/update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))
(global-set-key (kbd "s-m c") 'mpd/start-music-daemon)


(defun mpd/kill-music-daemon ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD Killed!"))
(global-set-key (kbd "s-m k") 'mpd/kill-music-daemon)

(defun mpd/update-database ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))
(global-set-key (kbd "s-m u") 'mpd/update-database)
