`(require 'emms-setup)
(require 'emms-soundcloud)

;;(require 'emms-info-mediainfo)

(emms-all)

;; libre-fm
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


;; mpd-info sucks. Missing metadata. Metaflac and native are better.

;; But so far, nothing works. The metadata in the music files is not found.
;; just metaflac and or native doesnt work. no album artist or date
;; metaflac on the cli certainly works fine.

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
;; To create a new browse-by-TYPE
(emms-browser-add-category "albumartist" 'info-albumartist)

;; Add to browser keymap
(general-define-key
 :keymaps 'emms-browser-mode-map
 "b o" 'emms-browse-by-albumartist
 "s o" 'emms-browser-search-by-albumartist
 "W o w" 'emms-browser-lookup-albumartist-on-wikipedia
 "b a" 'emms-browse-by-albumartist

 ;; fixing the default map. number choices suck.
 "b A" 'emms-browse-by-artist
 "b t" 'emms-browse-by-album
 "b g" 'emms-browse-by-genre
 "b y" 'emms-browse-by-year
 "b c" 'emms-browse-by-composer
 "b p" 'emms-browse-by-performer

 "h" 'hydra-emms/body
 "P" 'hydra-persp/body
 )

(general-define-key
 ;; NOTE: keymaps specified with :keymaps must be quoted
 :keymaps 'emms-playlist-mode-map
 "h" 'hydra-emms/body
 "P" 'hydra-persp/body
 )

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

;; Formats
(setq emms-browser-info-title-format "%i%-30t %-8g %y %a")
(setq emms-browser-playlist-info-title-format "%i%-30t %-8g %o : %a %y")

;; (setq emms-browser-playlist-info-title-format
;;       emms-browser-info-title-format)

;; covers
(setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
(setq emms-browser-thumbnail-small-size 64)
(setq emms-browser-thumbnail-medium-size 128)

;; filters
(setq emms-browser-current-filter-name nil)
(emms-browser-make-filter "all" 'ignore)
(emms-browser-make-filter "recent"
                          (lambda (track) (< 30
                                        (time-to-number-of-days
                                         (time-subtract (current-time)
                                                        (emms-info-track-file-mtime track))))))
(emms-browser-set-filter (assoc "all" emms-browser-filters))

(emms-browser-make-filter
 "all-files" (emms-browser-filter-only-type 'file))

(emms-browser-make-filter
 "last-month" (emms-browser-filter-only-recent 30))

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
  ;;(emms-player-mpd-update-all)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMS over rides to add album artist.

;; add album artist,  Over-riding emms code from here on.

(defun emms-browser-make-name (entry type)
  "Return a name for ENTRY, used for making a bdata object."
  (let ((key (car entry))
        (track (cadr entry))
        artist title) ;; only the first track
    (cond
     ((eq type 'info-title)
      (setq aartist (emms-track-get track 'info-albumartist))
      (setq artist (emms-track-get track 'info-artist))
      (setq title (emms-track-get track 'info-title))
      (if (not (and artist title))
          key
	(concat title "\t\t" aartist " : " artist)))
     (t key))))

(defun emms-browser-sort-alist (alist type)
  "Sort ALIST using the sorting function for TYPE."
  (let ((sort-func
         (cond
          ((or
            (eq type 'info-albumartist)
            (eq type 'info-artist)
            (eq type 'info-composer)
            (eq type 'info-performer)
            (eq type 'info-year)
            (eq type 'info-genre))
           'emms-browser-sort-by-name)
          ((eq type 'info-album)
           emms-browser-album-sort-function)
          ((eq type 'info-title)
           'emms-browser-sort-by-track)
          (t (message "Can't sort unknown mapping!")))))
    (funcall sort-func alist)))

(defun emms-browser-lookup-albumartist-on-wikipedia ()
  (interactive)
  (emms-browser-lookup-wikipedia 'info-albumartist))

(defun emms-browser-render-search (tracks)
  (let ((entries
         (emms-browser-make-sorted-alist 'info-albumartist tracks)))
    (dolist (entry entries)
      (emms-browser-insert-top-level-entry (car entry)
                                           (cdr entry)
                                           'info-albumartist))))
(defun emms-browser-search-by-albumartist ()
  (interactive)
  (emms-browser-search '(info-albumartist)))

(defun emms-browser-search-by-names ()
  (interactive)
  (emms-browser-search '(info-albumartist info-artist info-composer info-performer info-title info-album)))
(defun emms-browser-format-line (bdata &optional target)
  "Return a propertized string to be inserted in the buffer."
  (unless target
    (setq target 'browser))
  (let* ((name (or (emms-browser-bdata-name bdata) "misc"))
         (level (emms-browser-bdata-level bdata))
         (type (emms-browser-bdata-type bdata))
         (indent (emms-browser-make-indent level))
         (track (emms-browser-bdata-first-track bdata))
         (path (emms-track-get track 'name))
         (face (emms-browser-get-face bdata))
         (format (emms-browser-get-format bdata target))
         (props (list 'emms-browser-bdata bdata))
         (format-choices
          `(("i" . ,indent)
            ("n" . ,name)
            ("y" . ,(emms-track-get-year track))
            ("A" . ,(emms-track-get track 'info-album))
            ("o" . ,(emms-track-get track 'info-albumartist))
            ("a" . ,(emms-track-get track 'info-artist))
            ("C" . ,(emms-track-get track 'info-composer))
            ("p" . ,(emms-track-get track 'info-performer))
            ("t" . ,(emms-track-get track 'info-title))
            ("g" . ,(emms-track-get track 'info-genre))
	    ("D" . ,(emms-browser-disc-number track))
            ("T" . ,(emms-browser-track-number track))
            ("d" . ,(emms-browser-track-duration track))))
	 str)
    (when (equal type 'info-album)
      (setq format-choices (append format-choices
                                   `(("cS" . ,(emms-browser-get-cover-str path 'small))
                                     ("cM" . ,(emms-browser-get-cover-str path 'medium))
                                     ("cL" . ,(emms-browser-get-cover-str path 'large))))))


    (when (functionp format)
      (setq format (funcall format bdata format-choices)))

    (setq str
          (with-temp-buffer
            (insert format)
            (goto-char (point-min))
            (let ((start (point-min)))
              ;; jump over any image
              (when (re-search-forward "%c[SML]" nil t)
                (setq start (point)))
              ;; jump over the indent
              (when (re-search-forward "%i" nil t)
                (setq start (point)))
              (add-text-properties start (point-max)
                                   (list 'face face)))
            (buffer-string)))

    (setq str (emms-browser-format-spec str format-choices))

    ;; give tracks a 'boost' if they're not top-level
    ;; (covers take up an extra space)
    (when (and (eq type 'info-title)
               (not (string= indent "")))
      (setq str (concat " " str)))

    ;; if we're in playlist mode, add a track
    (when (and (eq target 'playlist)
               (eq type 'info-title))
      (setq props
            (append props `(emms-track ,track))))

    ;; add properties to the whole string
    (add-text-properties 0 (length str) props str)
    str))

(emms-browser-make-face "albumartist"     "#aaaabb" "#444455" 1.3)

(defun emms-browser-get-face (bdata)
  "Return a suitable face for BDATA."
  (let* ((type (emms-browser-bdata-type bdata))
         (name (cond
                ((or (eq type 'info-year)
                     (eq type 'info-genre)) "year/genre")
                ((eq type 'info-albumartist) "albumartist")
                ((eq type 'info-artist) "artist")
                ((eq type 'info-composer) "composer")
                ((eq type 'info-performer) "performer")
                ((eq type 'info-album) "album")
                ((eq type 'info-title) "track"))))
    (intern
     (concat "emms-browser-" name "-face"))))

;; (defun emms-browser-next-mapping-type (current-mapping)
;;   "Return the next sensible mapping.
;; Eg. if CURRENT-MAPPING is currently \\='info-artist, return
;;  \\='info-album."
;;   (cond
;;    ((eq current-mapping 'info-albumartist) 'info-artist)
;;    ((eq current-mapping 'info-artist) 'info-album)
;;    ((eq current-mapping 'info-composer) 'info-album)
;;    ((eq current-mapping 'info-performer) 'info-album)
;;    ((eq current-mapping 'info-album) 'info-title)
;;    ((eq current-mapping 'info-genre) 'info-artist)
;;    ((eq current-mapping 'info-year) 'info-artist)))



;; Think of it like a chain.  render tree gives the initial browse by type.
;; this goes from there and builds the appropriate tree.
;;
;; There needs to be a way to switch between tree mappings.
;; it would be nice to have album artist above artist When
;; browsing by genre.
;; different trees for different browsing types.
(defun emms-browser-next-mapping-type (current-mapping)
  "Return the next sensible mapping.
Eg. if CURRENT-MAPPING is currently \\='info-artist, return
 \\='info-album."
  (cond
   ((eq current-mapping 'info-albumartist) 'info-genre)
   ((eq current-mapping 'info-artist) 'info-title)
   ((eq current-mapping 'info-composer) 'info-album)
   ((eq current-mapping 'info-performer) 'info-album)
   ((eq current-mapping 'info-album) 'info-albumartist)
   ((eq current-mapping 'info-genre) 'info-artist)
   ((eq current-mapping 'info-year) 'info-album)))

(defun emms-info-mpd-process (track info)
  (dolist (data info)
    (let ((name (car data))
	  (value (cdr data)))
      (setq name (cond ((string= name "albumartist") 'info-albumartist)
		       ((string= name "artist") 'info-artist)
		       ((string= name "composer") 'info-composer)
		       ((string= name "performer") 'info-performer)
		       ((string= name "title") 'info-title)
		       ((string= name "album") 'info-album)
		       ((string= name "track") 'info-tracknumber)
		       ((string= name "disc") 'info-discnumber)
		       ((string= name "date") 'info-year)
		       ((string= name "genre") 'info-genre)
		       ((string= name "time")
			(setq value (string-to-number value))
			'info-playing-time)
		       (t nil)))
      (when name
	(emms-track-set track name value)))))

(defun emms-tag-editor-tag-flac (track)
  "Commit changes to an FLAC file according to TRACK."
  (require 'emms-info-metaflac)
  (with-temp-buffer
    (let ((tags '("albumartist" "artist" "composer" "performer" "title" "album" "tracknumber" "discnumber" "date" "genre" "note" "comment"))
	  val)
      (mapc (lambda (tag)
              (let ((info-tag (intern (concat "info-" tag))))
                (when (> (length (setq val (emms-track-get track info-tag))) 0)
                  (insert (upcase tag) "=" val "\n"))))
            tags)
      (when (buffer-string)
	(apply #'call-process-region (point-min) (point-max)
	       emms-info-metaflac-program-name nil
	       (get-buffer-create emms-tag-editor-log-buffer)
	       nil
	       (append
		(mapcar (lambda (tag)
			  (concat "--remove-tag=" tag))
			tags)
		'("--import-tags-from=-")
		'("--")
		(list (emms-track-name track))))))))
