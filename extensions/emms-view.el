;;; Emms-view --- formatting of track meta data
;;; Commentary:
;;  Built from pieces and parts of the Emms-browser.
;;  Group and manage the information required for viewing tracks.
;;  Control the building of the tree, the formats of the fields and
;;  the browse-by settings for playlists, browser, and searches.
;;; Code:

(require 'emms-browser)

;; Defaults
(defvar emv-default-browse-type 'info-albumartist
  "The default browsing mode. The root field in the tree view.")

(defvar emv-default-expand '0
  "Default expansion level of the tree.")

(defvar emv-default-format "%i%n"
  "Indent + name.")

(defvar emv-default-view-set nil
  "The Default view set if nothing else.")

;; Current settings.
(defvar emv-current-target (emv-target-keyword 'browser)
  "The current target playlist, browser or filtered.")

(defvar emv-current-tree-map nil
  "The current tree node map.
The alist mapping of the browser tree node map.")

(defvar emv-current-indent nil
  "Used to override the indentention level.")

(defvar emv-current-view-set nil
  "The current View Set.")

(defvar emv-current-view nil
  "The current View.")

(defvar emv-current-field-formats nil
  "The current field formats set.")

(defvar emv-current-browse-type nil
  "The current track sort.")

(defvar emv-current-expand nil
  "The current automatic tree expansion level.")

;; The rings.
(defvar emv-browse-type-ring nil
  "The ring of browse type info-fields to start the browse tree render from.")

(defvar emv-view-set-ring nil
  "The ring of view sets to change the view.")

(defvar emv-view-ring nil
  "The ring of views to change the view.")

(defvar emv-formats-ring nil
  "The ring of field format sets to apply to the view.")

(defvar emv-tree-map-ring nil
  "The ring of tree maps to apply to the view.")

(defvar emv-sort-ring nil
  "The ring of sorts to apply to the view.")

;; The things that we have lists of.
(defvar emv-browse-types
  '(info-albumartist
    info-artist
    info-composer
    info-performer
    info-album
    info-year
    info-genre)
  "The list of browse-by types.")

(defvar emv-tree-maps nil
  "A list of tree node maps by name.")

(defvar emv-formats nil
  "The alist of known formats.
A format can be a string expression or
a function which should take bdata and formats as parameters, and resolve
to valid string expression.")

(defvar emv-field-formats nil
  "The field type names mapped to formats.
Only fields for which the default field format is not desirable.")

(defvar emv-views nil
  "The list of view definitions available.")

(defvar emv-view-sets nil
  "The list of view set definitions available.
A view set defines one or more views for the view targets of browser,
playlist, filtered and default.")

;; field formats.
;; these are functions which take bdata and format or they are format strings.
(setq emv-formats '(("default" . "%i%n")
                    ("empty" . "")
                    ("artist and title" . emv-track-artist-and-title-format)
                    ("year and album". emv-track-year-and-album-format)
                    ("year and album medium cover". emv-track-year-and-album-format-med)
                    ("title genre orchestra album year" . "%i%-30t %-8g %o : %a %y")
                    ("title genre year album" . "%i%-30t %-8g %y %a")))

;; This is where we connect info-field to a format. Define the fields you wish
;; to change.  Almost always just title or album, otherwise the default,
;; '%i%n' is good.
(setq emv-field-formats '(("default" '())
                          ("artist and title"
                           ((info-title  . "artist and title")))
                          ("year-and-album"
                           ((info-album  . "year and album")))
                          ("year-and-album-med-cover"
                           ((info-album  . "year and album medium cover")))
                          ("title-genre-orch"
                           ((info-title  . "title genre orchestra album year")))
                          ("title-genre-year"
                           ((info-title  . "title genre year")))))

(setq emv-tree-maps
      '(("default"
         '((info-albumartist . info-artist)
           (info-artist      . info-album)
           (info-composer    . info-album)
           (info-performer   . info-album)
           (info-album       . info-title)
           (info-genre       . info-artist)
           (info-year        . info-artist)))

        ("AlbumArtist Genre Artist"
         ((info-albumartist . info-genre)
          (info-artist      . info-title)
          (info-composer    . info-album)
          (info-performer   . info-album)
          (info-album       . info-albumartist)
          (info-genre       . info-artist)
          (info-year        . info-album)))

        ("AlbumArtist Artist Genre"
         ((info-albumartist . info-artist)
          (info-artist      . info-genre)
          (info-composer    . info-album)
          (info-performer   . info-album)
          (info-album       . info-albumartist)
          (info-genre       . info-title)
          (info-year        . info-album)))))

(setq emv-views
      '(("default"
         ((:field-formats . "default")
          (:tree-map . "default")
          (:expand . 0)
          (:browse-by . info-albumartist)
          (:track-sort . emms-sort-year-p)))

        ("artist-and-title"
         ((:field-formats . "artist and title")
          (:tree-map . "default")
          (:expand . 0)
          (:browse-by . info-albumartist)))

        ("title-genre-orch"
         ((:field-formats . "title Genre Orch")
          (:tree-map . "AlbumArtist Artist Genre")
          (:expand . 0)
          (:browse-by . info-albumartist)))))

(setq emv-view-sets
      '(("default"
         ((:default . "default")
          (:browser . "default")
          (:playlist . "default")
          (:filtered . "default")))

        ("title-genre-orch"
         ((:default . "default")
          (:browser . "artist-and-title")
          (:playlist . "title-genre-orch")
          (:filtered . "artist-and-title")))))

(setq emv-default-view-set (assoc "default" emv-view-sets))

;; --------------------------------------------------
;; The User interface, rings and the mechanics.
;; --------------------------------------------------
(defun emv-new-view-from-current (some-name)
  "Create a view definition with SOME-NAME based on the current values of the view."
  (some-name (list (cons :field-formats (car emv-current-field-formats))
                   (cons :tree-node-map (car emv-current-tree-map))
                   (cons :expand emv-current-expand)
                   (cons :browse-by emv-current-browse-type))))

(defun emv-make-ring (list-of-things)
  "Make a ring of whatever names there are from the alist LIST-OF-THINGS."
  (let ((ring
         (make-ring
          (length list-of-things))))
    (mapcar (lambda (thing)
              (ring-insert ring thing))
            list-of-things)
    ring))

(setq emv-view-set-ring (emv-make-ring (mapcar 'car emv-view-sets)))
(setq emv-view-ring (emv-make-ring (mapcar 'car emv-views)))
(setq emv-formats-ring (emv-make-ring (mapcar 'car emv-formats)))
(setq emv-tree-map-ring (emv-make-ring (mapcar 'car emv-tree-maps)))
(setq emv-browse-type-ring (emv-make-ring emv-browse-types))
;; (setq emv-sort-ring (emv-make-ring (emv-sorts)))

(defun emv-name-current-ring-thing (some-ring current-thing)
  "Give the name of the CURRENT-THING in SOME-RING.
CURRENT-THING can be nil, a string, or a cons with a name in the car.
If CURRENT-THING is nil the first entry in SOME-RING is given."
  (if current-thing
      (if (consp current-thing)
          (car current-thing)
        current-thing)
    (ring-ref some-ring 0)))

(defun emv-next-ring-thing(some-ring  current-thing)
  "Move to the previous thing in SOME-RING based on CURRENT-THING.
CURRENT-THING can be a string name or a cons with a string name in
the car."
  (ring-next some-ring
             (emv-name-current-ring-thing
              some-ring current-thing)))

(defun emv-previous-ring-thing(some-ring current-thing)
  "Move to the previous thing in SOME-RING based on CURRENT-THING.
CURRENT-THING can be a string name or a cons with a string name in
the car."
  (ring-previous some-ring
                 (emv-name-current-ring-thing
                  some-ring current-thing)))

(defun emv-next-view-set ()
  "Switch to the next view set in the view set ring."
  (interactive)
  (emv-set-view-set  (emv-next-ring-thing
                      emv-view-set-ring
                      emv-current-view-set)))

(defun emv-previous-view-set ()
  "Switch to the previous view set in the view set ring."
  (interactive)
  (emv-set-view-set  (emv-previous-ring-thing
                      emv-view-set-ring
                      emv-current-view-set)))

(defun emv-next-view ()
  "Switch to the next view set in the view ring."
  (interactive)
  (emv-set-view  (emv-next-ring-thing
                  emv-view-ring
                  emv-current-view)))

(defun emv-previous-view ()
  "Switch to the previous view set in the view set ring."
  (interactive)
  (emv-set-view  (emv-previous-ring-thing
                  emv-view-ring
                  emv-current-view)))


(defun emv-next-format-set ()
  "Switch to the next format set in the formats ring."
  (interactive)
  (emv-set-field-formats  (emv-next-ring-thing
                           emv-formats-ring
                           emv-current-field-formats)))

(defun emv-previous-format-set ()
  "Switch to the previous format set in the formats ring."
  (interactive)
  (emv-set-field-formats  (emv-previous-ring-thing
                           emv-formats-ring
                           emv-current-field-formats)))

(defun emv-next-tree-map ()
  "Switch to the next tree node map."
  (interactive)
  (emv-set-tree-map  (emv-next-ring-thing
                      emv-tree-map-ring
                      emv-current-tree-map)))

(defun emv-previous-tree-map ()
  "Switch to the previous tree node map."
  (interactive)
  (emv-set-tree-map  (emv-previous-ring-thing
                      emv-tree-map-ring
                      emv-current-tree-map)))

(defun emv-next-browse-type ()
  "Switch to the next browse type in the browse-type ring."
  (interactive)
  (emv-set-browse-type  (emv-next-ring-thing
                         emv-browse-type-ring
                         emv-current-browse-type)))

(defun emv-previous-browse-type ()
  "Switch to the previous browse type in the browse-type ring."
  (interactive)
  (emv-set-browse-type  (emv-next-ring-thing
                         emv-browse-type-ring
                         emv-current-browse-type)))

;; Setting everything.
(defun emv-show ()
  "print out the status of things."
  (interactive)
  (message "View Set: %s\n Target: %s\n View: %s\n Formats: %s\n Sort: @@@\n Browse-type: %s\n Expand: %s\n Tree map: %s\n"
           emv-current-view-set
           emv-current-target
           emv-current-view
           emv-current-field-formats
           ;; emv-current-track-sort
           emv-current-browse-type
           emv-current-expand
           emv-current-tree-map))

(defun emv-ensure-name (name prompt list-of-things)
  "Ensure NAME has a value will PROMPT with LIST-OF-THINGS name is nil.
Return the name or prompt with a list if not."
  (if name name
    (completing-read prompt list-of-things nil t)))

(defun emv-target-keyword (target)
  "Translate a TARGET to a keyword."
  (when target
    (cond
     ((keywordp target) target)
     ((string= target 'browser) :browser)
     ((string= target 'playlist) :playlist)
     ((string= target 'filtered) :filtered)
     ((t :default)))))

(defun emv-get-target-view-from-view-set (target)
  "Return the TARGET's view from the current view-set.
If TARGET is not found look for the default target in the view set.
Finally look for the target in the default view set, and then the
default view in the default view-set as a last resort."
  (let ((current-view-set (cadr emv-current-view-set))
        (default-view-set (cadr emv-default-view-set))
        (target (emv-target-keyword target)))
    (assoc
     (cdr (or (assoc target  current-view-set)
              (assoc :default  current-view-set)
              (assoc target  default-view-set)
              (assoc :default default-view-set)))
     emv-views)))

(defun emv-set-current-from-view (&optional target)
  "Set all the current settings for TARGET from the current view.
If TARGET is nil, use the 'emv-current-view. Essentially a reset.
TARGET can be browser playlist filtered or default. Emv will
look for default if the target view is missing from the view set.
The emv-default-view will be used if no other target view was found."
  (let ((target-view (if target
                         (cdr (emv-get-target-view-from-view-set target))
                       (cadr emv-current-view) )))
    (message "target view %s " target-view)
    (emv-set-tree-map (cdr (assoc :tree-map target-view)))
    (emv-set-field-formats (cdr (assoc :field-formats target-view)))
    (setq emv-current-browse-type
          (or (cdr (assoc :browse-type target-view))
              emv-default-browse-type))
    (setq emv-current-expand
          (or (cdr (assoc :expand target-view))
              emv-default-expand))))

(defun emv-set-view-set (view-set-name &optional target)
  "Set the view set by VIEW-SET-NAME.
Find the view for TARGET if given otherwise use the last target used.
Set the view attributes to the current view."
  (setq emv-current-view-set
        (assoc  (emv-ensure-name view-set-name
                                 "View Set: "
                                 (mapcar 'car emv-view-sets))
                emv-view-sets))

  (let* ((target (or (emv-target-keyword target)
                     emv-current-target
                     :browser))
         (view (emv-get-target-view-from-view-set target)))

    (message "view %s " view)
    (setq emv-current-target target)
    (setq emv-current-view view )
    (emv-set-current-from-view)
    ;; Set all the 'current' values from the view set set above.
    ))

(defun emv-set-view (view-name)
  "Set the view to VIEW-NAME.
Set the view attributes to the current view."
  (setq emv-current-view
        (assoc  (emv-ensure-name view-name
                                 "View: "
                                 (mapcar 'car emv-views))
                emv-views))
  (emv-set-current-from-view))

(defun emv-set-tree-map (&optional map-name)
  "Set the tree map named MAP-NAME to the current view.
Prompt with a select list if no map name is given."
  (setq emv-current-tree-map
        (assoc (emv-ensure-name map-name
                                "Tree map: "
                                (mapcar 'car emv-tree-maps))
               emv-tree-maps)))

(defun emv-set-field-formats (&optional field-formats-name)
  "Set the field formats in the current view by FIELD-FORMATS-NAME.
and set it to the current view."
  (setq emv-current-field-formats
        (assoc (emv-ensure-name field-formats-name
                                "Formats: "
                                (mapcar 'car emv-field-formats))
               emv-field-formats)))

(defun emv-set-browse-type (&optional browse-type)
  "Set the current BROWSE-TYPE."
  (setq emv-current-browse-type
        (emv-ensure-name browse-type
                         "Browse by: "
                         emv-browse-types)))

(defun emv-next-mapping-type (current-mapping)
  "Return the next mapping according to CURRENT-MAPPING in the current tree map."
  (alist-get current-mapping emv-current-tree-map))

(emv-set-view-set "default" 'browser)

;; --------------------------------------------------
;; Album covers
;; --------------------------------------------------
;; Utilities to get and make covers.
;; Using Emms browser settings...

;; (defvar emms-browser--covers-filename nil "Foo.")
;; (defvar emms-browser-covers-file-extensions nil "Foo.")
;; (defvar emms-browser-covers nil "Foo.")
;; (defvar emms-browser-default-covers nil "Foo.")

(defun emv--build-cover-filename ()
  "Build `emms-browser--covers-filename'.

Based on from `emms-browser-covers' (when a list) and
`emms-browser-covers-file-extensions'."
  (setq emms-browser--covers-filename
        (mapcar (lambda (cover)
                  (if (file-name-extension cover)
                      (list cover)
                    (mapcar (lambda (ext) (concat cover "." ext))
                            emms-browser-covers-file-extensions)))
                emms-browser-covers)))

(defun emv-get-cover-from-path (path &optional size)
  "Return a cover filename at PATH with SIZE, if it exists."
  (unless size
    (setq size 'medium))
  (let* ((size-idx (cond
                    ((eq size 'small) 0)
                    ((eq size 'medium) 1)
                    ((eq size 'large) 2)))
         (cover
          (cond
           ((functionp emms-browser-covers)
            (funcall emms-browser-covers (file-name-directory path) size))
           ((and (listp emms-browser-covers)
                 (nth size-idx emms-browser-covers))
            (unless emms-browser--covers-filename
              (emv--build-cover-filename))
            (car (delq nil
                       (mapcar (lambda (cover)
                                 (let ((coverpath
                                        (concat (file-name-directory path) cover)))
                                   (and (file-exists-p coverpath) coverpath)))
                               (nth size-idx emms-browser--covers-filename))))))))
    (if (and cover (file-readable-p cover))
        cover
      ;; no cover found, use default
      (when emms-browser-default-covers
        (nth size-idx emms-browser-default-covers)))))


(defun emv-make-cover (path)
  "Make a caver from file at PATH."
  (let* ((ext (file-name-extension path))
         (type (cond
                ((string= ext "png")   'png)
                ((string= ext "xbm")   'xbm)
                ((string= ext "xpm")   'xpm)
                ((string= ext "pbm")   'pbm)
                ((string-match "e?ps"
                               ext)    'postscript)
                ((string= ext "gif")   'gif)
                ((string= ext "tiff")  'tiff)
                (t                     'jpeg))))
    (emms-propertize " "
                     'display `(image
                                :type ,type
                                :margin 5
                                :file ,path)
                     'rear-nonsticky '(display))))

(defun emv-get-cover-str (path size)
  "Get the cover string at PATH with SIZE."
  (let ((cover (emv-get-cover-from-path path size)))
    (if cover
        (emv-make-cover cover)
      ;; we use a single space so that cover & no cover tracks line up
      ;; in a terminal
      " ")))

;; --------------------------------------------------
;; Formatting the Data
;; --------------------------------------------------
(defun emv-make-indent (level)
  "Make an indention for the current LEVEL."
  (or
   emv-current-indent
   (make-string (* 1 (1- level)) ?\s)))

(defun emv-format-elem (format-string elem)
  "Format element, look for ELEM in FORMAT-STRING."
  (cdr (assoc elem format-string)))

(defun emv-format-line (bdata &optional target)
  "Return a propertized string from BDATA for TARGET to be inserted in the buffer."
  (unless target
    (setq target 'browser))
  ;; save it for if someone asks to change something through the rings.
  (setq emv-current-target (emv-target-keyword target))
  (let* ((name (or (emms-browser-bdata-name bdata) "misc"))
         (level (emms-browser-bdata-level bdata))
         (type (emms-browser-bdata-type bdata))
         (track (emms-browser-bdata-first-track bdata))
         (props (list 'emms-browser-bdata bdata))
         (path (emms-track-get track 'name))

         (indent (emv-make-indent level))
         (face (emv-get-face bdata))
         (format (emv-get-format bdata target))

         (format-choices
          `(("i" . ,indent)
            ("n" . ,name)
            ("y" . ,(emms-track-get-year track))
            ("A" . ,(emms-track-get track 'info-album))
            ("a" . ,(emms-track-get track 'info-artist))
            ("o" . ,(emms-track-get track 'info-albumartist))
            ("C" . ,(emms-track-get track 'info-composer))
            ("p" . ,(emms-track-get track 'info-performer))
            ("t" . ,(emms-track-get track 'info-title))
            ("g" . ,(emms-track-get track 'info-genre))
	    ("D" . ,(emms-browser-disc-number track))
            ("T" . ,(emms-browser-track-number track))
            ("d" . ,(emms-browser-track-duration track))))
	 str)
    (when (equal type 'info-album)
      (setq format-choices
            (append format-choices
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

    (setq str (emv-format-spec str format-choices))

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

(defun emv-get-face (bdata)
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
     (concat "emv-" name "-face"))))

;; based on gnus code
(defun emv-format-spec (format specification)
  "Return a string based on FORMAT and SPECIFICATION.
FORMAT is a string containing `format'-like specs like \"bash %u %k\",
while SPECIFICATION is an alist mapping from format spec characters
to values.  Any text properties on a %-spec itself are propagated to
the text that it generates."
  (with-temp-buffer
    (insert format)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (cond
       ;; Quoted percent sign.
       ((eq (char-after) ?%)
        (delete-char 1))
       ;; Valid format spec.
       ((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]+\\)")
        (let* ((num (match-string 1))
               (spec (match-string 2))
               (val-alist (assoc spec specification))
               (val (cdr val-alist)))
          (unless val-alist
            (error "Invalid format character: %s" spec))
          ;; Value for a valid spec may not exist. Not an error, just nothing to show.
          (unless val (setq val ""))
          ;; Pad result to desired length.
          (let ((text (format (concat "%" num "s") val)))
            ;; Insert first, to preserve text properties.
            (insert-and-inherit text)
            ;; Delete the specifier body.
            (delete-region (+ (match-beginning 0) (length text))
                           (+ (match-end 0) (length text)))
            ;; Delete the percent sign.
            (delete-region (1- (match-beginning 0)) (match-beginning 0)))))
       ;; Signal an error on bogus format strings.
       (t
        (error "Invalid format string"))))
    (buffer-string)))

;; --------------------------------------------------
;; Display formats - defaults
;; --------------------------------------------------
(defun emv-get-format (bdata target)
  "Get a format for the field type in BDATA for TARGET."
  (let* ((field-type (emms-browser-bdata-type bdata))
         (target-view (cdr (emv-get-target-view-from-view-set target))))
    (or
     (and target-view
          (assoc field-type
                 (assoc :field-formats target-view)))
     emv-default-format)))

(defun emv-track-artist-and-title-format (_bdata fmt)
  "Create a format string for artist and title using FMT."
  (concat
   "%i"
   (let ((track (emv-format-elem fmt "T")))
     (if (and track (not (string= track "0")))
         "%T. "
       ""))
   "%n"))

(defun emms-browser-year-and-album-fmt (_bdata fmt)
  "Create a format string year and album using FMT."
  (concat
   "%i%cS"
   (let ((year (emv-format-elem fmt "y")))
     (if (and year (not (string= year "0")))
         "(%y) "
       ""))
   "%n"))

(defun emms-browser-year-and-album-fmt-med (_bdata fmt)
  "Create a format string year and album with a medium album cover using FMT."
  (concat
   "%i%cM"
   (let ((year (emv-format-elem fmt "y")))
     (if (and year (not (string= year "0")))
         "(%y) "
       ""))
   "%n"))

;; --------------------------------------------------
;; Display faces
;; --------------------------------------------------

(defmacro emv-make-face (name dark-col light-col height)
  "Make a face using NAME, DARK-COL, LIGHT-COL and HEIGHT."
  (let ((face-name (intern (concat "emv-" name "-face"))))
    `(defface ,face-name
       '((((class color) (background dark))
          (:foreground ,dark-col
                       :height ,height))
         (((class color) (background light))
          (:foreground ,light-col
                       :height ,height))
         (((type tty) (class mono))
          (:inverse-video t))
         (t (:background ,dark-col)))
       ,(concat "Face for "
                name
                " in a browser/playlist buffer."))))

(emv-make-face "albumartist" "#aaaabb" "#444455" 1.3)
(emv-make-face "year/genre"  "#aaaaff" "#444477" 1.5)
(emv-make-face "artist"      "#aaaaff" "#444477" 1.3)
(emv-make-face "composer"    "#aaaaff" "#444477" 1.3)
(emv-make-face "performer"   "#aaaaff" "#444477" 1.3)
(emv-make-face "album"       "#aaaaff" "#444477" 1.1)
(emv-make-face "track"       "#aaaaff" "#444477" 1.0)

(provide 'emms-view)
;;; emms-view.el ends here.
