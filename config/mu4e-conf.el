;;; Package --- Summary
;;; Commentary:

(require 'mu4e)
(require 'smtpmail)
(require 'gnus-dired)

;;; for special characters hopefully.  éàî etc.
;;(require 'unicode-fonts)
;;(require 'persistent-soft) ; To cache the fonts and reduce load time
;;(unicode-fonts-setup)

;; default
;; (setq mu4e-maildir "~/.mail")
;;store org-mode links to messages

(require 'mu4e-org)
;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

(setq shr-color-visible-luminance-min 80)
(setq gnus-unbuttonized-mime-types nil)

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-attachment-dir  "~/Downloads")

(setq
 mu4e-get-mail-command "mbsync -a"  ;; or fetchmail, or ...
 mu4e-update-interval 300)          ;; update every 5 minutes

;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;; enable inline images
;; Try to show images
(setq mu4e-view-show-images t
      mu4e-show-images t
      mu4e-view-image-max-width 800)
(setq mu4e-view-show-addresses t)
(setq mu4e-use-fancy-chars t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; ;; SMTP requires gnutls-bin.
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-stream-type 'starttls
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

(setq
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 user-mail-address "ericalinagebhart@gmail.com"
 smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
 smtpmail-auth-credentials (expand-file-name "~/.authinfo")
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587  ;; 465 is for SSL.
 smtpmail-debug-info t
 starttls-extra-arguments nil
 starttls-gnutls-program "/usr/local/bin/gnutls-cli"
 starttls-extra-arguments nil
 starttls-use-gnutls t
 )

(setq
 mu4e-maildir       "~/.mail"     ;; top-level Maildir
 mu4e-sent-folder   "/sent"       ;; folder for sent messages
 mu4e-drafts-folder "/drafts"     ;; unfinished messages
 mu4e-trash-folder  "/trash"      ;; trashed messages
 mu4e-refile-folder "/archive")   ;; saved messages

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '(("/ericalinagebhart/inbox" . ?e)
        ;;("/ConciousTango/inbox" . ?y)
        ))

;; ;;these are the defaults.
;; (defvar mu4e-bookmarks
;;   '( ("flag:unread AND NOT flag:trashed" "Unread messages"      ?u)
;;      ("date:today..now"                  "Today's messages"     ?t)
;;      ("date:7d..now"                     "Last 7 days"          ?w)
;;      ("mime:image/*"                     "Messages with images" ?p))
;;   "A list of pre-defined queries; these show up in the main
;;     screen. Each of the list elements is a three-element list of the
;;     form (QUERY DESCRIPTION KEY), where QUERY is a string with a mu
;;     query, DESCRIPTION is a short description of the query (this
;;     shows up in the UI), and KEY is a shortcut key for the query.")

;; Use a function to create a query.
;; For example, to get all the messages that are at most a week old in your inbox:

;; (add-to-list 'mu4e-bookmarks
;;              '((concat "maildir:/inbox AND date:"
;;                        (format-time-string "%Y%m%d" (subtract-time (current-time) (days-to-time 7))))
;;                "Inbox messages in the last 7 days" ?W) t)

;; (setq mu4e-bookmarks
;;       '(("flag:unread AND NOT flag:trashed AND NOT maildir:/Spam" "Unread messages"      ?u)
;;         ("flag:unread AND NOT flag:trashed AND NOT maildir:/Spam AND NOT (/Inbox.General OR /Mediagoblin OR \"/W3C Social WG\""
;;          "Unread non-important messages"                                                 ?U)
;;         ("date:today..now AND NOT maildir:/Spam"                  "Today's messages"     ?T)
;;                                         ;("date:7d..now AND NOT maildir:/Spam"                     "Last 7 days"          ?w)
;;                                         ;("mime:image/* AND NOT maildir:/Spam"                     "Messages with images" ?p)
;;         ("flag:unread AND NOT flag:trashed AND maildir:/Spam"     "Unread spam"          ?s)
;;         ("flag:unread AND maildir:/Inbox.General"  "Unread general inbox" ?i)
;;         ("flag:unread AND (/Inbox.General OR /Mediagoblin OR \"/W3C Social WG\"" "Important messages)" ?I)
;;         ("flag:unread AND maildir:/Mediagoblin"  "MediaGoblin devel" ?m)
;;         ("flag:unread AND \"maildir:/MediaGoblin Tickets\""  "Unread general inbox" ?t)))


;;; Bookmarks - add to the default queries.

(add-to-list 'mu4e-bookmarks
             '("size:5M..500M"                                         "Big messages"     ?b))
(add-to-list 'mu4e-bookmarks
             '("date:today..now AND NOT flag:trashed"                   "Today not trash"  ?T))
(add-to-list 'mu4e-bookmarks
             '("date:today..now AND flag:unread AND NOT flag:trashed"   "Today Unread"     ?U))
(add-to-list 'mu4e-bookmarks
             '("date:7d..now AND NOT flag:trashed"                      "Week not trash"   ?W))
(add-to-list 'mu4e-bookmarks
             '("date:7d..now AND flag:unread AND NOT flag:trashed"      "Week Unread"      ?X))
(add-to-list 'mu4e-bookmarks
             '("maildir:/eric/inbox flag:unread"                        "Unread eric inbox" ?m))



;;; setup the first account for startup.
(setq mu4e-sent-folder "/erica/sent"
      mu4e-trash-folder "/erica/trash"
      mu4e-drafts-folder "/erica/drafts"
      user-mail-address "ericalinagebhart@gmail.com"
      ;;smtpmail-default-smtp-server "smtp.gmail.com"
      ;;smtpmail-local-domain "e.a.gebhart@gmail.com"
      ;;smtpmail-smtp-server "smtp.gmail.com"
      ;;smtpmail-stream-type 'starttls
      ;;smtpmail-smtp-service 25
      )

;; Then create a variable my-mu4e-account-alist, which should contain
;; a list for each of your accounts. Each list should start with the
;; account name, (which must be identical to the account’s directory
;; name under ~/Maildir), followed by (variable value) pairs:

(setq my-mu4e-account-alist
      '(
        ("erica"
         (mu4e-sent-folder "/erica/sent")
         (mu4e-drafts-folder "/erica/drafts")
         (mu4e-trash-folder "/erica/trash")
         (user-mail-address "ericalinagebhart@gmail.com")
         (smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil)))
         (smtpmail-default-smtp-server "smtp.gmail.com")
         (smtpmail-local-domain "gmail.com")
         (smtpmail-smtp-user "ericalinagebhart")
         (smtpmail-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-service 587)
         (smtpmail-debug-info t)
         (starttls-extra-arguments nil)
         (starttls-gnutls-program "/usr/local/bin/gnutls-cli")
         (starttls-extra-arguments nil)
         (starttls-use-gnutls t)
         ;;(smtpmail-smtp-server "smtp.gmail.com")
         ;;(smtpmail-stream-type starttls)
         ;;(smtpmail-smtp-service 25)
         )

        ;; ("tangobreath"
        ;;  (mu4e-sent-folder "/tangobreath/sent")
        ;;  (mu4e-drafts-folder "/tangobreath/drafts")
        ;;  (mu4e-trash-folder "/tangobreath/trash")
        ;;  (user-mail-address "tangobreath@gmail.com")
        ;;  (smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil)))
        ;;  (smtpmail-default-smtp-server "smtp.gmail.com")
        ;;  (smtpmail-local-domain "gmail.com")
        ;;  (smtpmail-smtp-user "tangobreath")
        ;;  (smtpmail-smtp-server "smtp.gmail.com")
        ;;  (smtpmail-smtp-service 587)
        ;;  (smtpmail-debug-info t)
        ;;  (starttls-extra-arguments nil)
        ;;  (starttls-gnutls-program "/usr/local/bin/gnutls-cli")
        ;;  (starttls-extra-arguments nil)
        ;;  (starttls-use-gnutls t)
        ;;  ;;(smtpmail-smtp-server "smtp.gmail.com")
        ;;  ;;(smtpmail-stream-type starttls)
        ;;  ;;(smtpmail-smtp-service 25)
        ;;  )
        ;; ("ericYeti"
        ;;  (mu4e-sent-folder "/ericYeti/sent")
        ;;  (mu4e-drafts-folder "/ericYeti/drafts")
        ;;  (mu4e-trash-folder "/ericYeti/trash")
        ;;  (user-mail-address "eric@yetidata.com")
        ;;  (smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil)))
        ;;  (smtpmail-default-smtp-server "smtp.gmail.com")
        ;;  (smtpmail-local-domain "gmail.com")
        ;;  (smtpmail-smtp-user "eric@yetidata.com")
        ;;  (smtpmail-smtp-server "smtp.gmail.com")
        ;;  (smtpmail-smtp-service 587)
        ;;  (smtpmail-debug-info t)
        ;;  (starttls-extra-arguments nil)
        ;;  (starttls-gnutls-program "/usr/local/bin/gnutls-cli")
        ;;  (starttls-extra-arguments nil)
        ;;  (starttls-use-gnutls t)
        ;;  ;;(smtpmail-smtp-server "smtp.gmail.com")
        ;;  ;;(smtpmail-stream-type starttls)
        ;;  ;;(smtpmail-smtp-service 25)
        ;;  )

        ))

;; You can put any variable you want in the account lists, just make
;; sure that you put in all the variables that differ for each account.
;; Variables that do not differ need not be included. For example, if
;; you use the same smtp server for both accounts, you don’t need to
;; include the smtp-related variables in my-mu4e-account-alist.

;; Note that some SMTP servers (such as Gmail) require the SMTP username
;; to match the user mail address. In this case, your mail is appear
;; to originate from whichever SMTP account you use. Thus unless you
;; are certain your SMTP server does not have this requirement, you
;; should generally use different SMTP account credentials for each
;; mail account.

;; now, the following function can be used to select an account and
;; set the variables in my-mu4e-account-alist to the correct values:

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error (format "No email account found: (%s)" account)))))

;;This function then needs to be added to mu4e-compose-pre-hook:
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(setq mu4e-compose-format-flowed t)

;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; guess or ask the correct context, e.g.

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
;; (setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask
;; '(setq mu4e-compose-context-policy nil)

;; This sets `mu4e-user-mail-address-list' to the concatenation of all
;; `user-mail-address' values for all contexts. If you have other mail
;; addresses as well, you'll need to add those manually.
(setq mu4e-user-mail-address-list
      (delq nil
            (mapcar (lambda (context)
                      (when (mu4e-context-vars context)
                        (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
                    mu4e-contexts)))

;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
	(set-buffer buffer)
	(when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
	  (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
