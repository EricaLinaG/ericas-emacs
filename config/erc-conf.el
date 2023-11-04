;;; Package --- configuration for ERC.
;;; Commentary:

(require 'erc)
;;(load-user-file "/path_to/.ercrc.el")

;;Set name / email
(setq erc-user-full-name "user_name")
(setq erc-email-userid "some@email.com")

;;Auto-join #emacs when connecting to Freenode
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
;; Set keybind to auto-connect to Freenode using C-c g i

(global-set-key "\C-ceb" (lambda () (interactive)
                           (erc :server "irc.freenode.net"
                                :port "6667"
                                :nick "zengirl")))

;; Hide/exclude server messages
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
;; Highlight the entire message where current nickname occurs
(setq erc-current-nick-highlight-type 'all)

;; Set char limit so that long lines conform to word wrap
(setq erc-fill-column 78)

;; Rename server buffers to reflect the current network name instead of SERVER:PORT (e.g., "freenode" instead of "irc.freenode.net:6667").
(setq erc-rename-buffers t)

;; Max buffer size
(setq erc-max-buffer-size 10000)

;; Hide timestamps or not
(setq erc-hide-timestamps nil)

;; Use sensible names for irc buffers
(setq erc-rename-buffers t)

;; Spellcheck - requires 'aspell' package
(erc-spelling-mode t)
;; Log options

(setq erc-log-channels-directory "~/.emacs.d/logs/")
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)
;; Highlight certain keywords

(require 'erc-match)
(setq erc-keywords '("kubernetes" "ansible" "org"))
;; Make C-c RET (or C-c C-RET) send messages instead of RET.

(define-key erc-mode-map (kbd "RET") nil)
(define-key erc-mode-map (kbd "C-c RET") 'erc-send-current-line)
(define-key erc-mode-map (kbd "C-c C-RET") 'erc-send-current-line)
;; Channel specific font (e.g. “#emacs>")

;; https://www.emacswiki.org/emacs/ErcConfiguration

(setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))
;; Kill buffers on server exit

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)

;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)

;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

;; Authentication
;; This guide assumes you already registered nickname on the freenode. In order to not store passwords in plain text Emacs has support for handling gpg (GNU Privacy Guard) encrypted netrc/authinfo files and decryption via either passphrases or with public/private key.

;; Plaintext password can be set in .authinfo file in the netRC format and encrypted into .authinfo.gpg file after setting up gpg in Emacs. Package gpg is most likely already part of your distro but in any case you can check that with simply running gpg --version in terminal.

;; Configure Emacs to use GPG
;; More information: https://docs.releng.linuxfoundation.org/en/latest/gpg.html.

;; Emacs: check if you have GPG key configured and loaded with M-x epa-list-secret-keys
;; Shell: list imported keys with gpg2 --list-keys
;; Set options to use gpg:

;; (defun epg-gpg-program (case system-type
;;                          ('darwin "/usr/local/bin/gpg")
;;                          ('windows-nt "C:/Program Files (x86)/GNU/GnuPG/gpg2")))

;; Setting other options to use auth-sources (from GPG manual):

;;     ;; The auth-sources variable tells the auth-source library where your netrc files, Secret Service API collection items, or your password store live for a particular host and protocol. While you can get fancy, the default and simplest configuration is:
;;     ;;; old default: required :host and :port, not needed anymore
(setq auth-sources '((:source "~/.authinfo.gpg" :host t :port t)))
;;     ;;; mostly equivalent (see below about fallbacks) but shorter:
(setq auth-sources '((:source "~/.authinfo.gpg")))
;;     ;;; even shorter and the default:
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
;;     ;;; use the Secrets API Login collection
;;     ;;; (see Secret Service API)
    (setq auth-sources '("secrets:Login"))
;;     ;;; use pass (~/.password-store)
;;     ;;; (see The Unix password store)
    (setq auth-sources '(password-store))
;;     ;;; JSON data in format [{ "machine": "SERVER",
;;     ;;; "login": "USER", "password": "PASSWORD" }...]
    (setq auth-sources '("~/.authinfo.json.gpg"))

;; Set erc-prompt-for-password and erc-prompt-for-nickserv-passwordset to nil
(setq erc-prompt-for-password nil)
(setq erc-prompt-for-nickserv-password nil)
;; Create secrets file with format:

;; machine irc.freenode.net login your_nickname password your_password
;; Create secret and encrypt .authinfo which generates .authinfo.gpg

;; gpg --encrypt .authinfo
;; Set more secure permissions for .authinfo.gpg file:

;; $ chmod 600 ~/.emacs.d/.your_erc_pass.gpg
;; Open and save above file with Emacs. Upon saving Emacs will symmetrically encrypt while prompting you twice for a passphrase and when you reopen the file Emacs will treat is as GnuPG file and decrypt it. You can also secure secrets asymmetrically or using public-private keys.

;; This file will be also decrypted during startup of Emacs. Evaluate init.el with M-x eval-buffer and connect to freenode with C-c g i.

;; You should automatically connect and login with NickServ using your own .gpg file. If you want to autojoin certain channels use the erc-autojoin-channels-alist from above. Since autojoin module is enabled by default you should be reconnect to those same channels next time you connect to freenode.

(provide 'erc-conf)
;;; erc-conf ends here
