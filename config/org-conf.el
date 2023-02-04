;;; package: --- Summary
;;; commentary:
;;; code:


(require 'org)
(require 'org-pretty-table)
(require 'evil-org)

(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(add-hook 'org-mode-hook (lambda () (org-pretty-table-mode)))

(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(add-hook 'org-mode-hook 'org-mode-setup)

(defun ee/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.75)
                  (org-level-2 . 1.5)
                  (org-level-3 . 1.25)
                  (org-level-4 . 1.2)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; (let* ((variable-tuple
  ;;         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
  ;;               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
  ;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
  ;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
  ;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
  ;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
  ;;        (base-font-color     (face-foreground 'default nil 'default))
  ;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  ;; (custom-theme-set-faces
  ;;  'user
  ;;  `(org-level-8 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-7 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-6 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-5 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
  ;;  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
  ;;  `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
  ;;  `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(setq org-ellipsis " ▾")
(setq org-return-follows-link t)

;; Clocking
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; (setq org-clock-into-drawer "CLOCKING") ; send it to it's own drawer.
(setq org-clock-idle-time 15)
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
;; Do not prompt to resume an active clock, just resume it
(setq org-clock-persist-query-resume nil)
;; Change tasks to whatever when clocking in
(setq org-clock-in-switch-to-state "NEXT")
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
;; with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; use pretty things for the clocktable
(setq org-pretty-entities t)
;; Record time and note when a task is completed
(setq org-log-done 'note)

;; Record time and note when the scheduled date of a task is modified
(setq org-log-reschedule 'note)

;; Record time and note when the deadline of a task is modified
(setq org-log-redeadline 'note)

;; Record time and note when clocking out of a task
;; (setq org-log-clock-out 'note)
;; Record note when clocking out
(setq org-log-note-clock-out t)

;; Record time and note when a task is refiled
(setq org-log-refile 'note)

(setq org-agenda-files (file-expand-wildcards "~/Documents/org/*.org"))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

;; (setq org-refile-targets
;;       '(("Archive.org" :maxlevel . 1)
;;         ("Tasks.org" :maxlevel . 1)))

;; (setq org-agenda-files
;;       '("~/play/OrgFiles/Tasks.org"
;;         "~/play/OrgFiles/Habits.org"
;;         "~/play/OrgFiles/Birthdays.org"))


(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n/!)" "|" "DONE(d@/!)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h@/!)" "|" "COMPLETED(c)" "CANC(k@/!)")))


;; Save Org buffers after refiling!

(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-tag-alist
      '((:startgroup)
                                        ; Put mutually exclusive tags here
        (:endgroup)
        ("house" . ?h)
        ("awareness" . ?a)
        ("blog" . ?b)
        ("code" . ?c)
        ("tango" . ?t)
        ("thought" . ?T)
        ("research" . ?r)
        ("planning" . ?p)
        ("publish" . ?P)
        ("note" . ?n)
        ("IT" . ?I)
        ("qmk" . ?q)
        ("emacs" . ?e)
        ("idea" . ?i)))

;; Configure custom agenda views

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))
          (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

        ("n" "Next Tasks"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ("W" "Work Tasks" tags-todo "+work-email")

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))

        ("w" "Workflow Status"
         ((todo "WAIT"
                ((org-agenda-overriding-header "Waiting on External")
                 (org-agenda-files org-agenda-files)))
          (todo "REVIEW"
                ((org-agenda-overriding-header "In Review")
                 (org-agenda-files org-agenda-files)))
          (todo "PLAN"
                ((org-agenda-overriding-header "In Planning")
                 (org-agenda-todo-list-sublevels nil)
                 (org-agenda-files org-agenda-files)))
          (todo "BACKLOG"
                ((org-agenda-overriding-header "Project Backlog")
                 (org-agenda-todo-list-sublevels nil)
                 (org-agenda-files org-agenda-files)))
          (todo "READY"
                ((org-agenda-overriding-header "Ready for Work")
                 (org-agenda-files org-agenda-files)))
          (todo "ACTIVE"
                ((org-agenda-overriding-header "Active Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "COMPLETED"
                ((org-agenda-overriding-header "Completed Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "CANC"
                ((org-agenda-overriding-header "Cancelled Projects")
                 (org-agenda-files org-agenda-files)))))))


;; (setq org-capture-templates
;;       '(("t" "TODO" entry (file+headline as/gtd "Collect")
;;          "* TODO %? %^G \n  %U" :empty-lines 1)
;;         ("s" "Scheduled TODO" entry (file+headline as/gtd "Collect")
;;          "* TODO %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 1)
;;         ("d" "Deadline" entry (file+headline as/gtd "Collect")
;;          "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
;;         ("p" "Priority" entry (file+headline as/gtd "Collect")
;;          "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
;;         ("a" "Appointment" entry (file+headline as/gtd "Collect")
;;          "* %? %^G \n  %^t")
;;         ("l" "Link" entry (file+headline as/gtd "Collect")
;;          "* TODO %a %? %^G\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
;;         ("n" "Note" entry (file+headline as/gtd "Notes")
;;          "* %? %^G\n%U" :empty-lines 1)
;;         ("j" "Journal" entry (file+datetree "/Users/andrew/org/agenda/journal.org")
;;          "* %? %^G\nEntered on %U\n")))

;; ("e" "Event" entry (file+headline "~/org/Events.org" "Transient")
;;  "* EVENT %?\n%U" :empty-lines 1)
;; ("E" "Event With Clipboard" entry (file+headline "~/org/Events.org" "Transient")
;;  "* EVENT %?\n%U\n   %c" :empty-lines 1))


;; (setq org-capture-templates
;;       (append '(("l" "Ledger entries")
;;                 ("lm" "MBNA" plain
;;                  (file "~/personal/ledger")
;;                  "%(org-read-date) %^{Payee}
;;   Liabilities:MBNA
;;   Expenses:%^{Account}  %^{Amount}
;; ")
;;                 ("lc" "Cash" plain
;;                  (file "~/personal/ledger")
;; 	         "%(org-read-date) * %^{Payee}
;;   Expenses:Cash
;;   Expenses:%^{Account}  %^{Amount}
;; "))
;;               org-capture-templates))

;; (setq org-capture-templates
;;       (append '(("i" "Income Ledger Entry")
;;                 ("ig" "Income:Gifts" plain
;;                  (file ledger-journal-file)
;;                  "%(org-read-date) * receive %^{Received From} %^{For why}
;; Assets:%^{Account|Personal|Home}  %^{Amount} %^{Currency|CNY|USD|JPY}
;; Income:Gifts
;; "))
;;               org-capture-templates))

;; (setq org-capture-templates
;;       (append '(("e" "Expense Ledger Entry ")
;;                 ("eg" "Expense:Gifts" plain
;;                  (file ledger-journal-file)
;;                  "%(org-read-date) * send %^{Send to} %^{For why}
;; Expense:Gifts  %^{Amount}  %^{Currency|CNY|USD|JPY}
;; Assets:%^{Account||Personal|Home}
;; "))
;;               org-capture-templates))
;; Capture template

;; needs clip-link

;; ("l" "Daily Bookmarks" entry
;;  (file+headline (lambda () (personal-note 'daily)) "Bookmarks")
;;  "** %(org-cliplink-capture)%?\n" :unnarrowed t)

;; ;; Utility function
;; (defun personal-note (ntype)
;;   (cond
;;    ((string= 'daily ntype) (concat org-directory (format-time-string "/%Y/%B_%-e.org")))
;;    ((string= 'work ntype) (concat org-directory (format-time-string "/work/%Y/note_%m_%d.org")))
;;    (t (error "Invalid personal note type: " ntype))))

(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-directory "~/org")

(setq org-capture-templates
      `(
        ;; REFERENCE(f) Reference template
        ("r" "REFERENCE (f) Reference org-protocol" entry (file "ref.org")
         "* REFERENCE [[%:link][%:description]]
  CLOSED: %U
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"REFERENCE\"  from \"\"           %U
  :END:

  %:initial" :empty-lines 1)

        ("C" "Complicated ones.")
        ;;          (p) Paycheck template
        ("Cp" "          (p) Paycheck" entry (file+headline "fin.org" "Paycheck")
         "* Paycheck :fin:
  :PROPERTIES:
  :Cost:     -%^{Amount}
  :Paid:     -%\\1
  :Method:   [[fin:Wells Fargo Debit Account][Wells Fargo Debit Account]]
  :Merchant: [[peo:General Atomics Aeronautical Systems Inc.][General Atomics Aeronautical Systems Inc.]]
  :Link:     [[val:fin/Banking/Work/General Atomics Aeronautical Systems Inc./Paycheck/%<%Y-%m-%d>.pdf][%<%Y-%m-%d>.pdf]]
  :Note:     %?
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T" :empty-lines 1)

        ("Cs" "          (s) Shopping" entry (file "ref.org")
         "* %^{Action|Paid|Shopped at|Ate at|Drank at} %^{Merchant|Sprouts Hillcrest San Diego|Trader Joe's Hillcrest San Diego|Trader Joe's Mira Mesa San Diego|Farmer's Market Hillcrest San Diego|Costco Poway|Costco Mission Valley San Diego|Target Mission Valley San Diego|Poncho Villa North Park San Diego|VONS Poway|Ralphs Hillcrest San Diego|Whole Foods Hillcrest San Diego} :fin:
  :PROPERTIES:
  :Cost:     %^{Cost}
  :Paid:     %^{Paid}
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\5]]
  :Merchant: [[peo:%\\2][%\\2]]
  :Link:     %?
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T

  | Item                           | Price ($) | Amount    | Total ($) |
  |                                | <9>       | <9>       | <9>       |
  |--------------------------------+-----------+-----------+-----------|
  |                                |           |           |           |
  |                                |           |           |           |
  |--------------------------------+-----------+-----------+-----------|
  | Tax                            |           | 1         |           |
  | Total                          |           |           |           |
  #+TBLFM: $4=$2*$3;%.2f::@>$4=vsum(@3..@-1);%.2f
  " :empty-lines 1)


        ("tS" "Sample" entry (file+olp "~/org/projects.org" "Sample")
         "* SAMPLE %^{Description}
          %?
          :PROPERTIES:
          :Creator:  %^{creator}
          :Created:  %U
          :Link:     %^{link}
          :Date:     %^{date}
          :Note:     %^{note}
          :END:
          :LOGBOOK:
             - Added: %U
          :END:"
         :empty-lines 1)

        ("D" "Daily List" entry
         (file+olp+datetree "~/org/Journal.org")
         "* Daily Tasks
          %?
          :PROPERTIES:
          :END:
            [] Meditate
            [] Flute
            [] Exercise
            [] Jardinage/Weeding
            [] Write
            [] Code
          :LOGBOOK:
             - Added: %U
          :END:"
         :empty-lines 1)

        ("s" "Code Snippet" entry (file "~/org/code-snippets.org")
         ;; Prompt for tag and language
         "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")

        ("t" "Tasks / Projects")
        (("tm" "todo" entry (file+headline "~/org/todo.org" "Tasks")
          "* TODO [#A] %?
           SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))
           %a"))
        ("ti" "Inbox" entry (file+olp "~/org/projects.org" "Inbox")
         "* TODO %^{Description}
          %?
          :LOGBOOK:
             - Added: %U
          :END:"
         :empty-lines 1)
        ("td" "Todo" entry (file "~/org/projects.org")
         "* TODO %^{Description}
          %?
          :LOGBOOK:
             - Added: %U
          :END:"
         :empty-lines 1)
        ("tD" "Todo with Clipboard" entry (file "~/org/projects.org")
         "* TODO %^{Description}
          %?
          %c
          :LOGBOOK:
            - Added: %U
          :END:"
         :empty-lines 1)

        ("n" "Note Entries")
        ("nn" "Note" entry (file "~/org/Refile.org")
         "* NOTE %^{Description}
          %?
          %c
          :LOGBOOK:
            - Added: %U
          :END:"
         :empty-lines 1)
        ("nt" "Note with tags" entry (file "~/org/Refile.org")
         "* NOTE %^{Description}
          %?
          %c
          :LOGBOOK:
            - Added: %U
          :END:"
         :empty-lines 1)
        ("nN" "Note with Clipboard" entry (file "~/org/Refile.org")
         "* NOTE %^{Description}
          %?
          %c
          :LOGBOOK:
            - Added: %U
          :END:"
         :empty-lines 1)
        ("nh" "note headline" entry
         (file+headline "~/org/DT.org" "Notes")
         "* %<%I:%M %p> - %a :note:%^g\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
        ("nl" "link" entry
         (file+olp "~/org/Refile.org" "Bookmarks")
         "** %(org-cliplink-capture)%?"
         :unnarrowed t)


        ("j" "Journal Entries")
        ("jj" "Journal" entry
         (file+olp+datetree "~/org/Journal.org")
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         ;; ,(dw/read-file-as-string "~/org/Templates/Daily.org")
         :clock-in :clock-resume
         :empty-lines 1)
        ("jr" "Random" entry
         (file+olp+datetree "~/org/Journal.org")
         "\n* %<%I:%M %p> - Random :random:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
        ("jL" "Learn" entry
         (file+olp+datetree "~/org/Journal.org")
         "\n* %<%I:%M %p> - To Learn :learn:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
        ("jl" "Language" entry
         (file+olp+datetree "~/org/Journal.org")
         "\n* %<%I:%M %p> - Journal :language:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
        ("jm" "Meeting" entry
         (file+olp+datetree "~/org/Journal.org")
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)

        ("b" "Book Entries")
        ("bc" "Concept" entry
         (file+headline "~/org/DT.org" "Capture")
         "\n* %<%I:%M %p> - Concept :concept:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
        ("ba" "Awareness" entry
         (file+headline "~/org/DT.org" "Capture")
         "\n* %<%I:%M %p> - Awareness :awareness:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
        ("bp" "Physical" entry
         (file+headline "~/org/DT.org" "Capture")
         "\n* %<%I:%M %p> - Physical :physical:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)


        ("w" "Workflows")
        ("we" "Checking Email" entry
         (file+olp+datetree "~/org/Journal.org")
         "* Checking Email :email:\n\n%?"
         :clock-in :clock-resume :empty-lines 1)

        ("m" "Metrics Capture")
        ("mw" "Weight" table-line
         (file+headline "~/org/Metrics.org" "Weight")
         "| %U | %^{Weight} | %^{Notes} |"
         :kill-buffer t)))


(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))

(ee/org-font-setup)

(setq org-hide-emphasis-markers t)

(require 'org-bullets)
(add-hook 'org-mode-hook #'org-bullets-mode)

;;(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))

(require 'visual-fill-column)

(defun ee/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(add-hook 'org-mode-hook  #'ee/org-mode-visual-fill)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)


;; This is needed as of Org 9.2
(require 'tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; (add-to-list 'org-structure-template-alist
;; 	     '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

;; Automatically tangle our Emacs.org config file when we save it
(defun ee/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ee/org-babel-tangle-config)))
