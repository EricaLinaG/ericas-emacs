;; evil-nerd-commenter config
(require 'evil-nerd-commenter)

(evilnc-default-hotkeys)

;; (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
;; (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
;; (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
;; (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)

;; I wish this would work.

;; (require 'evil-leader)
;; (global-evil-leader-mode)
;; (evil-leader/set-key
;;  "ci" 'evilnc-comment-or-uncomment-lines
;;  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
;;  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
;;  "cc" 'evilnc-copy-and-comment-lines
;;  "cp" 'evilnc-comment-or-uncomment-paragraphs
;;  "cr" 'comment-or-uncomment-region
;;  "cv" 'evilnc-toggle-invert-comment-line-by-line
;;  "\\" 'evilnc-comment-operator
;;  )
