    ;;; mypackages --- A list of all the packages to keep installed.
    ;;; Commentary:
;; All my packages, easy to install and update.
;; Provides two functions, one for install, one for update.
    ;;; Code:

(defvar my/packages
  '(golden-ratio
    general
    projectile
    perspective
    find-file-in-project
    dashboard
    windower
    expand-region
    dmenu
    async
    nov

    vertico
    ;;vertico-directory
    marginalia
    orderless
    consult
    embark
    embark-consult
    cape
    corfu
    corfu-terminal

    posframe
    vertico-posframe
    helm-posframe
    transient-posframe
    which-key-posframe

    ;;ido ido-flx ido-imenu
    smex

    ;; helm - an experiment in progess...
    helm
    helm-projectile
    helm-fish-completion
    helm-evil-markers
    helm-descbinds
    helm-swoop
    helm-org
    helm-cider
    cljr-helm

    helm-ag
    helm-sly
    helm-clojuredocs

    ;; hydra -- A lot to think about here...
    ;; https://github.com/abo-abo/hydra/wiki/Hydras-by-Topic
    ;; have to bind them, and try them, and make some.
    ;; maybe evil-leader stuff can just pop over.
    ;; maybe a few at first, that I use all the time.
    ;; ,fqgbsiaw   hmmm. don't know it's necessary...
    hydra pretty-hydra cider-hydra major-mode-hydra

    dash dash-functional

    emms
    emms-info-mediainfo
    emms-mark-ext
    ;; emms-player-simple-mpv
    emms-soundcloud
    ;; emms-state
    helm-emms
    org-emms

    diff-hl ;; has a dired mode.

    all-the-icons
    all-the-icons-dired
    all-the-icons-completion
    all-the-icons-ibuffer

    diredfl
    dired-rsync
    fd-dired
    ranger

    ;; modeline
    ;; smart-mode-line rich-minority
    doom-modeline ;; the doom modeline

    ibuffer-projectile
    ibuffer-tramp
    ibuffer-git

    ;; Multi-language support
    google-translate
    langtool
    mw-thesaurus
    powerthesaurus

    ;;navigation
    ace-jump-mode ace-window frog-jump-buffer ace-jump-buffer
    ;; basic tools
    which-key
    session

    exwm
    helm-exwm
    exwm-mff
    perspective-exwm

    ;; eshell
    eshell-autojump
    fish-completion

    multi-term
    vterm

    ;; evil-mode
    evil
    evil-nerd-commenter
    evil-leader
    evil-mu4e
    evil-org

    ;; Parentheses.
    evil-surround
    highlight-parentheses
    ;; paredit evil-paredit
    smartparens evil-smartparens evil-cleverparens
    ;; lispy lispyville ;; -- not a fan.
    rainbow-mode
    mic-paren

    ;; git
    magit git-gutter
    el-get
    gited

    ;;coding support
    restclient
    restclient-helm

    eldoc
    auto-compile
    company
    origami
    undo-tree
    flycheck-tip
    kibit-helper
    flycheck-pos-tip
    aggressive-indent

    ;; Silver Surfer, grep, ctags.
    ag wgrep wgrep-ag ctags-update

    ;;extras
    ;;treemacs treemacs-evil treemacs-magit treemacs-projectile
    expand-region floobits
    gist
    exec-path-from-shell

    ;; clojure -- need to rexamine this. lots of newer stuff.
    cider clj-refactor ac-cider
    cider-eval-sexp-fu
    clojure-mode eval-sexp-fu clojure-mode-extra-font-locking ;popup
    uuid rainbow-delimiters flycheck-clojure
    cider-hydra
    flycheck-clj-kondo
    4clojure

    ;; clojure script
    cljsbuild-mode smartscan

    lsp-mode
    lsp-treemacs
    lsp-ui

    lua-mode
    company-lua
    ruby-mode
    hy-mode

    json-mode
    markdown-mode
    yaml-mode
    apache-mode

    ;;Shell
    company-shell

    ;;C/C++
    ;; Not sure, need to pursue a better C/C++ setup
    ;;irony company-irony company-ctags helm-etags ;company-rtags

    ;;haskell
    haskell-mode
    ;;ghc
    haskell-snippets
    dante ;; GHCi
    ;;helm-ghc
    flycheck-haskell

    ;;scheme/common lisp
    geiser
    sly
    flycheck-guile
    buttercup

    ;;Python
    elpy
    pyenv-mode ein
    python-docstring
    py-autopep8
    py-yapf pydoc
    python-black

    ;;org mode.
    org
    org-roam
    org-ref
    org-ref-prettify
    org-auto-tangle
    org-roam-timestamps
    org-roam-bibtex
    org-rich-yank
    org-pretty-tags
    org-babel-eval-in-repl
    org-bullets
    visual-fill-column
    org-cliplink
    org-drill
    org-drill-table
    ox-gfm

    ;;slack - hasn't worked very well so far..
    slack
    oauth2
    alert
    emojify
    ;;helm-slack

    ;; themes
    modus-themes
    doom-themes
    monokai-theme monokai-alt-theme obsidian-theme atom-dark-theme
    bubbleberry-theme atom-one-dark-theme
    ujelly-theme twilight-theme
    tango-2-theme tango-plus-theme tangotango-theme zenburn-theme
    waher-theme underwater-theme toxi-theme sublime-themes
    subatomic-theme sunny-day-theme subatomic256-theme
    soft-stone-theme soft-morning-theme purple-haze-theme
    noctilux-theme naquadah-theme leuven-theme lavender-theme
    light-soap-theme ir-black-theme inkpot-theme heroku-theme
    github-theme gandalf-theme flatland-theme firecode-theme
    flatui-theme espresso-theme django-theme darkmine-theme
    darcula-theme oldlace-theme cyberpunk-theme clues-theme
    busybee-theme boron-theme bliss-theme
    ample-zen-theme ample-theme lush-theme smyx-theme gotham-theme
    solarized-theme dark-krystal-theme))

;;
;; Install stuff from packages.

(defun early-install-mypackages ()
  "Install a theme and any other early things."
  (dolist (pkg '(modus-themes))
    (unless (package-installed-p pkg))
    (package-install pkg)))

(defun install-mypackages ()
  "Install my/packages if they arent already."
  (dolist (pkg my/packages)
    (unless (package-installed-p pkg))
    (package-install pkg)))

(defun update-mypackages ()
  "Update my/packages if they need it."
  (interactive)
  (dolist (pkg my/packages)
    (package-install pkg)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; let this be controlled elsewhere.
;;(install-mypackages)
;;(update-mypackages)

(provide 'mypackages)
  ;;; mypackages.el ends here (emacs-lisp-checkdoc)
