(require 'ox-publish)
(require 'org-jekyll)
(require 'ox-gfm)

;; Export jekyll blog posts from org-mode
;; Extracts subtrees from your org-publish project files that have a :blog:
;; keyword and an :on: property with a timestamp, and exports them to a
;; subdirectory _posts of your project’s publishing directory in the
;; year-month-day-title.html format that Jekyll expects. Properties are
;; passed over as yaml front-matter in the exported files. The title of
;; the entry is the title of the subtree.

;; Configuration options
;; Org-jekyll understands three options that you can add to your
;; project’s org-publish-project-alist:

;; :blog-publishing-directory, where to publish the blog within your site. By
;; default goes to root.

;; :site-root is your blog’s site name, like
;; “https://juanreyero.com”. Used to build the entries’ titles as
;; absolute links. If not set the links will be relative, and will appear
;; broken in some feeds aggregators and in Google Buzz.

;; :jekyll-sanitize-permalinks, set to t if you want your permalinks to
;; be free of áccènted çhars (and therefore safe for all browsers). You
;; probably want to set this to t anyway, just in case.

(setq org-publish-eg "~/play/eg.com/")
(setq org-publish-tb "~/play/tb.com/")

(setq org-jekyll-categories
      '(Meditation
        Tango
        Mindfulness
        Flute
        QMK
        Python
        Clojure
        Emacs
        SPR
        Art
        Sculpture
        Music))

(setq org-jekyll-tags
      '(fressian
        pail
        cascalog
        clojure
        lisp
        spr
        org
        keyboards
        keymap
        mobile
        stabile
        sculpture
        streetArt))


(setq org-jekyll-lang-subdirs '(("en" . "publish-blog/blog/")))
(setq org-jekyll-category nil) ;; set to lang, to do lang specific post folders.

(setq org-jekyll-md-export-func 'org-gfm-export-as-markdown)

(add-to-list 'org-publish-project-alist
             `("eg-com"
               :base-directory "~/play/ericgebhart.github.io/"
               :recursive t
               :base-extension "org"
               :publishing-directory "~/play/ericgebhart.github.io/"
               :blog-publishing-directory "~/play/ericgebhart.github.io/"
               :exclude "^blog\\|^bitacora\\|tb.org"
               :site-root "https://ericgebhart.com/"
               :jekyll-sanitize-permalinks t
               :publishing-function org-md-publish-to-markdown
               :section-numbers nil
               :headline-levels 4
               :table-of-contents t
               :auto-index nil
               :auto-preamble nil
               :body-only nil
               :auto-postamble nil))

(add-to-list 'org-publish-project-alist
             `("eg-img"
               :base-directory "~/play/ericgebhart.github.io/"
               :recursive t
               :exclude "^publish"
               :base-extension "jpg\\|gif\\|png"
               :publishing-directory ,org-publish-eg
               :publishing-function org-html-publish-attachment))

(add-to-list 'org-publish-project-alist
             '("tb" :components ("eg-com"
                                 "tb-com"
                                 "eg-img")))

(add-to-list 'org-publish-project-alist
             `("tb-com"
               :base-directory "~/play/tangobreath.github.io/"
               :base-directory "~/play/tangobreath.github.io/"
               :recursive t
               :base-extension "org"
               :publishing-directory "~/play/tangobreath.github.io/"
               :blog-publishing-directory "~/play/tangobreath.github.io/"
               :site-root "https://tangobreath.com/"
               :jekyll-sanitize-permalinks t
               :publishing-function org-html-publish-to-html
               :section-numbers nil
               :headline-levels 4
               :table-of-contents nil
               :auto-index nil
               :auto-preamble nil
               :body-only t
               :auto-postamble nil))


;; Usage

;; Visit a file that belongs to the org-publish project that contains your
;; blog posts. Run M-x org-jekyll-export-blog if you want to export the
;; whole blog, or M-x org-jekyll-export-current-entry if you only want to
;; update the current entry. See the _posts/ subdirectory of your project’s
;; publish directory being populated with your posts. Then run jekyll.

;; Localization
;; Org-jekyll has some support for localization. If you define a :lang:
;; property in an entry it will be used to look for a [lang].yml localization
;; file in the org-jekyll-localize-dir directory (by default loc/). This
;; yaml file will be included in the front matter of the exported entry. Look
;; at the source of GreaterSkies for an example of how this works.

;; Categories
;; Categories have a special treatment in Jekyll: they are used to change
;; the directory to which the post is written, as in [category]/_posts. You
;; can use org-jekyll-category to specify a property which, if defined in
;; an entry, will be used as a category. It defaults to lang, so that if
;; you are using the :lang: property to localize you’ll end up with posts
;; in places like en/_posts/.

;; Stability and known issues
;; It works well enough that I can use it to produce the blog without much
;; trouble, but I do find glitches from time to time.

;; Do not finish an entry with #+end_quote
;; It messes things up pretty bad. Add an empty line at the end. I don’t
;; know yet why this happens.
