;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
(unpin! f)

(package! org-roam-ui)
(package! org-modern)
(package! solaire-mode :disable t)
(package! esup)
(package! ts)
(package! cl-lib)
(package! svg-lib)
;; (package! moom)
(package! company-tabnine)
;; (package! company-statistics)
(package! goto-chg)
(package! goto-last-point)
(package! clojure-mode-extra-font-locking)
(package! clj-deps-new)
(package! neil
  :recipe (:host github
           :repo "babashka/neil"
           :files ("*.el")))
(package! clomacs)
(package! magit-delta)

;; (package! emacsql-sqlite)
;; (package! org-modern)
;; (package! org-super-agenda)
;; (package! org-appear
;;   :recipe (:host github
;;            :repo "awth13/org-appear"))
;;(package! ob-clojure-literate
;;  :recipe (:host github :repo "emacsattic/ob-clojure-literate"))
;; (package! org-pandoc-import
;;   :recipe (:host github
;;            :repo "tecosaur/org-pandoc-import"
;;            :files ("*.el" "filters" "preprocessors")))
;; (package! org-auto-tangle)
;; (package! org-link-beautify)
;; (package! org-projectile)
;; (package! graphviz-dot-mode)
;; ;(package! zetteldesk
;; ;  :recipe (:host github :repo "Vidianos-Giannitsis/zetteldesk.el"))

;; Corfu <-> lsp-mode capf integration has not been playing nicely 
;; (package! corfu
;;   :recipe (:host github :repo "minad/corfu"))
;; (package! corfu-doc
;;   :recipe (:host github :repo "galeo/corfu-doc"))
;; (package! cape
;;   :recipe (:host github :repo "minad/cape"))

;; (package! consult-projectile)
;; (package! consult-dir)
;; (package! consult-flycheck)
;; (package! consult-lsp)
;; (package! consult-yasnippet)
;; (package! embark-consult)
(package! why-this)
(package! aggressive-indent)
(package! symbol-overlay)
(package! flycheck-status-emoji)
(package! company-emoji)
(package! modus-themes)
(package! rainbow-mode)
(package! vlf)
(package! info-colors)
;; TODO: it would be nice to have tree-sitter working in clojure-mode
;; (package! tree-sitter
;;   :recipe (:host github :repo "emacs-tree-sitter/elisp-tree-sitter"))
;; (package! tree-sitter-langs
;;   :recipe (:host github :repo "emacs-tree-sitter/tree-sitter-langs"))
;; (package! ts-fold
;;   :recipe (:host github :repo "jcs090218/ts-fold"))
;; (package! helm-tree-sitter)
;; (package! moldable-emacs
;;   :recipe (:host github :repo "ag91/moldable-emacs"))
;; (package! tree-sitter-playground
;;   :recipe (:host github :repo "jeetelongname/tree-sitter-playground"))
;;(package! tree-sitter-indent)
;;(package! tree-sitter-hl
;;  :recipe (:host github :repo "emacs-tree-sitter/elisp-tree-sitter"))
;; ;(package! tree-sitter-debug
;; ;  :recipe (:host github :repo "emacs-tree-sitter/elisp-tree-sitter"))
;; ;(package! tree-sitter-query
;; ;  :recipe (:host github :repo "emacs-tree-sitter/elisp-tree-sitter"))
(package! fireplace)
(package! mixed-pitch)
(package! poke-line)
(package! corfu)
(package! orderless)
(package! kind-icon)
(package! cape)
(package! corfu-doc)
(package! affe)
(package! dumb-jump)
(package! consult-jump-project
  :recipe (:host github :repo "jdtsmith/consult-jump-project"))
(package! git-gutter)
(package! embark-vc)
(package! jsonian :recipe (:host github :repo "iwahbe/jsonian"))
(package! json-mode :disable t)
(package! company-statistics)
(package! el-patch)
(package! prism)
(package! diff-hl)
(package! yasnippet-snippets)
(package! better-scroll)
(package! code-review)
;; (package! dirvish)
(package! shanty-themes)
(package! gitlab-pipeline)
(package! git-link)
(package! cider-eval-sexp-fu)
;; (package! sayid)
(package! tangonov-theme)
(package! cljstyle-format)
(package! buffer-flip)
(package! diredfl)
(package! all-the-icons-completion)

(package! s)
(package! dash)
(package! f
  :recipe (:host github :repo "rejeep/f.el"))  ;; possible emacs 28.2 issue with doom?
(package! f-shortdoc
  :recipe (:host github :repo "rejeep/f.el"))  ;; possible emacs 28.2 issue with doom?

(package! transpose-frame)
(package! color-theme-sanityinc-tomorrow)
