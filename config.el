;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Justin Frost"
      user-mail-address "frostjust@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Liberation Mono" :size 14 :weight 'regular)
      doom-big-font (font-spec :family "Liberation Mono" :size 36 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Roboto Medium" :size 20))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic)
  '(doom-dashboard-banner :inherit default)
  '(doom-dashboard-loaded :inherit default))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq fancy-splash-image (concat doom-private-dir "splash/logo_doom.png"))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-loaded)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(custom-theme-set-faces! 'doom-vibrant
  '(line-number :foreground "#4db5bd")
  '(line-number-current-line :foreground "white" :background "#2a2e38"))
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(menu-bar-mode t)
(delete-selection-mode 1)                         ; Replace selection when inserting text
(global-subword-mode 1)                           ; Iterate through CamelCase words

(with-eval-after-load 'evil
  (require 'evil-anzu))

;;
;; Just a few settings to change..
;;

(setq-default dired-listing-switches "-alh"
              delete-by-moving-to-trash t                      ; Delete files to trash
              evil-shift-width 2
              history-length 1000
              prescient-history-length 1000
              tab-width 2                                      ; Set width for tabs
              uniquify-buffer-name-style 'forward              ; Uniquify buffer names
              window-combination-resize t                      ; take new window space from all other windows (not just current)
              x-stretch-cursor t)                               ; Stretch cursor to the glyph width

(setq auto-save-default t
      confirm-kill-emacs nil
      company-minimum-prefix-length 3
      display-line-numbers-type t
      evil-want-fine-undo t                        ; By default while in insert all changes are one big blob. Be more granular
      flycheck-checker-error-threshold 10000
      ivy-read-action-function #'ivy-hydra-read-action
      ivy-sort-max-size 50000
      inhibit-compacting-font-caches t             ; When there are lots of glyphs, keep them in memory
      kill-whole-line t
      line-spacing 0.3
      make-backup-files t
      next-screen-context-lines 16
      prescient-sort-length-enable nil
      truncate-string-ellipsis "…"                 ; Unicode ellispis are nicer than "...", and also save /precious/ space
      undo-limit 80000000                          ; Raise undo-limit to 80Mb
      vterm-shell "/usr/local/bin/zsh"
      vterm-always-compile-module t
      which-key-idle-delay 0.5
      yas-triggers-in-field t)

(setq posframe-arghandler
      (lambda (buffer-or-name key value)
        (or (and (eq key :lines-truncate)
                 (equal ivy-posframe-buffer
                        (if (stringp buffer-or-name)
                            buffer-or-name
                          (buffer-name buffer-or-name)))
                 t)
            value)))

;; Mac OSX settings
(when (eq system-type 'darwin)
  ;;  (setq)
  ;;   ns-command-modifier 'control
  ;;   ns-option-modifier 'meta
  ;;   ns-control-modifier 'super
  ;;   ns-function-modifier 'hyper)
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)
  (global-set-key (kbd "s-x") 'kill-region))

;; runs after 45s of idle time
(run-with-idle-timer 45 t #'doom-save-session)

;; settings to load wc
(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))
;; (add-to-list 'default-frame-alist '(height . 75))
;; (add-to-list 'default-frame-alist '(width . 120))

(add-hook 'prog-mode-hook #'goto-address-mode) ;; Linkify links!

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(use-package! evil-goggles
  :after (evil)
  :config
  (evil-goggles-mode t)
  (setq evil-goggles-pulse t))

(use-package! all-the-icons
  :defer
  :config
  (setq all-the-icons-color-icons t))

(use-package! all-the-icons-dired
  :after (all-the-icons)
  :hook
  (dired-mode-hook . all-the-icons-dired-mode))

(use-package! treemacs
  :defer
  :commands treemacs
  :init
  (map! :leader
        (:prefix ("f" . "file")
         :desc "Open Treemacs" "t" #'+treemacs/toggle))
  :config
  (lsp-treemacs-sync-mode 1)
  (setq treemacs-width 25)
  (treemacs-git-mode 'deferred)
  (treemacs-follow-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-filewatch-mode t)
  (setq treemacs-is-never-other-window nil)
  (setq treemacs-show-hidden-files t)
  (setq treemacs-user-mode-line-format 'doom-modeline)
  (treemacs-load-all-the-icons-with-workaround-font
   (font-spec :family "Liberation Mono" :size 14 :weight 'regular))
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (custom-set-faces! '(treemacs-root-face :height 1.05)))

(use-package! treemacs-evil
  :after (treemacs evil))

(use-package! treemacs-projectile
  :after (treemacs projectile))

(use-package! treemacs-magit
  :after (treemacs magit))

(use-package! lsp-treemacs
  :after (treemacs lsp)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package! treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config
  (setq doom-themes-treemacs-theme "all-the-icons"))

(use-package! iedit
  :defer
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("C-;" . iedit-mode))

(setq ivy-read-action-function #'ivy-hydra-read-action
      ivy-sort-max-size 50000)

(use-package! all-the-icons-ivy-rich
  :init
  (all-the-icons-ivy-rich-mode 1)
  (setq all-the-icons-ivy-rich-color-icon t))

(use-package! ivy-rich
  :init (ivy-rich-mode 1))

(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(setq projectile-ignored-projects '("~/"
                                    "/tmp"
                                    "~/.emacs.d"
                                    "~/.emacs.d/.local/straight/repos/"
                                    "~/.doom.d"
                                    "~/.oh-my-zsh"
                                    "~/.lein"
                                    "~/.clojure"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))
(setq projectile-project-search-path '("~/Repos/"))
(setq projectile-sort-order 'recentf)
(setq projectile-globally-ignored-directories
      '("~/.doom.d/" "~/.emacs.d/" ".clojure" ".config" ".oh-my-zsh" ".lein" ".local" ".lsp" "flow-typed" "node_modules"
        "~/.emacs.d/.local/" ".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs"
        ".tox" ".svn" ".stack-work" ".ccls-cache" ".cache" ".clangd"))

(when noninteractive
  (add-to-list 'doom-env-whitelist "^SSH_"))
(setq next-screen-context-lines 8)
(after! dired
  (remove-hook 'dired-mode-hook 'dired-omit-mode))

(after! lsp-ui
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-enable t)
  (setq lsp-lens-enable t) ; Code lense
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-symbol t)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(project file symbols))
  (setq lsp-headerline-breadcrumb-icons-enable t)
  (custom-set-faces!
    '(lsp-headerline-breadcrumb-path-face :family "Roboto Medium" :height 120)
    '(lsp-headerline-breadcrumb-symbols-face :family "Roboto Medium" :height 120 :slant italic)
    '(lsp-headerline-breadcrumb-project-prefix-face :family "Roboto Medium" :height 120)))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'cider-enlighten-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'visual-line-mode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(use-package! clojure-mode-extra-font-locking)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
;; (setq tab-always-indent 'complete)
(setq cider-print-fn 'puget)
(setq cider-repl-pop-to-buffer-on-connect 'display-only)
(setq cider-repl-require-ns-on-set t)
(setq cider-test-show-report-on-success t)
(setq cider-font-lock-dynamically '(macro core function var))
(setq cider-preferred-build-tool 'clojure-cli)
(setq cider-clojure-cli-global-options "-A:dev:test:portal")

;; smartparens settings
(use-package! smartparens
  :defer
  :init
  (map! :map smartparens-mode-map
        "C-M-f" #'sp-forward-sexp
        "C-M-b" #'sp-backward-sexp
        "C-M-u" #'sp-backward-up-sexp
        "C-M-d" #'sp-down-sexp
        "C-M-p" #'sp-backward-down-sexp
        "C-M-n" #'sp-up-sexp
        "C-M-s" #'sp-splice-sexp
        "C-M-." #'sp-forward-slurp-sexp
        "C->" #'sp-forward-barf-sexp
        "C-M-," #'sp-backward-slurp-sexp
        "C-<" #'sp-backward-barf-sexp))

(defun instant-cheatsheet-search (search-term)
  "Open a browser window and search Instant Clojure Cheatsheet for SEARCH-TERM."
  (interactive (list (read-string "Search Instant Clojure Cheatsheet for: "
                                  (when (symbol-at-point)
                                    (let ((s (copy-sequence (symbol-name (symbol-at-point)))))
                                      (set-text-properties 0 (length s) nil s)
                                      s)))))
  (browse-url (concat "http://localhost:13370/#?q=" (url-hexify-string search-term))))

(after! cider
  (dolist (keymap (list clojure-mode-map cider-repl-mode-map))
    (define-key keymap (kbd "<f12> i") #'instant-cheatsheet-search)))


;; Portal config
;; Leverage an existing cider nrepl connection to evaluate portal.api functions
;; and map them to convenient key bindings.

(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(require 'portal.api) (portal.api/tap) (portal.api/open)"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

;; Example key mappings for doom emacs
(map!
 (:after clojure-mode
  :m "s-o" #'portal.api/open
  :m "C-l" #'portal.api/clear))

;; dash settings
(after! dash (dash-enable-font-lock))

;; web-mode settings
(after! web-mode
  (add-hook 'web-mode-hook #'flycheck-mode)
  (setq web-mode-markup-indent-offset 2 ;; Indentation
        web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil ;; disbale adding "" after an =
        web-mode-auto-close-style 2))

(use-package! origami
  :after clojure-mode
  :defer
  :config
  (origami-mode)
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(use-package! origami-predef
  :requires origami
  :defer
  :config

  (defun origami-predef-java()
    "Close some predefined patterns, useful in java."
    (interactive)
    (origami-predef-apply-patterns '("private .*{" "protected .*{")))
  (add-hook 'java-mode-hook #'origami-predef-java)

  (defun origami-predef-sql()
    "Close some predefined patterns, useful in SQL."
    (interactive)
    (origami-predef-apply-patterns '("create .*table" "create .*view" "begin")))
  (add-hook 'sql-mode-hook #'origami-predef-sql)


  (defun origami-predef-gift()
    "Close some predefined patterns, useful in gift-mode (https://github.com/csrhodes/gift-mode)"
    (interactive)
    (origami-predef-apply-patterns "{" ))
  (add-hook 'gift-mode-hook #'origami-predef-gift)

  (defun origami-predef-clojure()
    "Close defun types"
    (interactive)
    (origami-predef-apply-patterns '("(defn.*)"))))

;; customizing tabs
(after! centaur-tabs
  (custom-set-faces!
    '(centaur-tabs-default :family "Helvetica Neue" :height 120)
    '(centaur-tabs-selected :family "Helvetica Neue" :height 120)
    '(centaur-tabs-selected-modified :family "Helvetica Neue" :height 120 :slant italic)
    '(centaur-tabs-unselected :family "Helvetia Neue" :height 120)
    '(centaur-tabs-unselected-modified :family "Helvetica Neue" :height 120)
    '(centaur-tabs-active-bar-face :family "Helvetica Neue" :height 120))
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-height 28)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-icon-scale-factor 0.85)
  (setq centaur-tabs-icon-v-adjust 0.10)
  (centaur-tabs-headline-match))
