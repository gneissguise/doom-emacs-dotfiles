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
(setq doom-font (font-spec :family "Input Mono" :size 17 :weight 'regular)
      doom-big-font (font-spec :family "Input Mono" :size 38 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Articulat CF" :size 16))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq fancy-splash-image (concat doom-private-dir "splash/logo_doom.png"))
(setq doom-theme 'doom-dark+)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(delete-selection-mode 1)                             ; Replace selection when inserting text
(global-subword-mode 1)                           ; Iterate through CamelCase words

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 2                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 history-length 1000
 prescient-history-length 1000)

(setq display-line-numbers-type t
      kill-whole-line t
      auto-save-default t
      make-backup-files t
      confirm-kill-emacs nil
      undo-limit 80000000                          ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                        ; By default while in insert all changes are one big blob. Be more granular
      inhibit-compacting-font-caches t             ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…"                 ; Unicode ellispis are nicer than "...", and also save /precious/ space
      line-spacing 1.5
      which-key-idle-delay 0.5
      vterm-shell "/usr/bin/zsh"
      vterm-always-compile-module t)

(setq posframe-arghandler
  (lambda (buffer-or-name key value)
     (or (and (eq key :lines-truncate)
              (equal ivy-posframe-buffer
                     (if (stringp buffer-or-name)
                       buffer-or-name
                       (buffer-name buffer-or-name)))
        t)
value)))

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
  (toggle-frame-maximized)
  (toggle-frame-fullscreen))

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

(use-package! treemacs
  :defer
  :commands treemacs
  :init
    (map! :leader
      (:prefix ("f" . "file")
        :desc "Open Treemacs" "t" #'+treemacs/toggle))
  :config
    (setq treemacs-width 25)
    (treemacs-git-mode 'deferred)
    (treemacs-follow-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-filewatch-mode t)
    (setq treemacs-is-never-other-window nil)
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(use-package! iedit
  :defer
  :config
    (set-face-background 'iedit-occurrence "Magenta")
  :bind
    ("C-;" . iedit-mode))

(setq ivy-read-action-function #'ivy-hydra-read-action
      ivy-sort-max-size 50000)

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

(when noninteractive
  (add-to-list 'doom-env-whitelist "^SSH_"))
(setq next-screen-context-lines 16)
(after! dired
  (remove-hook 'dired-mode-hook 'dired-omit-mode))

(setq lsp-lens-enable t) ; Code lens

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(use-package! smartparens
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

