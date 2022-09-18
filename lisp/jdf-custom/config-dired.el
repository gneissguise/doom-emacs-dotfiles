;;; $DOOMDIR/lisp/jdf-custom/config-dired.el -*- lexical-binding: t; -*-

;; (use-package dirvish
;;   :init
;;   (dirvish-override-dired-mode)
;;   :custom
;;   (dirvish-quick-access-entries
;;    '(("h" "~/"                          "Home")
;;      ("d" "~/Downloads/"                "Downloads")
;;      ("r" "~/Repos"                       "Repos")
;;      ("g" "~/Repos/gateless"                       "Gateless")
;;      ("t" "~/.local/share/Trash/files/" "TrashCan")))
;;   ;; (dirvish-header-line-format '(:left (path) :right (free-space)))
;;   (dirvish-mode-line-format ; it's ok to place string inside
;;    '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
;;   ;; Don't worry, Dirvish is still performant even you enable all these attributes
;;   (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
;;   ;; Maybe the icons are too big to your eyes
;;   ;; (dirvish-all-the-icons-height 0.8)
;;   ;; In case you want the details at startup like `dired'
;;   ;; (dirvish-hide-details nil)
;;   :config
;;   (dirvish-peek-mode)
;;   ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
;;   (setq dired-dwim-target t)
;;   (setq delete-by-moving-to-trash t)
;;   ;; Enable mouse drag-and-drop files to other applications
;;   (setq dired-mouse-drag-files t)                   ; added in Emacs 29
;;   (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
;;   ;; Make sure to use the long name of flags when exists
;;   ;; eg. use "--almost-all" instead of "-A"
;;   ;; Otherwise some commands won't work properly
;;   ;; (setq dired-listing-switches
;;   ;;       "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
;;   :bind
;;   ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
;;   (("C-c f" . dirvish-fd)
;;    ;; Dirvish has all the keybindings in `dired-mode-map' already
;;    :map dirvish-mode-map
;;    ;; ("h" . dired-up-directory)
;;    ;; ("j" . dired-next-line)
;;    ;; ("k" . dired-previous-line)
;;    ;; ("l" . dired-find-file)
;;    ;; ("i" . wdired-change-to-wdired-mode)
;;    ;; ("." . dired-omit-mode)
;;    ("a"   . dirvish-quick-access)
;;    ("f"   . dirvish-file-info-menu)
;;    ("y"   . dirvish-yank-menu)
;;    ("N"   . dirvish-narrow)
;;    ("^"   . dirvish-history-last)
;;    ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;;    ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;;    ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;;    ("TAB" . dirvish-subtree-toggle)
;;    ("M-f" . dirvish-history-go-forward)
;;    ("M-b" . dirvish-history-go-backward)
;;    ("M-l" . dirvish-ls-switches-menu)
;;    ("M-m" . dirvish-mark-menu)
;;    ("M-f" . dirvish-layout-toggle)
;;    ("M-s" . dirvish-setup-menu)
;;    ("M-e" . dirvish-emerge-menu)
;;    ("M-j" . dirvish-fd-jump)))

(use-package dired-x
  :defer t
  ;; Enable dired-omit-mode by default
  ;; :hook
  ;; (dired-mode . dired-omit-mode)
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; Addtional syntax highlighting for dired
(use-package! diredfl
  :defer t
  :hook (dired-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

;; Use `all-the-icons' as Dirvish's icon backend
(use-package! all-the-icons :defer t)
