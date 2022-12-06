;;; $DOOMDIR/lisp/jdf-custom/config-coding.el -*- lexical-binding: t; -*-

(use-package! projectile
  :defer t
  :config
  (setq projectile-project-search-path '("~/Repos/gateless")
        projectile-ignored-projects '("~/"
                                      "~/.doom.d/"
                                      "~/.config/emacs"
                                      "~/.config/emacs/.local/straight/repos"
                                      "~/.clojure/")
        projectile-sort-order 'recently-active)
  (defun projectile-ignored-project-function (filepath)
    "Return t if FILEPATH is within any of `projectile-ignored-projects'"
    (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects))))

(use-package! code-review
  :defer t
  :config
  (add-hook 'code-review-mode-hook #'emojify-mode)
  (setq code-review-fill-column 96)
  (setq code-review-gitlab-host "gitlab.com/api/v4")
  ;; (setq code-review-auth-login-marker 'forge)
  (define-key forge-topic-mode-map (kbd "C-c v \"") 'code-review-forge-pr-at-point)
  (define-key code-review-mode-map (kbd "C-c C-n") 'code-review-comment-jump-next)
  (define-key code-review-mode-map (kbd "C-c C-p") 'code-review-comment-jump-previous)
  )


;; (use-package! git-gutter
;;   ;; :init
;;   ;; (git-gutter:linum-setup)
;;   :config
;;   (global-git-gutter-mode +1)
;;   (setq git-gutter:modified-sign "Δ"
;;         git-gutter:added-sign "+"
;;         git-gutter:deleted-sign "-"
;;         git-gutter:diff-option "-w"
;;         ;; git-gutter:window-width 2
;;         ;; git-gutter:linum-enabled t
;;         ))

(use-package! diff-hl
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode))

(use-package! git-timemachine
  ;; :hook (git-time-machine-mode . evil-normalize-keymaps)
  :init (setq git-timemachine-show-minibuffer-details t)
  :commands (git-timemachine)
  :config
  (define-key git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-previous-revision)
  (define-key git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-next-revision)
  (define-key git-timemachine-mode-map (kbd "C-k") 'git-timemachine-quit))


(use-package! info-colors
  :defer t
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(defun +hexl/buffer-binary-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.

A binary buffer is defined as containing at least one null byte.

Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion (goto-char (point-min))
                    (search-forward (string ?\x00) nil t 1))))

(defun +hexl/hexl-if-binary ()
  "If `hexl-mode' is not already active, and the current buffer
is binary, activate `hexl-mode'."
  (interactive)
  (unless (eq major-mode 'hexl-mode)
    (when (+hexl/buffer-binary-p)
      (hexl-mode))))

(add-to-list 'magic-fallback-mode-alist '(+hexl/buffer-binary-p . hexl-mode) t)

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; This is *NECESSARY* for Doom users who enabled `dired' module
(map! :map dired-mode-map :ng "q" #'quit-window)

(use-package! rainbow-mode :defer t :config (rainbow-mode 1))

(use-package! flycheck-status-emoji
  :after flycheck
  :hook (flycheck-mode . flycheck-status-emoji-mode))

;; :tools magit
(setq magit-repository-directories '(("~/Repos/gateless" . 2))
      magit-save-repository-buffers nil
      magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
      magit-diff-refine-hunk 'all
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      transient-values '((magit-rebase "--autosquash" "--autostash")
                         (magit-pull "--rebase" "--autostash")
                         (magit-revert "--autostash")))

(use-package! magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package! lispy
  :defer t
  :hook (tree-sitter-query-mode . lispy-mode)
  :bind (:map lispy-mode-map
              ("M-w" . lispy-new-copy)))

;; To enable jsonian to work with flycheck
(after! (jsonian flycheck) (jsonian-enable-flycheck))
;; To diasable so-long mode overrides
(after! (jsonian so-long) (jsonian-no-so-long-mode))

(use-package! aggressive-indent
  :defer t
  :hook
  ((emacs-lisp-mode  . aggressive-indent-mode)
   (common-lisp-mode . aggressive-indent-mode)
   (clojure-mode     . aggressive-indent-mode))
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or  (derived-mode-p 'clojure-mode)
              (derived-mode-p 'emacs-lisp-mode)
              (derived-mode-p 'common-lisp-mode))
         (null (string-match "\\(;\\|\\b\\)"
                             (thing-at-point 'line))))))

(use-package! dumb-jump
  :defer t
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-default-project "~/Repos/gateless")
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package! cider
  :defer t
  :config
  ;; (set-popup-rule! "^\\*cider-repl" :actions '(jdf-cider-load-repl-window))
  ;; (set-popup-rule! "^\\*cider-repl" :regexp t :side 'right :slot 1 :vslot 0 :size 0.45 :quit nil :ttl nil)
  (set-popup-rule! "^\\*cider-repl " :ignore t)
  (set-popup-rule! "^\\*cider-\\(result\\|error\\|macroexpansion\\|ns-browser\\)" :vslot -13 :slot 2 :side 'right :size 0.33 :select 'ignore :quit t)
  (setq nrepl-hide-special-buffers t
        cider-popup-stacktraces nil
        cider-repl-popup-stacktraces t
        cider-repl-wrap-history t
        cider-repl-display-in-current-window nil
        cider-repl-display-help-banner nil
        cider-auto-select-error-buffer t
        cider-overlays-use-font-lock t
        cider-prompt-for-symbol nil
        cider-repl-history-size 10000
        cider-repl-use-pretty-printing t
        cider-history-file "~/.clojure/nrepl-history"
        cider-print-fn 'puget
        cider-print-quota 10485760
        cider-print-buffer-size 8192
        cider-print-options '(("print-length" 4096) ("print-level" 5) ("width" 80))
        cider-result-overlay-position 'at-point
        cider-test-show-report-on-success nil
        cider-auto-select-test-report-buffer nil
        cider-font-lock-dynamically '(macro core function var)
        cider-preferred-build-tool 'clojure-cli
        cider-clojure-cli-aliases "dev:test:debug:clj-kondo"
        cider-repl-pop-to-buffer-on-connect nil
        cider-eldoc-display-for-symbol-at-point nil
        ))

(add-hook 'cider-connected-hook #'cider-upgrade-nrepl-connection)

;; (use-package! cider
;;   :after clojure-mode
;;   :config
;;   (set-lookup-handlers! 'cider-mode nil))

;; (use-package! clj-refactor
;;   :after clojure-mode
;;   :config
;;   (set-lookup-handlers! 'clj-refactor-mode nil))

(add-hook! '(cider-repl-mode-hook
             cider-repl-history-mode-hook)
           #'(lambda () (+word-wrap-mode +1)))

(use-package! neil
  :defer t
  ;; This requires neil to be installed from the cli:
  ;; > brew install babashka/brew/neil
  :config
  (setq neil-prompt-for-version-p nil
        neil-inject-dep-to-project-p t))

(use-package! clj-deps-new :defer t)

(after! lsp-mode
  (setq lsp-lens-enable t
        lsp-auto-guess-root t
        lsp-semantic-tokens-enable nil  ;Disabled in favor of tree-sitter-hl
        lsp-headerline-breadcrumb-enable nil
        lsp-signature-auto-activate nil
        lsp-signature-render-documentation t
        lsp-eldoc-enable-hover t
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-delay 1.5
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-use-webkit t
        lsp-ui-sideline-enable nil
        +lsp-prompt-to-install-server 'quiet)
  ;; ui-ish setup
  (setq lsp-ui-doc-alignment 'window
        lsp-ui-doc-webkit-max-width-px 600
        lsp-ui-doc-border "#272A36"
        lsp-ui-doc-max-height 20
        lsp-ui-doc-enhanced-markdown t
        lsp-ui-doc-text-scale-level 0.8
        lsp-ui-doc-max-width 120
        lsp-ui-peek-fontify 'always
        )
  (custom-set-faces
   `(lsp-headerline-breadcrumb-path-face ((t :inherit custom-link :height 0.6 :underline nil)))
   `(lsp-headerline-breadcrumb-symbols-face ((t :inherit custom-link :weight bold :height 0.75)))
   `(lsp-headerline-breadcrumb-separator-face ((t :inherit custom-link :height 0.6 :underline nil)))
   `(lsp-headerline-breadcrumb-project-prefix-face ((t :inherit custom-link :height 0.6 :underline nil)))
   )

  (add-hook! 'lsp-ui-doc-mode-hook
             #'(lambda () (+word-wrap-mode +1))))

;; (use-package! why-this
;;   :defer t
;;   :config
;;   (setq why-this-annotate-heat-map-cold "#203448")
;;   (setq why-this-annotate-heat-map-warm "#382f27")
;;   :bind ("s-w" . why-this))

;; (use-package! clojure-mode-extra-font-locking
;;   :after clojure-mode
;;   ;; :config
;;   ;; adding fontlocking for clara rules
;;   ;; (font-lock-add-keywords
;;   ;;  'clojure-mode
;;   ;;  '(("\\(=>\\|<-\\)"
;;   ;;     . font-lock-variable-name-face)))
;;   (put-clojure-indent 'match 'defun)
;;   )

(use-package! tree-sitter
  ;; setting up for clojure defs ~/.tree-sitter/bin
  :ensure t
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(clojure-mode . clojure))
  (tree-sitter-load 'clojure
                    "clojure"
                    "tree_sitter_clojure_def")
  ;; (add-hook 'clojure-mode-hook #'tree-sitter-mode)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package! tree-sitter-langs
  :after tree-sitter)

(after! projectile ;; Babashka - projectile bindings
  (projectile-register-project-type
   'babashka '("bb.edn")
   :project-file "bb.edn"
   :test "bb test"
   :test-suffix "_test"))

(setq flycheck-checker-error-threshold 128000)

;; (use-package! flycheck-joker
;;   :if (executable-find "joker")
;;   :after flycheck)

;; (defvar clojure-deps-hook nil
;;   "Utility providing additional functionality to deps.edn project files.")

;; (defun clojure-deps ()
;;   "Fun things to do with clojure deps.edn files"
;;   (interactive)
;;   (run-hooks 'clojure-deps-hook)
;;   (message "clojure-deps hook"))
