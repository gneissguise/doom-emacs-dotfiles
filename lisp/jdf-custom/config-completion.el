;;; $DOOMDIR/lisp/jdf-custom/config-completion.el -*- lexical-binding: t; -*-

(defadvice keyboard-escape-quit
    (around keyboard-escape-quit-dont-close-windows activate)
  (let ((buffer-quit-function (lambda () ())))
    ad-do-it))
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-escape-quit)

(map! "C-<tab>" #'completion-at-point
      "M-<tab>" #'hippie-expand)

(map! "s-t" #'+vterm/toggle)
(map! "s-r" #'er/expand-region)

(use-package! uniquify
  :defer t
  :config
  (setq! uniquify-buffer-name-style 'post-forward-angle-brackets
         uniquify-ignore-buffers-re "^\\*"
         ;; uniquify-separator "/"
         uniquify-strip-common-suffix t
         uniquify-after-kill-buffer-p t))

;; Configure directory extension.
(use-package! vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package! affe
  :defer t
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq input (orderless-pattern-compiler input))
  (cons input (lambda (str) (orderless--highlight input str))))
(setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

(defun consult--orderless-regexp-compiler (input type &rest _config)
  (setq input (orderless-pattern-compiler input))
  (cons
   (mapcar (lambda (r) (consult--convert-regexp r type)) input)
   (lambda (str) (orderless--highlight input str))))
(setq consult--regexp-compiler #'consult--orderless-regexp-compiler)


(use-package! all-the-icons-completion
  :defer t
  :config
  (all-the-icons-completion-mode))

(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

(use-package! consult-jump-project
  :defer t
  ;; :load-path "~/code/emacs/consult-jump-project/"
  ;; :straight (consult-jump-project :type git :host github :repo "jdtsmith/consult-jump-project")
  :bind ("C-c p J" . consult-jump-project))

(use-package! embark-vc
  :after embark)
