;;; $DOOMDIR/lisp/jdf-custom/config-ui.el -*- lexical-binding: t; -*-

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(defun my-doom-modeline-normalizer ()
  (set-face-attribute 'mode-line nil :height 170)
  (set-face-attribute 'mode-line-inactive nil :height 170)
  (setq doom-modeline-bar-width 4
        doom-modeline-height 1
        doom-modeline-modal-icon nil))

(use-package! modus-themes
  :defer t
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-variable-pitch-ui nil
        modus-themes-region '(bg-only no-extend)
        modus-themes-syntax '(yellow-comments green-strings alt-syntax)
        modus-themes-mode-line '(borderless accented)
        x-underline-at-descent-line t
        modus-themes-tabs-accented t
        modus-themes-completions '((matches extrabold background intense)
                                   (selection semibold accented intense)
                                   (popup accented intense))
        modus-themes-fringes nil
        modus-themes-lang-checkers '(text-also background)
        modus-themes-subtle-line-numbers t
        modus-themes-hl-line '(intense)
        modus-themes-markup '(bold italic intense)
        modus-themes-paren-match '(bold intense)
        modus-themes-region '(bg-only no-extend)
        modus-themes-org-blocks '(tinted-background)
        modus-themes-org-agenda '((header-block . (variable-pitch 1.2))
                                  (header-date . (grayscale workaholic bold-today 1.05))
                                  (event . (accented italic varied))
                                  (scheduled . uniform)
                                  (habit . traffic-light))
        modus-themes-headings '((1 . (background overline variable-pitch 1.2))
                                (2 . (overline rainbow 1.05))
                                (3 . (overline 1.0))
                                (t . (semibold))))
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)

  :config
  (setq doom-font (font-spec :family "Iosevka Comfy Fixed" :size 20)
        doom-variable-pitch-font (font-spec :family "Iosevka Comfy" :size 22)
        doom-big-font (font-spec :family "Iosevka Comfy" :size 24)
        doom-unicode-font (font-spec :family "Iosevka Comfy Fixed" :size 20))

  (setq doom-theme 'modus-vivendi)

  (setq ;;+pretty-code-fira-font-name "Fira Code Symbol"
   ;;+pretty-code-hasklig-font-name "Hasklig"
   +pretty-code-iosevka-font-name "Iosevka Comfy Fixed")
  (doom-themes-org-config)
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi)
  (my-doom-modeline-normalizer)
  :bind ("<f5>" .  modus-themes-toggle))

(defun my-modus-themes-custom-faces ()
  (modus-themes-with-colors
    (custom-set-faces
     `(default ((,class :weight semi-light :width semi-condensed)))
     `(bold ((,class :weight semi-bold)))
     `(mode-line ((,class :distant-foreground nil :box nil :underline nil :overline nil)))
     `(mode-line-inactive ((,class :distant-foreground nil :box nil :underline nil :overline nil)))
     `(modus-themes-ui-variable-pitch ((,class :distant-foreground nil :box nil :underline nil :overline nil)))
     `(corfu-annotations ((,class :slant oblique)))))
  (my-doom-modeline-normalizer))

(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)
(global-set-key (kbd "S-<down-mouse-1>") #'mouse-set-mark)
(global-set-key (kbd "A-<down-mouse-1>") #'mouse-drag-region-rectangle)

(use-package! poke-line
  :defer t
  :config
  (poke-line-global-mode 1)
  (setq-default poke-line-pokemon "magikarp")
  (setq poke-line-bar-length 24
        poke-line-size 4))

(use-package! buffer-flip
  :defer t
  :bind  (("C-|" . buffer-flip)
          :map buffer-flip-map
          ( "C-|" .   buffer-flip-forward)
          ( "C-M-|" . buffer-flip-backward)
          ( "M-ESC" .     buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$"
          "^\\*Messages*"
          "^\\*Compile-Log*"
          "^\\*envrc*"
          "lsp")))

(defun jdf-golden-ratio-margin-update (side &optional fill-limit)
  "Divide screen halfs using golden ratio, and return 'side-a (bigger) or 'side-b (smaller)"
  (let* ((max-width (frame-width))
         (side-a (truncate (/ max-width 1.619)))
         (side-b (truncate (- max-width side-a)))
         (my-side (if (eq side 'side-a) side-a side-b)))
    ;; (message (format "Gooold: max-width: %d side-a: %d side-b: %d" max-width side-a side-b))
    (cond ((not fill-limit) my-side)
          ((> fill-column my-side) fill-column)
          (t my-side))))

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(map! "C-M-\\" #'transpose-windows)

;; (use-package! moom
;;   :config
;;   (setq moom-use-font-module nil)
;;   (moom-mode 1)
;;   ;; (moom-fill-right)
;;   )

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (error "This works only for two windows!")))

(map! "C-\\" #'toggle-window-split)

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)
