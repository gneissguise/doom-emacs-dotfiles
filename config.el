;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; HACK:
;; (setq-hook! 'emacs-everywhere-init-hooks doom-inhibit-local-var-hooks t)

(setq user-full-name "Justin Frost"
      user-mail-address "justin.frost@gateless.com")

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-debug t
      auth-source-do-cache t
      auth-source-cache-expiry nil
      password-cache t
      password-cache-expiry nil
      epg-gpg-program "gpg2")
(setq-default epa-file-encrypt-to '("D0F1FB504EC6A13F"))
(setenv "GPG_AGENT_INFO" nil)       ; use emacs key helper


(setq initial-frame-alist
      '((top . 50)
        (left . 75)
        (width . 180)
        (height . 90)
        ;; (fullscreen . fullheight)
        ))

(setq-default delete-by-moving-to-trash t ; Delete files to trash
              evil-shift-width 2
              history-length 20000
              tab-width 2                 ; Set width for tabs
              window-combination-resize t ; take new window space from all other windows (not just current)
              x-stretch-cursor t
              fill-column 96)

(setq undo-limit 80000000
      kill-whole-line t
      display-line-numbers-type t
      so-long-threshold 100000
      inhibit-splash-screen t
      initial-scratch-message ";; - 'Tis but a scratch!\n;; - A scratch? Your arm's off!\n;; - No, it isn't!\n\n"
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      inhibit-compacting-font-caches t
      confirm-kill-emacs nil
      confirm-kill-processes nil
      delete-exited-processes t
      scroll-margin 2
      sentence-end-double-space nil
      comp-async-report-warnings-errors nil
      flycheck-checker-error-threshold 4000
      read-process-output-max 4194304
      yas-triggers-in-field t
      evil-default-state 'emacs
      custom-safe-themes t
      evil-want-fine-undo t ;; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t   ;; Nobody likes to lose work, I certainly don't
      scroll-preserve-screen-position 'always ;; Don't have `point' jump around
      frame-resize-pixelwise t
      find-file-visit-truename t
      all-the-icons-scale-factor 1.1)

(delete-selection-mode 1)
(global-visual-line-mode t)
(global-whitespace-mode 0)

;; all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(cond (IS-MAC
       (setq mac-pass-control-to-system nil
             mac-pass-command-to-system nil
             mac-mouse-wheel-smooth-scroll t
             mac-command-modifier 'super
             mac-option-modifier 'meta
             mac-control-modifier 'control
             ns-use-proxy-icon  nil
             ns-use-thin-smoothing t
             ns-confirm-quit nil
             dired-listing-switches "--long --group-directories-first --classify --git --no-user --no-permissions --octal-permissions"
             ;; insert-directory-program "/usr/local/bin/gls"
             insert-directory-program "/usr/local/bin/exa")
       (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
       (add-to-list 'default-frame-alist '(ns-appearance . dark))
       (global-set-key (kbd "s-s") 'save-buffer)
       (global-set-key (kbd "s-x") 'kill-region)))

;; Load my custom configurations (bisected from this file)
(load! "lisp/jdf-custom/config.el")
