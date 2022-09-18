;;; $DOOMDIR/lisp/jdf-custom/config-org.el -*- lexical-binding: t; -*-

;; Justin's org config
(setq
 org-directory "~/Dropbox/Org"
 org-agenda-files (mapcar 'file-truename
                          (file-expand-wildcards "~/Dropbox/org/*.org"))
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'smart
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Log
 org-log-done 'time
 org-log-into-drawer t
 org-log-redeadline 'time
 org-log-reschedule 'time
 org-refile-use-outline-path 'file
 org-use-property-inheritance t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis " [+]"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────"
 org-startup-folded 'overview
 mixed-pitch-variable-pitch-cursor nil
 org-agenda-hide-tags-regexp "."
 org-agenda-prefix-format
 '((agenda . " %i %-12:c%?-12t% s")
   (todo   . " ")
   (tags   . " %i %-12:c")
   (search . " %i %-12:c"))

 ;; Capture templates
 org-capture-templates `(("i" "Inbox" entry  (file "inbox.org")
                          ,(concat "* TODO %?\n"
                                   "/Entered on/ %U"))
                         ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
                          ,(concat "* %? :meeting:\n"
                                   "<%<%Y-%m-%d %a %H:00>>"))
                         ("n" "Note" entry  (file "notes.org")
                          ,(concat "* Note (%a)\n"
                                   "/Entered on/ %U\n" "\n" "%?"))
                         ("@" "Inbox [mu4e]" entry (file "inbox.org")
                          ,(concat "* TODO Reply to \"%a\" %?\n"
                                   "/Entered on/ %U")))
 org-refile-targets
 '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)"))
 org-refile-use-outline-path 'file
 org-outline-path-complete-in-steps nil
 org-todo-keywords
 '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)"))
 org-agenda-custom-commands
 '(("g" "Get Things Done (GTD)"
    ((agenda ""
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-deadline-warning-days 0)))
     (todo "NEXT"
           ((org-agenda-skip-function
             '(org-agenda-skip-entry-if 'deadline))
            (org-agenda-prefix-format "  %i %-12:c [%e] ")
            (org-agenda-overriding-header "\nTasks\n")))
     (agenda nil
             ((org-agenda-entry-types '(:deadline))
              (org-agenda-format-date "")
              (org-deadline-warning-days 7)
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
              (org-agenda-overriding-header "\nDeadlines")))
     (tags-todo "inbox"
                ((org-agenda-prefix-format "  %?-12t% s")
                 (org-agenda-overriding-header "\nInbox\n")))
     (tags "CLOSED>=\"<today>\""
           ((org-agenda-overriding-header "\nCompleted today\n")))))))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

(add-hook! 'org-mode-hook #'mixed-pitch-mode #'org-modern-mode #'visual-line-mode)
(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defun org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(defun org-capture-mail ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "@"))

;; Save the corresponding buffers
(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
                         (when (member (buffer-file-name) org-agenda-files)
                           t)))
  (message "Saving org-agenda-files buffers... done"))

;; Add it after refile
(advice-add 'org-refile :after
            (lambda (&rest _)
              (gtd-save-org-buffers)))

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; (define-key global-map            (kbd "C-c c") 'org-capture)
;; (define-key global-map            (kbd "C-c i") 'org-capture-inbox)

;; Only if you use mu4e
;; (require 'mu4e)
;; (define-key mu4e-headers-mode-map (kbd "C-c i") 'org-capture-mail)
;; (define-key mu4e-view-mode-map    (kbd "C-c i") 'org-capture-mail)

;; work with org-agenda dispatcher [c] "Today Clocked Tasks" to view today's clocked tasks.
(defun org-agenda-log-mode-colorize-block ()
  "Set different line spacing based on clock time duration."
  (save-excursion
    (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
                     ('light
                      (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
                     ('dark
                      (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue"))))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          ;; larger duration bar height
          (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors) :foreground "black"))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'org-agenda-log-mode-colorize-block)

(use-package! mixed-pitch
  :defer t
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

(setq time-stamp-active t
      time-stamp-start "#\\+lastmod:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "%04Y-%02m-%02d")

(add-hook 'before-save-hook 'time-stamp nil)

;;(use-package! ob-clojure-literate
;;  :config
;;  (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((clojure . t)
;;     (sh . t)
;;     (dot . t)
;;     (emacs-lisp . t)))
;; (setq org-babel-clojure-backend 'cider)
;;  (setq ob-clojure-literate-auto-jackin-p t)
;; (add-hook 'org-mode-hook #'ob-clojure-literate-mode)
;;  (setq org-edit-src-content-indentation 0
;;        org-src-tab-acts-natively t
;;        org-src-fontify-natively t
;;        org-confirm-babel-evaluate nil
;;        org-support-shift-select 'always
;;        org-hide-emphasis-markers t
;;        org-babel-clojure-nrepl-timeout nil)
;;  (setq org-confirm-babel-evaluate nil)
;;  (setq org-export-use-babel t)
;;  (org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
;;  (org-defkey org-mode-map "\C-c\C-d" 'cider-doc))
