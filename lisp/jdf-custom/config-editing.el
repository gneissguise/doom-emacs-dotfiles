;;; $DOOMDIR/lisp/jdf-custom/config-editing.el -*- lexical-binding: t; -*-

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(use-package! goto-last-point
  :defer t
  :bind ("C-<" . goto-last-point)
  :config
  (goto-last-point-mode))

(use-package! goto-chg
  :defer t
  :bind ("C->" . goto-last-change))

(defun narrow-to-regexp ()
  "Narrow the buffer visibility to the regexp the user provides."
  (interactive)
  (let* ((regexp (read-regexp "Regexp to narrow down"))
         (beg)
         (end))
    (goto-char (point-min)) ;; go to the start of the buffer
    ;; The \\(?:...\\) grouping construct ensures that a leading ^, +, * or ?
    ;; or a trailing $ in REGEXP will be interpreted correctly.
    (setq regexp (concat "^.*\\(?:" regexp "\\).*\\(?:$\\)\n?"))
    (if (re-search-forward regexp nil t nil)
        (setq beg (- (point) (length regexp)))
      (setq end (point)))
    (if (and beg end (> end beg))
        (narrow-to-region beg end)
      (message "Did not find both instances of the regex, %s %s, no narrow" beg end))))
