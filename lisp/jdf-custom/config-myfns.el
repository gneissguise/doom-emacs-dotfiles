;;; $DOOMDIR/lisp/jdf-custom/config-myfns.el -*- lexical-binding: t; -*-

(defun jdf-cider-load-repl-window (buffer &optional alist)
  (split-window-right -95)
  ;;another way to get the cider buffer as a standalone
  ;;(cider-current-repl nil 'ensure)
  (set-window-buffer (next-window) buffer))

(use-package! ts :defer t)
(defun ndk/ts-at-point ()
  (unless (looking-at "[0-9.]")
    nil)
  (let ((origin (point))
        beg end)
    (save-excursion
      (setq beg (+ origin (skip-chars-backward "0123456789."))))
    (save-excursion
      (setq end (+ origin (skip-chars-forward "0123456789."))))
    (list beg end)))

(defun ndk/show-date-at-point ()
  (interactive)
  (let* ((r (ndk/ts-at-point))
         (beg (nth 0 r))
         (end (nth 1 r))
         (unix_ts (buffer-substring beg end)))
    (message (ts-format (make-ts :unix (float (read unix_ts)))))))

;; Portal config
;; Leverage an existing cider nrepl connection to evaluate portal.api functions
;; and map them to convenient key bindings.

(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(require 'portal.api) (portal.api/tap) (portal.api/open {:portal.colors/theme :portal.colors/material-ui})"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

(defun postmortem-requires ()
  "Using postmortem for debugging, requires postmortem in deps.edn
    The main functions are:
    (pm/spy> expression :key)
        The spy> function stores the value of the expression as a log entry under the key. This is the thread-first  version so takes the data first.

    (pm/spy>> :key (expression))
        The spy>> stores the value of the expression as a log entry under the key. This is the thread-last version so takes the data last.

    (pm/dump :key)
        Dump stores a local environment map to the log entry of :key. Very useful for quickly seeing how a function works.

    (log-for :key)
        Returns all the logged data for the key.

    (pm/reset-key! :key)
        We can reset a key by calling it.

    (pi/instrument 'sym)
        Allows you to collect execution log for a function without touching it's code

    (pi/unstrument 'sym)
        De instruments function to disable logging"
  (interactive)
  (cider-nrepl-sync-request:eval "(require '[postmortem.core :as pm])")
  (cider-nrepl-sync-request:eval "(require '[postmortem.instrument :as pi])"))

(defun malli-requires ()
  (interactive)
  (cider-nrepl-sync-request:eval "(require '[malli.core :as m])")
  (cider-nrepl-sync-request:eval "(require '[malli.instrument :as mi])")
  (cider-nrepl-sync-request:eval "(require '[malli.provider :as mp])"))

(defun flowstorm-requires ()
  (interactive)
  (cider-nrepl-sync-request:eval "(require '[flow-storm.api :as fs-api])"))

(defun flowstorm-connect ()
  (interactive)
  (cider-nrepl-sync-request:eval "(fs-api/local-connect)"))

(defun jdf-cider-let-binding-to-def ()
  "Intern let binding pairs in last-sexp to current repl ns"
  (interactive)
  (cider-ensure-connected)
  (let* ((stripped-str (s-trim (replace-regexp-in-string "\\W+" " " (cider-last-sexp))))
         (quoted-str (replace-regexp-in-string "\\(\\w\\)\\s-\\(\\s.?\\w\\s.?\\)"  "['\\1 \\2]" stripped-str)))
    (cider-insert-in-repl
     (format "(->> [%s] (map (partial apply intern *ns*)))" quoted-str) nil)))

(defun jdf-cider-drop-in-def (start end)
  "Wrap selected text into a clojure def form and drop into cider-repl"
  (interactive "r")
  (cider-ensure-connected)
  (cider-insert-in-repl (format "(def %s)"
                                (buffer-substring-no-properties start end)) nil))

(defun jdf-cider-count-last-sexpr ()
  (interactive)
  (cider-ensure-connected)
  (let* ((stripped-str (s-trim (replace-regexp-in-string "\\W+" " " (cider-last-sexp))))
         (quoted-str (replace-regexp-in-string "\\(\\w?\\)"  "'\\1" stripped-str)))
    (cider-insert-in-repl ;; cider-interactive-eval
     (format "(count [%s])" stripped-str) nil)))

(defun jdf-cider-count-last-sexpr ()
  (interactive)
  (cider-ensure-connected)
  (let* ((lparen-str (replace-regexp-in-string "^\\s("  "[" (cider-last-sexp)))
         (rparen-str (replace-regexp-in-string "\\s)$"  "]" lparen-str))
         (final-str (s-trim (replace-regexp-in-string "\\([a-zA-Z0-9-_:?*/]+\\)" "'\\1" rparen-str))))
    (cider-interactive-eval (format "(count %s)" final-str))))

(defun jdf-ts-epoch-secs-to-local (start end)
  "Convert selected unix time to local date-time"
  (interactive "r")
  (message "fun not implemented"))

(defun jdf-ts-epoch-millis-to-local (start end)
  "Convert selected unix time (in milliseconds) to local date-time"
  (interactive "r")
  (message "fun not implemented"))

(defun jdf-bb-eval-syntax (syn)
  "eval clojure using babashka!"
  (interactive "sSyntax for babashka to eval: ")
  (shell-command (format "bb -e  \'%s\'" syn)))

(defun jdf-bb-eval-syntax-and-insert (syn)
  "eval clojure using babashka!"
  (interactive "sSyntax for babashka to eval: ")
  (insert
   (shell-command-to-string (format "bb -e  \'%s\'" syn))))

(defun jdf-bb-replace-dt-MM ()
  "Convert word at point (or selected region) to thing."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (select   (buffer-substring-no-properties
                    (car bounds)
                    (cdr bounds)))
         (text (replace-regexp-in-string "\\(\"\\|\n\\)" "" select))
         (cmd (format "bb -e  \'(-> (java.text.SimpleDateFormat. \"MM/dd/yyyy\")(.parse \"%s\")(.getTime))\'"
                      text))
         (result (shell-command-to-string cmd)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (replace-regexp-in-string "\n" "" result))
      (message (format "%s replaced with %s" text result)))))


(defun jdf-mark-insert-symbol ()
  "Mark a symbol and replace it with something from the kill-ring"
  (interactive)
  (lispy-mark-symbol)
  (lispy-delete -1)
  (consult-yank-pop))

;; Example key mappings for doom emacs
(map! (:map (clojure-mode-map
             clojurescript-mode-map
             clojurec-mode-map
             cider-mode-map
             cider-repl-mode-map)
       "s-d o" #'portal.api/open
       "s-d c" #'portal.api/close
       "s-d d" #'portal.api/clear
       "s-d p" #'postmortem-requires
       "s-d m" #'malli-requires
       "s-d f" #'flowstorm-requires
       "s-d F" #'flowstorm-connect
       "C-c <backtab>" #'jdf-cider-let-binding-to-def
       "C-c TAB" #'jdf-cider-drop-in-def))

;; (define-key 'cider-insert-commands-map (kbd "c") #'jdf-cider-count-last-sexpr)
;; (define-key 'cider-insert-commands-map (kbd "C-c") #'jdf-cider-count-last-sexpr)

(map! "C-x M-t l" #'jdf-ts-epoch-secs-to-local
      "C-x M-t L" #'jdf-ts-epoch-millis-to-local
      "C-x M-t d" #'ndk/show-date-at-point
      "C-x M-t M" #'jdf-bb-replace-dt-MM
      "C-c b" #'jdf-bb-eval-syntax
      "C-c B" #'jdf-bb-eval-syntax-and-insert
      "M-n" #'jdf-mark-insert-symbol)

(defun jdf-mark-quote-word ()
  "Mark a word and surround it in quotes"
  (interactive)
  (mark-word)
  (insert-pair))

(map! "s-\"" #'jdf-mark-quote-word)

(defun jdf-shell-command-on-buffer (cmd &optional buff)
  "Run cli command 'cmd"
  (interactive)
  (shell-command-on-region (point-min) (point-max) cmd (or buff (current-buffer)) t)
  ;; (format "%s %s" cmd (buffer-substring-no-properties (point-min) (point-max)))
  )

(defun jdf-format-xml ()
  "Format using xmllint"
  (interactive)
  (jdf-shell-command-on-buffer "xmllint --noblanks --format -"))

(defun jdf-cli-stdin-pipe (to-cmd)
  (format "bash -c 'echo $0' | %s" to-cmd))

(defun jdf-jq-format ()
  "Format using jq"
  (interactive)
  (jdf-shell-command-on-buffer "bash -c echo"))


(defun jdf-cljfmt (&optional option)
  "Clojure formatting using cljfmt.
    option: 'check or 'fix"
  (interactive)
  (let ((filename (buffer-file-name))
        (cmd "clojure -Sdeps '{:deps {cljfmt {:mvn/version \"0.9.0\"}}}' -M -m cljfmt.main %s %s")
        (info-buffer "*cljfmt-info*")
        (arg (cond
              ((eq option 'check) "check")
              ((eq option 'fix) "fix")
              (t "check"))))
    (message (format "Running cljfmt %s on %s" arg (expand-file-name filename)))
    (let ((formatted-cmd (format cmd arg filename))
          (ansi-color-apply-face-function
           (lambda (beg end face)
             (when face
               (put-text-property beg end 'face face)))))
      (with-output-to-temp-buffer info-buffer
        (shell-command formatted-cmd info-buffer)
        (pop-to-buffer info-buffer))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(defun jdf-cljfmt-check ()
  "Check clj file formatting using cljfmt"
  (interactive)
  (jdf-cljfmt 'check))

(defun jdf-cljfmt-fix ()
  "Fix clj file formatting using cljfmt"
  (interactive)
  (jdf-cljfmt 'fix))

(set-popup-rule! "\\*cljfmt-info\\*" :vslot -13 :slot 2 :side 'right :size 0.33 :select 'ignore :quit t)

(map! :map nxml-mode-mapa
      "C-c SPC" #'jdf-format-xml)

(defun jet-pretty ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "jet --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))

(defun figlet-region (&optional b e)
  (interactive "r")
  (call-process-region b e "figlet" t t)
  (comment-region (mark) (point)))

(defun shell-command-on-buffer ()
  "Asks for a command and executes it in inferior shell with current buffer
as input."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (read-shell-command "Shell command on buffer: ")))

(defadvice erase-buffer (around erase-buffer-noop)
  "make erase-buffer do nothing")

(defadvice shell-command (around shell-command-unique-buffer activate compile)
  (if (or current-prefix-arg
          (not (string-match "[ \t]*&[ \t]*\\'" command)) ;; background
          (bufferp output-buffer)
          (stringp output-buffer))
      ad-do-it ;; no behavior change

    ;; else we need to set up buffer
    (let* ((command-buffer-name
            (format "*background: %s*"
                    (substring command 0 (match-beginning 0))))
           (command-buffer (get-buffer command-buffer-name)))

      (when command-buffer
        ;; if the buffer exists, reuse it, or rename it if it's still in use
        (cond ((get-buffer-process command-buffer)
               (set-buffer command-buffer)
               (rename-uniquely))
              ('t
               (kill-buffer command-buffer))))
      (setq output-buffer command-buffer-name)

      ;; insert command at top of buffer
      (switch-to-buffer-other-window output-buffer)
      (insert "Running command: " command
              "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n")

      ;; temporarily blow away erase-buffer while doing it, to avoid
      ;; erasing the above
      (ad-activate-regexp "erase-buffer-noop")
      ad-do-it
      (ad-deactivate-regexp "erase-buffer-noop"))))

;; (set-eval-handler! 'crystal-mode
;;   '((:command     . "crystal")
;;     (:exec        . "%c %s")
;;     (:description . "Run Crystal script")))
