;;; +init.el -*- lexical-binding: t; -*-

;; Fix for #2386 until further investigation
;;(when noninteractive
;;  (after! undo-tree
;;    (global-undo-tree-mode -1)))

;; Use Plists with LSP
(setenv "LSP_USE_PLISTS" "1")

(setq native-comp-deferred-compilation t
      native-comp-async-query-on-exit t
      native-comp-async-jobs-number 8
      native-comp-async-report-warnings-errors nil)
(setq package-native-compile t)
(setq comp-deferred-compilation t)

    ;; block until native compilation has finished
    (while (or comp-files-queue
               (> (comp-async-runnings) 0))
      (sleep-for 1))
