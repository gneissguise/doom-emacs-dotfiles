;;; $DOOMDIR/lisp/jdf-custom/config.el -*- lexical-binding: t; -*-

(load! "config-coding.el")
(load! "config-company.el")
(load! "config-completion.el")
(load! "config-dired.el")
(load! "config-editing.el")
(load! "config-myfns.el")
(load! "config-org.el")
(load! "config-template")
(load! "config-ui.el")

;; (add-hook 'sql-interactive-mode-hook
;;           (lambda ()
;;             (setq sql-prompt-regexp "^[_[:alpha:]]*[=][#>] ")
;;             (setq sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")
;;             (toggle-truncate-lines t)))

;; (setq sql-connection-alist
;;       '((dev-cond-svc (sql-product 'postgres)
;;                       (sql-port 5432)
;;                       (sql-server "dev-platform-aurora.cluster-cnx83vlpr5p0.us-east-1.rds.amazonaws.com")
;;                       (sql-user "condition_service")
;;                       (sql-database "condition_service"))
;;         (stage-cond-svc (sql-product 'postgres)
;;                         (sql-port 5432)
;;                         (sql-server "stage-platform-aurora.cluster-cnx83vlpr5p0.us-east-1.rds.amazonaws.com")
;;                         (sql-user "condition_service")
;;                         (sql-database "condition_service"))
;;         (prod-cond-svc (sql-product 'postgres)
;;                        (sql-port 5432)
;;                        (sql-server "prod-platform-aurora.cluster-cnx83vlpr5p0.us-east-1.rds.amazonaws.com")
;;                        (sql-user "condition_service")
;;                        (sql-database "condition_service"))))

;; (defun +g8-sql-connect (conx)
;;   (load! "config-g8secret.el.gpg")

;;   ;; update the password to the sql-connection-alist
;;   (let ((connection-info (assoc conx sql-connection-alist))
;;         (sql-password (car (last (assoc conx +g8-sql-password)))))
;;     (delete sql-password connection-info)
;;     (nconc connection-info `((sql-password ,sql-password)))
;;     (setq sql-connection-alist (assq-delete-all conx sql-connection-alist))
;;     (add-to-list 'sql-connection-alist connection-info))

;;   ;; connect to db
;;   (setq sql-product 'postgres)
;;   (sql-connect conx))

;; (defun +g8-dev-cond-svc ()
;;   (interactive)
;;   (+g8-sql-connect 'dev-cond-svc))

;; (defun +g8-stage-cond-svc ()
;;   (interactive)
;;   (+g8-sql-connect 'stage-cond-svc))

;; (defun +g8-prod-cond-svc ()
;;   (interactive)
;;   (+g8-sql-connect 'prod-cond-svc))
