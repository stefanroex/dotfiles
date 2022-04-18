(use-package dashboard
  :demand t
  :custom
  (dashboard-set-footer nil)
  (dashboard-banner-logo-title nil)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-set-file-icons t)
  (dashboard-items '((projects . 10)
		     (recents . 10)))
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))

(provide 'stx-module-dashboard)
