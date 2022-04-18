(use-package exec-path-from-shell
  :demand
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(provide 'stx-core-shell)
