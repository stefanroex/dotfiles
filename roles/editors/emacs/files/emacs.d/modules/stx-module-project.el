(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-create-missing-test-files t
        projectile-completion-system 'ivy)

  (keys-l "p" 'projectile-command-map)

  (defun advice-projectile-no-sub-project-files ()
    "Directly call `projectile-get-ext-command'. No need to try to get a
        list of sub-project files if the vcs is git."
    (projectile-files-via-ext-command (projectile-get-ext-command)))

  (advice-add 'projectile-get-repo-files :override #'advice-projectile-no-sub-project-files)
  (projectile-global-mode t))

(use-package counsel-projectile
  :config
  (setq projectile-switch-project-action 'counsel-projectile-find-file))

(provide 'stx-module-project)
