(provide 'init-projectile)

(defun find-file-projectile-or-dir ()
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (ido-find-file)))

(evil-leader/set-key
  "f" 'find-file-projectile-or-dir
  "p" 'projectile-switch-project)

(defvar projectile-rails-rspec '("Gemfile" "app" "config" "spec"))

(projectile-global-mode)
