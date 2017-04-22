(use-package web-mode
  :ensure t
  :mode (("\\.js$" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2)

  (keys-l :keymaps 'web-mode-map
          "d" 'flow-type)

  (add-hook 'web-mode-hook
            (lambda ()
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx")
                (message "now set to: %s" web-mode-content-type)))))

(defun import-js/word-at-point ()
  (save-excursion
    (skip-chars-backward "A-Za-z0-9:_")
    (let ((beg (point)) module)
      (skip-chars-forward "A-Za-z0-9:_")
      (setq module (buffer-substring beg (point)))
      module)))

(defun import-js/word ()
  (interactive)
  (let ((default-directory (projectile-project-root))
        (command (concat "importjs word --overwrite "
                         (import-js-word-at-point)
                         " "
                         (file-relative-name (buffer-file-name) default-directory))))
    (shell-command command)
    (revert-buffer t t t)))

(provide 'init-javascript)
