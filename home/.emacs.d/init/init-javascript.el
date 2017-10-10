(use-package web-mode
  :mode (("\\.js$" . web-mode))
  :config
  (setq web-mode-enable-auto-quoting nil
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-indent-style 2)

  (keys-l :keymaps 'web-mode-map
          "d" 'flow-type)

  (add-to-list 'company-dabbrev-code-modes 'web-mode)

  (add-hook 'web-mode-hook
            (lambda ()
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx")
                (message "now set to: %s" web-mode-content-type)))))

(use-package prettier-js
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           "--single-quote" "false"))
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package import-js
  :commands import-js-fix
  :init
  (defun fix-js-file ()
    (interactive)
    (import-js-fix)
    (prettier-js))
  (setq import-js-project-root "~/Code/cep")
  (keys-l "i" 'fix-js-file))

(use-package add-node-modules-path
  :defer t
  :init
  (add-hook 'web-mode-hook #'add-node-modules-path))

(provide 'init-javascript)
