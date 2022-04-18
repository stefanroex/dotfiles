(use-package yaml-mode)

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode)))

(use-package prettier-js
  :config
  (add-hook 'markdown-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

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

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2))

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(provide 'stx-module-web)
