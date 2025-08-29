(use-package web-mode
  :mode "\\.js[x]?\\'"
  :custom
  (web-mode-enable-auto-quoting nil)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-indent-style 2)
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (keys-l :keymaps 'web-mode-map "d" 'flow-type)
  ;; (add-to-list 'company-dabbrev-code-modes 'web-mode)
  )

(use-package prettier-js
  :hook
  ((css-mode web-mode) . prettier-js-mode))

(require 'web-mode)

(use-package css-mode)

(use-package sass-mode)

(use-package scss-mode)

(use-package yaml-mode)

(use-package markdown-mode)

(provide 'stx-module-web)
