;;;  -*- lexical-binding: t -*-

(use-package css-mode)

(use-package sass-mode)

(use-package scss-mode)

(use-package yaml-mode)

(use-package markdown-mode)

(use-package web-mode
  :custom
  (web-mode-attr-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (web-mode-indent-style 2)
  (web-mode-markup-indent-offset 2))

(use-package prettier-js
  :hook
  ((css-mode web-mode) . prettier-js-mode))

(provide 'stx-module-lang-web)
