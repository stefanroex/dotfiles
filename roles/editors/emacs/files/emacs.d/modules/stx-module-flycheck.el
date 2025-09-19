;;;  -*- lexical-binding: t -*-

(defconst stx/flycheck-disabled-checkers
  '(emacs-lisp-checkdoc
    handlebars
    javascript-jshint
    ruby-reek
    scss
    scss-lint
    terraform-tflint))

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)

  :custom
  (flycheck-disabled-checkers stx/flycheck-disabled-checkers)
  (flycheck-standard-error-navigation nil)
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package consult-flycheck
  :general
  (:states '(normal emacs motion)
   "SPC he" 'consult-flycheck))

(provide 'stx-module-flycheck)
