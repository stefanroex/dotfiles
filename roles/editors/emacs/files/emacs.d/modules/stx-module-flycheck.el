;;;  -*- lexical-binding: t -*-

(defconst stx/flycheck-disabled-checkers
  '(emacs-lisp-checkdoc
    ruby-reek
    javascript-jshint
    handlebars
    scss-lint
    scss))

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)

  :custom
  (flycheck-disabled-checkers stx/flycheck-disabled-checkers)
  (flycheck-emacs-lisp-load-path 'inherit))

(provide 'stx-module-flycheck)
