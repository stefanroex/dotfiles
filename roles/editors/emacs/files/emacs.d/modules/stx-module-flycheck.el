(use-package flycheck
  :defer 1
  :config
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc
                  ruby-reek
                  javascript-jshint
                  handlebars
                  scss-lint
                  scss))
  (setq flycheck-idle-change-delay 1
	flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (global-flycheck-mode))

(provide 'stx-module-flycheck)
