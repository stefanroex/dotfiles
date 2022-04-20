(use-package flycheck
  :hook (prog-mode . flycheck-mode)
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
        flycheck-check-syntax-automatically '(save mode-enabled idle-change)))

(provide 'stx-module-flycheck)
