;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package flycheck
  :defer 1
  :config
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc
                  javascript-jshint
                  handlebars
                  scss-lint
                  scss))
  ;; (add-to-list 'flycheck-disabled-checkers 'scss)

  (setq flycheck-idle-change-delay 1
        flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (global-flycheck-mode))

(use-package flycheck-flow
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-flow-coverage 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-flow 'web-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-flow-coverage)))

(provide 'init-flycheck)
;;; init-flycheck ends here
