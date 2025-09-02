(setq tab-always-indent 'complete
      completion-cycle-threshold nil
      completion-ignore-case t)

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-commit-on-ret t)

  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode))

(use-package kind-icon
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(provide 'stx-module-autocomplete)
