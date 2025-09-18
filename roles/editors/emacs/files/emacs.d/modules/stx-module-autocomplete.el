;;;  -*- lexical-binding: t -*-

(setq tab-always-indent 'complete
      completion-cycle-threshold nil
      completion-ignore-case t)

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  :custom
  (corfu-cycle t)
  (corfu-commit-on-ret t)
  (corfu-preview-current 'insert)

  :general-config
  (:states '(nil insert)
   :keymaps 'corfu-map
   [tab] 'corfu-next
   [backtab] 'corfu-previous))

(use-package kind-icon
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(provide 'stx-module-autocomplete)
