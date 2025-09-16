;;;  -*- lexical-binding: t -*-

(setq tab-always-indent 'complete
      completion-cycle-threshold nil
      completion-ignore-case t)

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-commit-on-ret t)
  (corfu-preview-current 'insert)

  :general
  (keys
    :states '(nil insert)
    :keymaps 'corfu-map
    [tab] 'corfu-next
    [backtab] 'corfu-previous)

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; (use-package corfu-candidate-overlay
;;   :after corfu
;;   :general
;;   (keys
;;     :states 'insert
;;     [backtab] 'corfu-candidate-overlay-complete-at-point)
;;   :init
;;   (corfu-candidate-overlay-mode +1)
;;   (setq corfu-popupinfo-delay 0.5))

(use-package kind-icon
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(provide 'stx-module-autocomplete)
