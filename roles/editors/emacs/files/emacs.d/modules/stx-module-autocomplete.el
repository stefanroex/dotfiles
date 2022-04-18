(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :config
  (keys :states 'insert
    "<tab>" 'company-complete-common-or-cycle)
  (general-def 'company-active-map
    "C-s" 'company-filter-candidates
    "C-d" 'company-show-doc-buffer
    "<tab>" 'company-complete-common-or-cycle
    "S-<tab>" 'company-select-previous-or-abort))

(use-package yasnippet
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (yas-reload-all))

(provide 'stx-module-autocomplete)
