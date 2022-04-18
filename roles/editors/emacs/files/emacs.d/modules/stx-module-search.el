(use-package ag
  :general
  (keys-l
    "s" 'ag-project-regexp
    "S" 'ag-project)
  :config
  (setq ag-reuse-buffers t)
  (define-key ag-mode-map (kbd "k") nil))

(provide 'stx-module-search)
