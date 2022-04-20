;; (use-package ag
;;   :config
;;   (require 'exec-path-from-shell)
;;   (setq ag-reuse-buffers t)
;;   (define-key ag-mode-map (kbd "k") nil))

(use-package rg
  :general
  (keys-l
    "s" 'stx/rg
    "S" 'stx/rg-literal)
  :config
  (rg-define-search stx/rg
    :dir project
    :files "everything")
  (rg-define-search stx/rg-literal
    :dir project
    :format literal
    :files "everything"))

(provide 'stx-module-search)
