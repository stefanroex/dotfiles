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
  :custom
  (rg-show-header nil)
  :custom-face
  (rg-match-face ((t (:inherit highlight))))
  :config
  (keys
    :keymaps 'rg-mode-map
    "C-n" 'next-error
    "C-p" 'previous-error)
  (rg-define-search stx/rg
    :dir project
    :files "everything")
  (rg-define-search stx/rg-literal
    :dir project
    :format literal
    :files "everything"))

(provide 'stx-module-search)
