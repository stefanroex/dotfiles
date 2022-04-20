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
  (rg-file-tag-face ((t (:inherit rg-filename-face))))
  (rg-filename-face ((t (:foreground "systemTealColor"))))
  (rg-line-number-face ((t (:foreground "systemGrayColor"))))
  (rg-match-face ((t (:foreground "systemYellowColor"))))
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
