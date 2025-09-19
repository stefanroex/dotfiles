;;;  -*- lexical-binding: t -*-

(use-package terraform-mode
  :hook
  (terraform-mode . lsp-deferred)
  :custom
  (terraform-format-on-save t))

(use-package iflipb
  :custom
  (iflipb-wrap-around t)

  :general
  (:states '(normal emacs motion)
   "SPC z" 'iflipb-previous-buffer
   "SPC x" 'iflipb-next-buffer))

(provide 'stx-module-packages)
