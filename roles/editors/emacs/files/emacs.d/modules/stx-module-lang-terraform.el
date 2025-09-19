;;;  -*- lexical-binding: t -*-

(use-package terraform-mode
  :hook
  (terraform-mode . lsp-deferred)
  :custom
  (terraform-format-on-save t))

(provide 'stx-module-lang-terraform)
