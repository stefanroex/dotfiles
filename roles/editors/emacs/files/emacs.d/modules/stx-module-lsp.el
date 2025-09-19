;;;  -*- lexical-binding: t -*-

(use-package lsp-mode
  :custom-face
  (lsp-face-highlight-textual ((t (:inherit region))))

  :general-config
  (:states '(normal emacs motion)
   "SPC lR" 'lsp-rename
   "SPC la" 'lsp-execute-code-action
   "SPC lr" 'lsp-find-references)

  :custom
  (lsp-enable-folding nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-text-document-color nil)
  (lsp-keymap-prefix nil)
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-indentation nil)
  (lsp-completion-enable nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-idle-delay 0.05)
  (lsp-completion-provider :capf)
  (lsp-diagnostics-provider :none))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable nil)
  (lsp-ui-sideline-enable nil))

(provide 'stx-module-lsp)
