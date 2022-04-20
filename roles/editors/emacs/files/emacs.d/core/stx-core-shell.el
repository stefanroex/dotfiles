(use-package exec-path-from-shell
  :demand t
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "LSP_USE_PLISTS"))
  :config
  (exec-path-from-shell-initialize))

(provide 'stx-core-shell)
