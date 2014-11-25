(provide 'init-autocomplete)

;; Autocomplete
(global-auto-complete-mode t)
(ac-config-default)
(setq ac-sources '(ac-source-semantic ac-source-yasnippet ac-source-abbrev))
(setq ac-disable-faces nil)
