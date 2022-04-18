;; cmd key for meta
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

;; use system clipboard
(setq select-enable-clipboard t)

(use-package browse-url
  :ensure nil ; built-in package
  :config
  (setq browse-url-generic-program "open")
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))

(use-package reveal-in-osx-finder
  :config
  (keys-l "o" 'reveal-in-osx-finder))

(provide 'stx-core-macos)
