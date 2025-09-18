;;;  -*- lexical-binding: t -*-

;; cmd key for meta
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

;; use system clipboard
(setq select-enable-clipboard t)

(use-package browse-url
  :ensure nil ; built-in package
  :commands (browse-url)
  :custom
  (browse-url-generic-program "open")
  (browse-url-browser-function 'browse-url-default-macosx-browser))

(use-package reveal-in-osx-finder
  :general
  (:prefix "SPC"
   :states 'normal
   "o" 'reveal-in-osx-finder))

(provide 'stx-core-macos)
