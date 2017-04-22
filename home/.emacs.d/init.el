(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (nlinum alchemist command-log-mode slim-mode markdown-mode reason-mode magithub yaml-mode typescript-mode js2-mode sublime-themes spaceline flycheck elm-mode scss-mode sass-mode web-mode kibit-helper clj-refactor cider clojure-mode-extra-font-locking clojure-mode evil-cleverparens indent-guide aggressive-indent paredit magit evil-search-highlight-persist evil-nerd-commenter evil-surround evil helm-projectile projectile helm which-key ace-jump-mode project-explorer company-flow company ag yasnippet undo-tree general smart-mode-line exec-path-from-shell base16-theme better-defaults use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-search-highlight-persist-highlight-face ((t (:background "#fff" :foreground "#333"))))
 '(isearch ((t (:background "#fff" :foreground "#555"))))
 '(isearch-fail ((t (:background "#fff" :foreground "red"))))
 '(lazy-highlight ((t (:background "#fff" :foreground "#333")))))

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/init"))

(require 'init-use-package)
(require 'init-functions)
(require 'init-editor-packages)
(require 'init-editor-custom)
(require 'init-evil)
(require 'init-magit)
(require 'init-lisp)
(require 'init-clojure)
(require 'init-javascript)
(require 'init-flow)
(require 'init-css)
(require 'init-org)
(require 'init-elm)
(require 'init-flycheck)
(require 'symbol-focus)

(use-package alchemist)

;; Start emacs in dired mode.
(dired ".")
