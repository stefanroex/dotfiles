(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (reveal-in-osx-finder dired+ yaml-mode which-key web-mode use-package smex smart-mode-line slim-mode scss-mode sass-mode project-explorer markdown-mode magit kibit-helper ivy-hydra indent-guide helm-projectile general flycheck flx exec-path-from-shell evil-surround evil-snipe evil-search-highlight-persist evil-nerd-commenter evil-cleverparens elm-mode counsel-projectile company-flow clojure-mode-extra-font-locking clj-refactor challenger-deep-theme better-defaults base16-theme avy alchemist aggressive-indent ag ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-search-highlight-persist-highlight-face ((t (:background "#fff" :foreground "#333"))))
 '(isearch ((t (:background "#fff" :foreground "#555"))))
 '(isearch-fail ((t (:background "#fff" :foreground "red"))))
 '(lazy-highlight ((((class color) (min-colors 257)) (:background "#91ddff" :foreground "#100e23")) (((class color) (min-colors 89)) (:background "#87d7ff" :foreground "#121212")))))

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
