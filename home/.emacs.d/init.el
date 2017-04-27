(require 'package)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
