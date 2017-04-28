;; Setup package.el
(require 'package)
(package-initialize)

;; Make sure all the auto generated custom config is in one place
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Load configuration
(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))
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
(require 'init-elm)
(require 'init-flycheck)
(require 'symbol-focus)

(projectile-switch-project)
