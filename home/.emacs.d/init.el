;; Setup package.el
(require 'package)
(package-initialize)

;; Make sure all the auto generated custom config is in one place
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Load configuration
(add-to-list 'load-path (expand-file-name (expand-file-name "init" user-emacs-directory)))

(require 'init-use-package)
; (require 'init-editor-custom)
; (require 'init-functions)
; (require 'init-editor-packages)
; (require 'init-evil)
; (require 'init-clojure)
; (require 'init-lisp)
