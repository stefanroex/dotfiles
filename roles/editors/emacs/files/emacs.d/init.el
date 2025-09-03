;; Core
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(require 'stx-core-packages)
(require 'stx-core-emacs-settings)
(require 'stx-core-ui)
(require 'stx-core-keybindings)
(require 'stx-core-macos)
(require 'stx-core-shell)

;; Modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'stx-module-autocomplete)
(require 'stx-module-clojure)
(require 'stx-module-completion)
(require 'stx-module-evil)
(require 'stx-module-flycheck)
(require 'stx-module-jump)
(require 'stx-module-lisp)
(require 'stx-module-magit)
(require 'stx-module-profiler)
(require 'stx-module-project)
(require 'stx-module-ruby)
(require 'stx-module-search)
(require 'stx-module-treemacs)
(require 'stx-module-web)
(require 'stx-module-buffer-name)
(require 'stx-module-windows)

(require 'stx-module-packages) ; WIP

;; Make sure all the auto generated custom config is in one place
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil t))
