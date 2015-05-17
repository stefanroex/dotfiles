;; Bootstrap use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; Bootstrap Cask
(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))

(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)

(mapc 'require '(init-evil
                 init-projectile
                 init-autocomplete
                 init-clojure
                 init-lisp
                 init-js))

;; Requires
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

;; Theme
(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t))

;; Font
(set-default-font "Inconsolata 18")
(setq-default line-spacing 4)

;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (exec-path-from-shell-initialize)))

;; UI
(use-package init-custom
  :load-path "init/"
  :config
  (progn
    (init-custom-settings)))

;; Fancy battery info for mode line
(use-package fancy-battery
  :ensure t
  :defer t
  :config (setq fancy-battery-show-percentage t)
  :init (fancy-battery-mode))

;; Ido settings
(use-package flx-ido
  :ensure t
  :defer t
  :init (flx-ido-mode 1)
  :config
  (setq ido-use-faces nil
        flx-ido-threshhold 1000
        ido-enable-flex-matching t
        ido-everywhere 1
        ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")))

;; Smex support
(use-package smex
  :ensure t
  :defer t
  :init (smex-initialize)
  :bind ("M-x" . smex))
