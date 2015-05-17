;; Bootstrap use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Bootstrap Cask
(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))

(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)

(mapc 'require '(init-custom
                 init-appearance
                 init-evil

                 init-ido
                 init-neotree
                 init-smex
                 init-projectile
                 init-autocomplete
                 init-recentf

                 init-clojure
                 init-lisp
                 init-js))

;; Requires

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

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
             :init (progn
                     (init custom-settings)))
