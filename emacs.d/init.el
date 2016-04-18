(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))

(setq package-enable-at-startup nil)
(package-initialize)

(require 'init-vars)
(require 'init-use-package)
(require 'init-functions)
(require 'init-packages)
(require 'init-custom)
