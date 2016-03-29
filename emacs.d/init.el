(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))

(setq package-pinned-packages
      '((cider . "melpa-stable")
        (clj-refactor . "melpa-stable")
        (clojure-mode . "melpa-stable")))

(setq package-enable-at-startup nil)
(package-initialize)

(require 'init-use-package)
(require 'init-packages)
(require 'init-custom)
