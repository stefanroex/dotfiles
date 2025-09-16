(require 'package)
(require 'use-package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))
      package-native-compile t
      package-archive-priorities '(("melpa" . 10)
                                   ("melpa-stable" . 5)
                                   ("org" . 1)
                                   ("elpa" . 0))
      use-package-verbose nil ; prints message when package is loaded, usefull for performance debugging
      use-package-always-ensure t
      use-package-always-defer t)

;; Setup diminish to be used by other packages
(use-package diminish
  :ensure nil ; build-in
  )

(use-package compile-angel
  :demand t
  :config
  (setq compile-angel-verbose t)
  (add-to-list 'compile-angel-excluded-files "/init.el")
  (add-to-list 'compile-angel-excluded-files "/early-init.el")
  (add-to-list 'compile-angel-excluded-files "/custom.el")
  (add-to-list 'compile-angel-excluded-files-regexps "/core/.*.el")
  (add-to-list 'compile-angel-excluded-files-regexps "/modules/.*.el")
  (compile-angel-on-load-mode 1))

(provide 'stx-core-packages)
