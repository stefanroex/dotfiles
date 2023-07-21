;; Setup build-in package.el
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(setq package-archive-priorities '(("melpa-stable" . 10)
                                   ("melpa" . 5)
                                   ("org" . 1)
                                   ("elpa" . 0)))
(package-initialize)

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Setup use-package
(require 'use-package)
(setq use-package-verbose nil ; prints message when package is loaded, usefull for performance debugging
      use-package-always-ensure t
      use-package-always-defer t)

;; Setup diminish to be used by other packages
(use-package diminish
  :ensure nil ; build-in
  )

(provide 'stx-core-packages)
