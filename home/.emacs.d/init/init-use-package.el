(require 'package)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)

(setq use-package-verbose nil
      use-package-always-ensure t)

(provide 'init-use-package)
