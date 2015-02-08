(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))
(mapc 'require '(init-bootstrap
                 init-custom
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
                 init-js
                 ))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/evil-rebellion"))
(require 'evil-rebellion)
