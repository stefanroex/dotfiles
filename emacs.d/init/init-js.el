(provide 'init-js)

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.js.erb$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json.erb$" . javscript-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.js.jsx$" . jsx-mode))

(setq js-indent-level 2)
(add-hook 'js-mode-hook 'subword-mode)

;; Coffeescript
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'ac-modes 'coffee-mode)

(eval-after-load "coffee-mode"
  '(setq coffee-js-mode 'js2-mode
         coffee-tab-width 2))

(add-hook 'coffee-mode-hook 'subword-mode)
(add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'coffee-mode-hook
          (function (lambda ()
                      (setq evil-shift-width js-indent-level))))

;; JSX
(add-to-list 'auto-mode-alist '("\\.module.js.jsx$" . jsx-mode))
(add-to-list 'ac-modes 'jsx-mode)

(add-hook 'jsx-mode-hook 'subword-mode)
(add-hook 'jsx-mode-hook
          (function (lambda ()
                      (setq evil-shift-width js-indent-level))))

(custom-set-variables
 '(jsx-indent-level 2))
