;; Most UI settings are set in early-init.el

;; font settings
(set-face-attribute 'default t :font "Menlo")
(set-face-attribute 'default nil :height 150)
(setq-default line-spacing 4)

;; disable visible-bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Prevent the cursor from blinking
(blink-cursor-mode nil)

;; Theme
(use-package challenger-deep-theme
  :demand
  :init
  (load-theme 'challenger-deep t))

;; Modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(provide 'stx-core-ui)
