;; Most UI settings are set in early-init.el

;; font settings
(set-face-attribute 'default nil
                    :font "iMWritingMono Nerd Font"
                    :height 160
                    :weight 'normal)

(set-face-attribute 'bold nil
                    :font "iMWritingMono Nerd Font"
                    :weight 'bold)

(setq-default line-spacing 5)

;; disable visible-bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; No blinking cursor, it's hard to find sometimes
(blink-cursor-mode -1)

;; Don't open new frames if possible
(setq ns-pop-up-frames nil)

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
