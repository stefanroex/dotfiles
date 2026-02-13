;;;  -*- lexical-binding: t -*-

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

;; Make some buffers slightly darker
(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

;; Theme
(use-package challenger-deep-theme
  :demand
  :init
  (load-theme 'challenger-deep t)

  (set-face-attribute 'fringe nil :background "#100e23" :foreground "#565575"))

;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package nerd-icons
  :commands nerd-icons-install-fonts
  :functions font-available-p
  :config
  (when (and (display-graphic-p)
             (not (find-font (font-spec :name "Symbols Nerd Font Mono"))))
    (nerd-icons-install-fonts t)))

(provide 'stx-core-ui)
