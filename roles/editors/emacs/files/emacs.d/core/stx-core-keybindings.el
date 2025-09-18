;;;  -*- lexical-binding: t -*-

(defun stx/open-emacs-config ()
  "Open the emacs packages file for quick changes."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun stx/switch-to-other-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(use-package general
  :demand
  :init
  (general-auto-unbind-keys)

  ;; Define global SPC as prefix key
  (general-define-key
   :states 'motion
   "SPC" nil))

(use-package simple
  :ensure nil

  :general-config
  (:states '(normal emacs motion)
   "C-n" 'next-error
   "C-p" 'previous-error))

(use-package window
  :ensure nil

  :general-config
  (:states '(normal emacs motion)
   "SPC SPC" 'stx/switch-to-other-buffer
   "SPC q" 'kill-buffer-and-window
   "SPC v" 'stx/open-emacs-config
   "SPC w" 'delete-window))

(use-package help
  :ensure nil

  :general-config
  (:states 'normal
   "SPC hk" 'describe-key
   "SPC hb" 'describe-bindings
   "SPC hf" 'describe-function
   "SPC hm" 'describe-mode
   "SPC hv" 'describe-variable))

;; (use-package minibuffer
;;   :ensure nil
;;   :general-config
;;   (:states nil
;;    :keymaps '(minibuffer-local-map
;;               minibuffer-local-ns-map
;;               minibuffer-local-completion-map
;;               minibuffer-local-must-match-map
;;               minibuffer-local-isearch-map
;;               minibuffer-local-shell-command-map)
;;    "<escape>" 'abort-recursive-edit))

(use-package which-key
  :defer 1
  :diminish which-key-mode

  :general-config
  (:states 'normal
   "SPC h1" 'which-key-show-major-mode
   "SPC h2" 'which-key-show-minor-mode-keymap)

  :custom
  (which-key-side-window-max-width 0.7)
  (which-key-add-column-padding 1)

  :config
  (which-key-mode +1)
  (which-key-setup-side-window-right))

(use-package free-keys
  :general
  (:states 'normal
   "SPC hu" 'free-keys)

  :general-config
  (:keymaps 'free-keys-mode-map
   "p" 'free-keys-set-prefix
   "q" 'kill-buffer-and-window))

(provide 'stx-core-keybindings)
