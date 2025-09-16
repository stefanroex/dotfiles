;;;  -*- lexical-binding: t -*-

(defun consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol)))

(use-package vertico
  :demand t

  :general
  (keys-l
    "f" 'projectile-find-file
    "F" 'find-file)

  :custom
  (vertico-cycle t)

  :init
  (vertico-mode +1)
  (vertico-multiform-mode))

(use-package consult
  :general
  (keys-l
    "B" 'consult-buffer
    "b" 'consult-project-buffer
    "y" 'consult-yank-pop
    "i" 'consult-outline
    "s" 'consult-ripgrep
    "S" 'consult-ripgrep-at-point)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package hotfuzz
  :demand t
  :config
  (setq completion-styles '(hotfuzz)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package embark
  :general
  (keys
    :states '(nil normal emacs motion)
    :keymaps '(vertico-map prog-mode-map)
    "M-RET" 'embark-act)
  :custom
  (embark-prompter 'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :config
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'stx-module-completion)
