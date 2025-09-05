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
  (vertico-mode +1))

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
    "C-;" 'embark-act))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep)

(provide 'stx-module-completion)
