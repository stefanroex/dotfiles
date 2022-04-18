(use-package flx)

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq ivy-height 20
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-on-del-error-function nil
        ivy-virtual-abbreviate 'full
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        enable-recursive-minibuffers t)
  (general-def 'ivy-minibuffer-map
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line))

(use-package counsel
  :general
  (keys-l
    "y" 'counsel-yank-pop
    "f" 'counsel-projectile-find-file
    "F" 'counsel-find-file
    "b" 'counsel-projectile-switch-to-buffer)
  (keys :states nil
    "M-x" 'counsel-M-x)
  :config
  (setq counsel-fzf-cmd "rg --files --hidden --follow --glob '!.git'"))

(provide 'stx-module-completion)
