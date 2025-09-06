(defun display-buffer-full-screen (buffer alist)
  (delete-other-windows)
  (set-window-dedicated-p nil nil)
  (set-window-buffer nil buffer)
  (get-buffer-window buffer))

(defun magit-buffer-full-screen (buffer)
  (if magit-display-buffer-noselect
      (magit-display-buffer-traditional buffer)
    (display-buffer buffer '(display-buffer-full-screen))))

(use-package magit
  :general
  (keys-l
    "gs" 'magit-status
    "gl" 'magit-log-head
    "gb" 'magit-blame)
  (keys :keymaps 'magit-blame-read-only-mode-map
    "RET" 'magit-show-commit)

  :config
  (general-def 'magit-mode-map        "SPC"      nil)
  (general-def 'transient-map         "q" 'transient-quit-one)
  (general-def 'transient-edit-map    "q" 'transient-quit-one)
  (general-def 'transient-sticky-map  "q" 'transient-quit-seq)
  (general-def 'magit-status-mode-map "K" 'magit-discard)

  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (setq magit-diff-refine-hunk t
        magit-display-buffer-function 'magit-buffer-full-screen))

(use-package forge
  :after (magit evil-collection)
  :general
  (keys-l
    "go" 'forge-browse)
  :custom
  (forge-add-default-bindings nil)
  (forge-add-default-sections nil)
  :config
  (evil-collection-forge-setup))

(provide 'stx-module-magit)
