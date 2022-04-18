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
  :diminish 'auto-revert-mode
  :init
  (keys-l
    "gs" 'magit-status
    "gl" 'magit-log-head
    "gb" 'magit-blame)
  :config
  (general-def 'transient-map        "q" 'transient-quit-one)
  (general-def 'transient-edit-map   "q" 'transient-quit-one)
  (general-def 'transient-sticky-map "q" 'transient-quit-seq)
  (keys :keymaps 'magit-status-mode-map
    "K" 'magit-discard)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (setq magit-diff-refine-hunk t
        magit-display-buffer-function 'magit-buffer-full-screen)
  (define-key magit-mode-map (kbd "SPC") nil)
  (global-auto-revert-mode t))

(provide 'stx-module-magit)
