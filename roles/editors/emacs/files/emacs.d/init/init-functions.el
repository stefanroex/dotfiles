(defun my-switch-to-other-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun open-emacs-config ()
  "Open the emacs packages file for quick changes."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun display-buffer-full-screen (buffer alist)
  (delete-other-windows)
  (set-window-dedicated-p nil nil)
  (set-window-buffer nil buffer)
  (get-buffer-window buffer))

(defun magit-buffer-full-screen (buffer)
  (if magit-display-buffer-noselect
      (magit-display-buffer-traditional buffer)
    (display-buffer buffer '(display-buffer-full-screen))))

(defun neotree-project-root ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-show)
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name)))))

(defun neotree-toggle-project-root ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (neotree-project-root)))

(defun add-hooks (mode-hooks mode)
  "Add mode to all mode hooks"
  (dolist (mode-hook mode-hooks)
    (add-hook mode-hook mode)))

(provide 'init-functions)
