(defun my-switch-to-other-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun open-emacs-config ()
  "Open the emacs packages file for quick changes."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defconst my-skippable-buffers '("*Messages*" "*scratch*" "*Help*"))

(defun my-change-buffer (circle-fn)
  (let ((bread-crumb (buffer-name)))
    (funcall circle-fn)
    (while
        (and
         (not (code-buffer? (buffer-name)))
         (not (equal bread-crumb (buffer-name))) )
      (funcall circle-fn))))

(defun next-code-buffer ()
  (interactive)
  (my-change-buffer 'next-buffer))

(defun previous-code-buffer ()
  (interactive)
  (my-change-buffer 'previous-buffer))

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

(provide 'init-functions)
