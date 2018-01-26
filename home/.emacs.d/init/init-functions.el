(defun my-switch-to-other-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun open-emacs-config ()
  "Open the emacs packages file for quick changes."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(setq my-skippable-buffers '("*Messages*" "*scratch*" "*Help*"))

(defun my-change-buffer (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `my-skippable-buffers'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (member (buffer-name) my-skippable-buffers)
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

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

(defun open-iterm-in-project-root ()
  "Opens iTerm for current directory."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (shell-command "open -a iTerm $PWD && echo 'Opening' $PWD 'in iTerm...'")))

(provide 'init-functions)
