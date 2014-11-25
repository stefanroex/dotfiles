(provide 'init-bootstrap)

(push "/usr/local/bin" exec-path)
(push "/usr/bin" exec-path)

;; Setting rbenv path
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
