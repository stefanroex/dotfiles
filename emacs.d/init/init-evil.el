(provide 'init-evil)

;; Evil mode
(evil-mode t)
(global-evil-tabs-mode 1)
(global-evil-surround-mode 1)
(evil-ex-define-cmd
  "A" 'projectile-toggle-between-implementation-and-test)

;; Map ; to : for easier ex access
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-motion-state-map (kbd ";") 'evil-ex)

;; Evil leader
(defun my-switch-to-other-buffer ()
  "Switch to other buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun open-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun next-code-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (next-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name) )) )
      (next-buffer))))

(defun previous-code-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (previous-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name) )) )
      (previous-buffer))))

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "," 'my-switch-to-other-buffer
  "x" 'next-code-buffer
  "z" 'previous-code-buffer
  "b" 'switch-to-buffer
  "w" 'quit-window
  "m" 'smex
  "F" 'find-file
  "k" 'kill-buffer
  "v" 'open-emacs-config)

;; esc quits
(setq evil-intercept-esc 'always)
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; Moves lines
(define-key evil-normal-state-map (kbd "] e") 'move-text-down)
(define-key evil-normal-state-map (kbd "[ e") 'move-text-up)

;; Next and previous search results
(define-key evil-normal-state-map (kbd "C-n") 'next-error)
(define-key evil-normal-state-map (kbd "C-p") 'previous-error)

;; emacs org mode
(require 'evil-org)
(defun disable-linenumbers ()
    (linum-mode -1))
(add-hook 'org-mode-hook 'disable-linenumbers)
(add-hook 'org-mode-hook 'visual-line-mode)

(setq evil-shift-width 2)

;; Easy window management
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; Window switching similar in emacs mode
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

;; Tab navigation
(global-set-key (kbd "s-w") 'elscreen-kill)
(global-set-key (kbd "s-t") 'elscreen-create)
(global-set-key (kbd "s-}") 'elscreen-next)
(global-set-key (kbd "s-{") 'elscreen-previous)
