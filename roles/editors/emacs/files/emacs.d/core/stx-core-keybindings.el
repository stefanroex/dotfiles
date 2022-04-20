(defun stx/open-emacs-config ()
  "Open the emacs packages file for quick changes."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun stx/switch-to-other-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun stx/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defconst escape-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    minibuffer-local-shell-command-map))

(use-package general
  :demand
  :config
  (progn
    (general-auto-unbind-keys)

    (general-define-key
     :states 'motion
     "SPC" nil)

    (general-define-key
     :states nil
     :keymaps escape-maps
     "<escape>" 'abort-recursive-edit)

    (general-create-definer
      keys
      :states '(normal emacs motion))

    (general-create-definer
      keys-l
      :prefix "SPC"
      :states '(normal emacs motion))

    (keys-l
      :states 'normal
      :keymaps 'emacs-lisp-mode-map
      "e" 'eval-last-sexp)

    (keys-l
      "hk" 'describe-key
      "hm" 'describe-mode
      "hf" 'describe-function
      "hv" 'describe-variable
      "SPC" 'stx/switch-to-other-buffer
      "k" 'stx/kill-other-buffers
      "q" 'kill-buffer-and-window
      "w" 'delete-window
      "B" 'ibuffer
      "O" 'open-iterm-in-project-root
      "v" 'stx/open-emacs-config)))

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :init
  (keys-l
    "h1" 'which-key-show-major-mode
    "h2" 'which-key-show-minor-mode-keymap)
  :config
  (setq which-key-side-window-max-width 0.7
        which-key-add-column-padding 1)
  (which-key-mode +1)
  (which-key-setup-side-window-right))

(use-package free-keys
  :general
  (keys-l
    "hu" 'free-keys)
  :config
  (keys
    :keymaps 'free-keys-mode-map
    "p" 'free-keys-set-prefix
    "q" 'kill-buffer-and-window))

(provide 'stx-core-keybindings)
