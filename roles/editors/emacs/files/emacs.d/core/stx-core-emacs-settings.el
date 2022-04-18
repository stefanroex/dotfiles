;; Show column and linum in mode-line
(line-number-mode 1)
(column-number-mode 1)

;; Nowrap
(setq-default truncate-lines t)

;; Don't use messages that you don't read
(setq initial-scratch-message ""
      inhibit-startup-message t
      inhibit-startup-screen t)

;; Spaces instead of tabs
(setq c-basic-offset 2
      js-indent-level 2
      css-indent-offset 2
      tab-width 2
      indent-tabs-mode nil)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Disable backup files
(setq make-backup-files nil)

;; Deal with temp files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; just type y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; adjustable text-size
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Automaticaly add newline at and of document
(setq require-final-newline t)

;; Disable highlighting of matching par
(setq show-paren-mode t)

;; warn when opening files bigger than 2MB
(setq large-file-warning-threshold (* 2 1024 1024))

;; Automatically remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; whitespace-mode config
(require 'whitespace)
(whitespace-mode t)
(setq whitespace-line-column 80
      whitespace-style '(face tabs empty trailing lines-tail))

;; enabled line numbers
(global-linum-mode)

;; revert buffers automatically when underlying files are changed externally
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Save cursor position for files.
(require 'saveplace)
(save-place-mode 1)
(setq save-place-file (locate-user-emacs-file "places"))

;; Load latest compiled elisp file
(setq load-prefer-newer t)

;; prevent new windows
(setq pop-up-frames nil)

;; sh indentation
(setq sh-basic-offset 2)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Don't confirm when creating new file
(setq confirm-nonexistent-file-or-buffer nil)

;; Scroll in compilation mode
(require 'compile)
(setq compilation-scroll-output t)

;; Increase how much data is read from process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; When buffers have the same name, uniqify the names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq-default recentf-max-menu-items 50
                recentf-max-saved-items 100)
  (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/")))

(provide 'stx-core-emacs-settings)
