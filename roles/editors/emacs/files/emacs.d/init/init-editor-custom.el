;; reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 100000000)

;; Show column and linum in mode-line
(line-number-mode 1)
(column-number-mode 1)

;; osx titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; font settings
(set-face-attribute 'default t :font "Menlo")
(set-face-attribute 'default nil :height 150)
(setq-default line-spacing 4)

;; Nowrap
(setq-default truncate-lines t)

;; Prevent the cursor from blinking
(blink-cursor-mode 0)

;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; Disable message in echo area
(defun display-startup-echo-area-message ()
  (message ""))

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

;; use system clipboard
(setq x-select-enable-clipboard t)

;; Skip messages in next-buffer command
(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
  (when (string= "*Messages*" (buffer-name))
    (next-buffer)))

;; Skip messages in prev-buffer command
(defadvice previous-buffer (after avoid-messages-buffer-in-next-buffer)
  (when (string= "*Messages*" (buffer-name))
    (previous-buffer)))

;; Automaticaly add newline at and of document
(setq require-final-newline t)

;; Disable highlighting of matching par
(setq show-paren-mode t)

;; avoid hiding with M-h
(setq mac-pass-command-to-system nil)

;; disable visible-bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; warn when opening files bigger than 10MB
(setq large-file-warning-threshold 10000000)

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
(global-auto-revert-mode t)

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Save cursor position for files.
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
(setq compilation-scroll-output t)

;; Increase how much data is read from process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Disable compile warnings for gccemacs
(setq native-comp-async-report-warnings-errors nil)

(provide 'init-editor-custom)
