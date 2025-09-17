;;;  -*- lexical-binding: t -*-

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
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Deal with temp files
(when (not (file-directory-p (expand-file-name "backups" user-emacs-directory)))
  (make-directory (expand-file-name "backups" user-emacs-directory)))
(when (not (file-directory-p (expand-file-name "auto-save-list" user-emacs-directory)))
  (make-directory (expand-file-name "auto-save-list" user-emacs-directory)))

;; Put backups and auto-save files in subdirectories, so the
;; user-emacs-directory doesn't clutter
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;; just type y or n
;; TODO: Doesn't seem to compile correcty?
; (defalias 'yes-or-no-p 'y-or-n-p)

;; adjustable text-size
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Automaticaly add newline at and of document
(setq require-final-newline t)

;; Enable highlighting of matching par
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
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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
(setq-default sh-basic-offset 2)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Don't confirm when creating new file
(setq confirm-nonexistent-file-or-buffer nil)

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

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; No electric indent
(setq electric-indent-mode nil)

;; Set *scatch* buffer to fundamental-mode instead of elisp-mode.
;; elisp-mode triggers prog-mode hooks which loads extra packages.
(setq initial-major-mode 'fundamental-mode)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Never split windows
;; (advice-add 'window-splittable-p :before-while (lambda () nil))

;; Disable warnings for emacs compile
(setq-default warning-minimum-level :error)

(provide 'stx-core-emacs-settings)
