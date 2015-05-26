;; Show Linenumbers
;; (global-linum-mode t)

;; Dont show linenumbers in terminal
;; (add-hook 'term-mode-hook (lambda () (linum-mode -1)))

;; Show column and linum in mode-line
(line-number-mode 1)
(column-number-mode 1)

;; Nowrap
(setq-default truncate-lines t)

;; Prevent the cursor from blinking
(blink-cursor-mode 0)

;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; Spaces instead of tabs
(setq tab-width 2
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
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; Turn down the time to echo keystrokes so I don't have to wait around for things to happen. Dialog boxes are also a bit annoying, so just have Emacs use the echo area for everything. Beeping is for robots, and I am not a robot.
;; (setq echo-keystrokes 0.1
;;     use-dialog-box nil)

;; use system clipboard
(setq x-select-enable-clipboard t)

;; Skip messages in next-buffer command
;; (defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
;; (when (string= "*Messages*" (buffer-name))
;;     (next-buffer)))

;; Skip messages in prev-buffer command
;; (defadvice previous-buffer (after avoid-messages-buffer-in-next-buffer)
;; (when (string= "*Messages*" (buffer-name))
;;     (previous-buffer)))

;; Automaticaly add newline at and of document
(setq require-final-newline t)

(provide 'init-custom)
