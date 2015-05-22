;; Scroll in less larger steps
;; (setq scroll-margin 5
;;     scroll-step 5)

;; Show Linenumbers
;; (global-linum-mode 1)

;; Nowrap
;; (setq-default truncate-lines t)

;; start maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

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

;; shell scripts
;; (setq-default sh-basic-offset 2)
;; (setq-default sh-indentation 2)

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

;; Turn down the time to echo keystrokes so I don't have to wait around for things to happen. Dialog boxes are also a bit annoying, so just have Emacs use the echo area for everything. Beeping is for robots, and I am not a robot.

(setq echo-keystrokes 0.1
    use-dialog-box nil)

;; Disable show params mode from better-defaults

(show-paren-mode -1)

;; Ingore ring-bell

(setq ring-bell-function 'ignore)

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

(provide 'init-custom)
