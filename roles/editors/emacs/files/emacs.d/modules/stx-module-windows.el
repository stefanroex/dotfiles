;;;  -*- lexical-binding: t -*-

(defvar stx/popper-reference-buffers
  '("\\*Messages\\*"
    "Output\\*$"
    "\\*Async Shell Command\\*"
    "\\*xref\\*"
    "\\*rg\\*"
    "\\*cider-test-report\\*"
    "\\*cider-error\\*"
    "\\*cider-inspect\\*"
    "\\*cider-repl.*"
    "\\*Backtrace\\*"
    "\\*helpful.*"
    "\\*ert\\*"
    help-mode
    compilation-mode))

(defun stx/popper-height (win)
  (fit-window-to-buffer
   win
   (floor (frame-height) 2)
   (floor (frame-height) 3)))

(use-package popper
  :general
  (keys
    "M-p" 'popper-toggle-latest
    "M-P" 'popper-cycle)

  :custom
  (popper-reference-buffers stx/popper-reference-buffers)
  (popper-window-height #'stx/popper-height)
  (popper-display-control 'user)

  :init
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package shackle
  :init
  (shackle-mode 1)
  :custom
  (shackle-rules '((compilation-mode      :align t     :size 0.3 :select nil)
                   (magit-log-mode        :align right :size 0.7 :select t :regexp t)
                   (magit-stash-mode      :align right :size 0.7 :select t :regexp t)
                   (magit-revision-mode   :align right :size 0.7 :select t :regexp t)
                   ("*rg*"                :align t     :size 0.3 :select nil)
                   ("*xref*"              :align t     :size 0.3 :select nil)
                   ("*Messages*"          :align t)
                   ("*Help*"              :align right           :select t)
                   ("*cider-inspect*"     :align right :size 0.5 :select t)
                   ("\\*Embark Export:.*" :align t     :size 0.3 :select nil :regexp t)
                   ("\\*cider-repl.*"     :align right :size 0.5 :select t   :regexp t))))

(use-package winner
  :hook (after-init . winner-mode))

(provide 'stx-module-windows)
