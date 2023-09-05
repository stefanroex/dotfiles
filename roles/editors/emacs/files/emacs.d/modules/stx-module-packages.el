(use-package iflipb
  :init
  (setq iflipb-wrap-around t)
  :general
  (keys-l
    "z" 'iflipb-previous-buffer
    "x" 'iflipb-next-buffer))

(use-package popper
  :general
  (keys
    "M-p" 'popper-toggle-latest
    "M-P" 'popper-cycle)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*xref\\*"
          "\\*rg\\*"
          "\\*cider-test-report\\*"
          "\\*cider-error\\*"
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

  (setq popper-window-height #'stx/popper-height)

  (popper-mode +1)
  ;; (popper-echo-mode +1)
  )

(provide 'stx-module-packages)
