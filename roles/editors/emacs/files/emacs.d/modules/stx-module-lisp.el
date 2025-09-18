;;;  -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
(defun stx/lisp-indent-function (indent-point state)
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local lisp-indent-function #'stx/lisp-indent-function)))

(defconst stx/lisp-mode-hooks
  '(cider-mode-hook
    cider-repl-mode-hook
    clojure-mode-hook
    emacs-lisp-mode-hook
    lisp-interaction-mode-hook
    lisp-mode-hook
    scheme-mode-hook))

(defun stx/add-lisp-mode-hook (mode)
  (dolist (hook stx/lisp-mode-hooks)
    (add-hook hook mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(use-package paredit
  :init (stx/add-lisp-mode-hook 'paredit-mode))

(use-package evil-cleverparens
  :init (stx/add-lisp-mode-hook 'evil-cleverparens-mode)
  :custom
  (evil-cleverparens-use-regular-insert t))

(use-package paxedit
  :init (stx/add-lisp-mode-hook 'paxedit-mode)
  :general
  (:states 'normal
   :keymaps 'override
   "M-j" 'paxedit-transpose-backward
   "M-k" 'paxedit-transpose-forward))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((clojure-mode emacs-lisp-mode) . aggressive-indent-mode))

(use-package eldoc
  :diminish eldoc-mode
  :hook ((cider-mode emacs-lisp-mode) . eldoc-mode))

(use-package elisp-mode
  :ensure nil ; build-in
  :general-config
  (:keymaps 'emacs-lisp-mode-map
   :states 'normal
   "SPC e" 'eval-last-sexp))

(provide 'stx-module-lisp)
