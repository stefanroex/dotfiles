;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config)

;; Flow (JS) flycheck config (http://flowtype.org)
;; from https://github.com/bodil/emacs.d/blob/master/bodil/bodil-js.el
(require 'f)
(require 'json)
(require 'flycheck)

(defun flycheck-parse-flow (output checker buffer)
  (let ((json-array-type 'list))
    (let ((o (json-read-from-string output)))
      (mapcar #'(lambda (errp)
                  (let ((err (cadr (assoc 'message errp))))
                    (flycheck-error-new
                     :line (cdr (assoc 'line err))
                     :column (cdr (assoc 'start err))
                     :level 'error
                     :message (cdr (assoc 'descr err))
                     :filename (f-relative
                                (cdr (assoc 'path err))
                                (f-dirname (file-truename
                                            (buffer-file-name))))
                     :buffer buffer
                     :checker checker)))
              (cdr (assoc 'errors o))))))

(flycheck-define-checker javascript-flow
  "Javascript type checking using Flow."
  :command ("flow" "check-contents" "--json" source-original)
  :standard-input t
  :error-parser flycheck-parse-flow
  :modes web-mode
  :next-checkers ((error . javascript-eslint)))

(add-to-list 'flycheck-checkers 'javascript-flow)

(flycheck-define-checker javascript-eslint
  "A Javascript syntax and style checker using eslint.
See URL `https://github.com/eslint/eslint'."
  :command ("eslint" "--format=checkstyle"
            (config-file "--config" flycheck-eslintrc)
            (option-list "--rulesdir" flycheck-eslint-rules-directories)
            "--rule" "{\"import/no-unresolved\": 0}"
            "--stdin" "--stdin-filename" source-original)
  :standard-input t
  :error-parser flycheck-parse-checkstyle
  :error-filter (lambda (errors)
                  (seq-do (lambda (err)
                            ;; Parse error ID from the error message
                            (setf (flycheck-error-message err)
                                  (replace-regexp-in-string
                                   (rx " ("
                                       (group (one-or-more (not (any ")"))))
                                       ")" string-end)
                                   (lambda (s)
                                     (setf (flycheck-error-id err)
                                           (match-string 1 s))
                                     "")
                                   (flycheck-error-message err))))
                          (flycheck-sanitize-errors errors))
                  errors)
  :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode web-mode))

(defun setup-local-eslint ()
  "If ESLint found in node_modules directory - use that for flycheck."
  (interactive)
  (let ((local-eslint (expand-file-name (concat (projectile-project-root) "node_modules/.bin/eslint")))
        (local-config (expand-file-name (concat (projectile-project-root) ".eslintrc"))))
    (setq flycheck-javascript-eslint-executable (and (file-exists-p local-eslint) local-eslint)
          flycheck-eslintrc (and (file-exists-p local-config) local-config))))

(add-hook 'web-mode-hook 'setup-local-eslint)

(provide 'init-flycheck)
;;; init-flycheck ends here
