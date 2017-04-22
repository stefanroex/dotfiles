;;; package --- Summary
;;; Commentary:
;;; Code:

(provide 'init-flow)

(defun eldoc-flow-type ()
  "Call flow type type-at-pos and messages first line."
  (let* ((command (format "flow type-at-pos %s %d %d"
                          (buffer-file-name)
                          (line-number-at-pos)
                          (+ (current-column) 1)))
         (output (shell-command-to-string command))
         (first-line (car (split-string output "\n"))))
    (unless (string= first-line "(unknown)")
      (propertize first-line 'face 'font-lock-variable-name-face))))

(add-hook 'web-mode-hook
	  '(lambda ()
	     (set
	      (make-local-variable 'eldoc-documentation-function)
	      'eldoc-flow-type)
	     (eldoc-mode)))

;; (defun flow-pragma-exists ()
;;   "Check if @flow exists in file."
;;   (save-excursion
;;     (save-match-data
;;       (goto-char (point-min))
;;       (not (null (search-forward "@flow" 200 t))))))

;;; init-flow.el ends here
