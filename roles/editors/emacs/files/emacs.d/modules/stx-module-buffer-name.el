;;;  -*- lexical-binding: t -*-

;; Provides a function that renames buffers to include file paths
;; relative to the project root. This makes it easier to distinguish
;; between files with the same name (e.g. multiple `endpoint.clj`
;; files in different locations) and improves both identification and
;; searchability.

(require 'projectile)

(defun buffer-file-name-rename-buffer* ()
  (when (projectile-project-p)
    (let ((new-name (file-relative-name
                     (buffer-file-name)
                     (projectile-project-root))))
      (when (and new-name
                 (not (equal (buffer-name) new-name)))
        (rename-buffer new-name t)))))

(defun buffer-file-name-rename-buffer (buffer)
  (with-current-buffer buffer
    (buffer-file-name-rename-buffer*)))

(defun buffer-file-name-find-file-noselect-advice (ffn-fun filename &rest args)
  (let* ((buffer (apply ffn-fun filename args)))
    (buffer-file-name-rename-buffer buffer)
    buffer))

(defun buffer-file-name-create-file-buffer-advice (cfb-fun filename &rest args)
  (if (and (file-exists-p filename)
           (projectile-project-p))
      (let* ((new-name (file-relative-name filename (projectile-project-root))))
        (if (and new-name
                 (not (equal (buffer-name) new-name)))
            (progn
              (generate-new-buffer new-name)
              new-name)
          (apply cfb-fun filename args)))
    (apply cfb-fun filename args)))

(defun buffer-file-name-install ()
  (advice-add 'find-file-noselect :around #'buffer-file-name-find-file-noselect-advice)
  (advice-add 'create-file-buffer :around #'buffer-file-name-create-file-buffer-advice)
  (add-hook 'after-save-hook #'buffer-file-name-rename-buffer*))

(buffer-file-name-install)

(provide 'stx-module-buffer-name)
