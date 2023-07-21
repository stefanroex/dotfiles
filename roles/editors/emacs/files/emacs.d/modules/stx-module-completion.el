(use-package vertico
  :demand t
  :init
  (vertico-mode +1)
  (setq vertico-cycle t)
  (keys-l
    "f" 'projectile-find-file
    "F" 'find-file))

(use-package consult
  :general
  (keys-l
    "B" 'consult-buffer
    "b" 'consult-project-buffer
    "y" 'consult-yank-pop))

;; (use-package orderless
;;   :demand t
;;   :custom
;;   (completion-styles '(basic orderless))
;;   (completion-category-overrides '((file (styles basic partial-completion))))
;;   (orderless-matching-styles '(orderless-literal
;;                                orderless-regexp
;;                                orderless-prefixes
;;                                orderless-initialism)))

;; (use-package hotfuzz
;;   :demand t)

(use-package fussy
  :demand t
  :config
  ;; (setq fussy-score-fn 'fussy-hotfuzz-score)
  ;; (setq fussy-filter-fn 'fussy-filter-default)
  (push 'fussy completion-styles)
  (setq
   ;; For example, project-find-file uses 'project-files which uses
   ;; substring completion by default. Set to nil to make sure it's using
   ;; flx.
   completion-category-defaults nil
   completion-category-overrides nil))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package selectrum
;;   :demand t
;;   :init
;;   (selectrum-mode +1)
;;   (keys-l
;;     "f" 'projectile-find-file
;;     "F" 'find-file))

;; (use-package hotfuzz
;;   :demand t
;;   :init
;;   (hotfuzz-selectrum-mode +1)
;;   (setq completion-styles '(hotfuzz)
;;         completion-ignore-case t))

;;;;;;;;;;;;;;;;;;;

;; (use-package flx)

;; (use-package ivy
;;   :defer 0.1
;;   :diminish ivy-mode
;;   :init
;;   (setq ivy-height 20
;;         ivy-count-format "(%d/%d) "
;;         ivy-initial-inputs-alist nil
;;         ivy-on-del-error-function nil
;;         ivy-virtual-abbreviate 'full
;;         ivy-re-builders-alist '((t . ivy--regex-fuzzy))
;;         enable-recursive-minibuffers t)
;;   :config
;;   (ivy-mode)
;;   (general-def 'ivy-minibuffer-map
;;     "C-j" 'ivy-next-line
;;     "C-k" 'ivy-previous-line))

;; (use-package counsel
;;   :general
;;   (keys-l
;;     "y" 'counsel-yank-pop
;;     "f" 'counsel-projectile-find-file
;;     "F" 'counsel-find-file
;;     "b" 'counsel-projectile-switch-to-buffer)
;;   (keys :states nil
;;     "M-x" 'counsel-M-x))

;; (use-package smex)

(provide 'stx-module-completion)
