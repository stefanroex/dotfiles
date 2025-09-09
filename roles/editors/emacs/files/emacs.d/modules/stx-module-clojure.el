;;;  -*- lexical-binding: t -*-
(require 'subr-x)

(defun stx/lsp--usage-count (title)
  (if (string-match "× \\([0-9]+\\)$" title)
      (string-to-number (match-string 1 title))
    0))

(defun stx/lsp--sort-usage-count (actions)
  (seq-sort-by (lambda (a) (stx/lsp--usage-count (plist-get a :title))) #'> actions))

(defun stx/lsp--retitle-action (action)
  (let ((title (plist-get action :title)))
    (string-match "^Add require '\\([^']+\\)'\\(?: × \\([0-9]+\\)\\)?$" title)
    (let ((lib (match-string 1 title))
          (cnt (match-string 2 title)))
      (plist-put (copy-sequence action) :title (if cnt (format "%s (%s×)" lib cnt) lib)))))

(defun stx/lsp-add-missing-libspec ()
  (interactive)
  (let ((actions (seq-filter (lambda (action)
                               (string-prefix-p "Add require" (plist-get action :title)))
                             (lsp-code-actions-at-point))))
    (if (= (length actions) 1)
        (lsp-execute-code-action
         (car actions))
      (lsp-execute-code-action
       (lsp--select-action
        (mapcar 'stx/lsp--retitle-action (stx/lsp--sort-usage-count actions)))))
    (flycheck-buffer)))

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo)
  (modify-syntax-entry ?? "w" clojure-mode-syntax-table)
  (modify-syntax-entry ?! "w" clojure-mode-syntax-table)

  (define-clojure-indent
   (with-tmp-taf-dir 1)
   (with-test-system 1)
   (with-tmp-dir 1)
   (with-tmp-file 1)
   (try-let 1)
   (for-all 1)
   (assoc 1)
   (do-at 1)))

(use-package clojure-mode-extra-font-locking)

(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom-face
  (lsp-face-highlight-textual ((t (:inherit region))))
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (keys :prefix "SPC l"
    "R" 'lsp-rename
    "a" 'lsp-execute-code-action
    "f" 'lsp-clojure-extract-function
    "l" 'lsp-clojure-move-to-let
    "c" 'lsp-clojure-clean-ns
    "r" 'lsp-find-references)

  (keys :prefix "SPC"
    "j" 'stx/lsp-add-missing-libspec)

  ;; performance tweaks
  (setq lsp-enable-folding nil
        lsp-enable-file-watchers nil
        lsp-enable-text-document-color nil)

  (setq lsp-keymap-prefix "SPC l"
        lsp-lens-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-indentation nil
        ;; lsp-log-io t
        ;; lsp-use-plists t
        lsp-completion-enable nil
        lsp-enable-on-type-formatting nil
        lsp-modeline-code-actions-enable nil
        ;; lsp-enable-symbol-highlighting nil
        lsp-eldoc-enable-hover nil
        lsp-idle-delay 0.05
        lsp-completion-provider :capf
        lsp-diagnostics-provider :none)

  )

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-peek-enable nil))

(use-package cider
  :config
  (setq cider-ns-refresh-before-fn "reloaded.repl/suspend"
        cider-ns-refresh-after-fn "reloaded.repl/resume"
        cider-print-fn 'puget)

  (setq cider-prompt-for-symbol nil
        cider-download-java-sources t
        cider-dynamic-indentation t
        cider-test-fail-fast nil
        cider-repl-display-help-banner nil
        cider-eval-result-duration nil
        cider-repl-use-pretty-printing t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t)

  (setq nrepl-hide-special-buffers t)

  (defvar cider-mode-maps
    '(cider-repl-mode-map
      clojure-mode-map
      clojurescript-mode-map))

  ;; (add-to-list 'evil-emacs-state-modes 'cider-inspector-mode)
  ;; (add-to-list 'evil-emacs-state-modes 'cider-connections-buffer-mode)
  (evil-add-hjkl-bindings cider-connections-buffer-mode-map 'emacs)

  (keys :keymaps '(cider-inspector-mode-map)
    :modes 'emacs
    "u" 'cider-inspector-pop
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "h" 'evil-backward-char
    "l" 'evil-forward-char
    "o" 'cider-inspector-operate-on-point)

  (keys :keymaps '(cider-repl-mode-map)
    :modes 'normal
    ";" 'cider-repl-shortcuts-help
    "<return>" 'cider-repl-return)

  (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
  (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)

  (keys-l :keymaps cider-mode-maps
    "E" 'cider-pprint-eval-last-sexp
    "T" 'cider-test-run-project-tests
    "t" 'cider-test-run-ns-tests
    "d" 'cider-doc
    "n" 'cider-eval-sexp-at-point
    "D" 'cider-grimoire-web
    "e" 'cider-eval-last-sexp)

  (keys :keymaps cider-mode-maps
    "gf" 'cider-find-var
    ;; "gF" 're-frame-jump-to-reg
    )

  (keys :keymaps cider-mode-maps
    :prefix "SPC c"
    ;; "J" 'cider-project-cljs
    "c" 'cider-connect
    "d" 'cider-doc-map
    "eb" 'cider-load-buffer
    "ef" 'cider-eval-defun-at-point
    "ef" 'cider-load-file
    "el" 'cider-eval-last-sexp
    "er" 'cider-pprint-eval-last-sexp-to-comment
    "i" 'cider-inspect
    "j" 'cider-jack-in
    "l" 'cider-inspect-last-result
    "m" 'cider-macroexpand-1
    "pf" 'cider-pprint-eval-defun-at-point
    "pl" 'cider-pprint-eval-last-sexp
    "q" 'cider-quit
    "r" 'cider-ns-refresh
    "sn" 'cider-repl-set-ns
    "sr" 'cider-switch-to-repl-buffer
    "t" 'cider-test-run-project-tests)

  (advice-add 'evil-search-highlight-persist-remove-all :after #'cider--remove-result-overlay)

  )

(use-package clj-refactor
  :hook ((clojure-mode . clj-refactor-mode)
         (clojurec-mode . clj-refactor-mode)
         (clojurescript-mode . clj-refactor-mode))
  :init
  (setq cljr-auto-sort-ns t
	cljr-insert-newline-after-require nil
        cljr-magic-requires nil
        cljr-favor-prefix-notation nil
        cljr-favor-private-functions nil
        cljr-project-clean-prompt nil
        cljr-clojure-test-declaration "[clojure.test :refer [deftest is testing]]")
  :config
  (dolist (details cljr--all-helpers)
    (let ((key (car details))
          (fn (cadr details)))
      (keys :prefix "SPC r"
        :keymaps '(clojure-mode-map clojurescript-mode-map)
        key fn)))
  (keys :prefix "SPC r"
    "r n" 'cljr-rename-file))

(provide 'stx-module-clojure)
