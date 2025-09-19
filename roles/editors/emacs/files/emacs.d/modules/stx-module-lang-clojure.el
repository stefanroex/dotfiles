;;;  -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :hook
  ((clojure-mode . lsp)
   (clojurec-mode . lsp)
   (clojurescript-mode . lsp))

  :general-config
  (:states '(normal emacs motion)
   "SPC j" 'stx/lsp-add-missing-libspec
   "SPC lc" 'lsp-clojure-clean-ns
   "SPC lf" 'lsp-clojure-extract-function
   "SPC ll" 'lsp-clojure-move-to-let)

  :config
  (require 'flycheck-clj-kondo)
  (modify-syntax-entry ?? "w" clojure-mode-syntax-table)
  (modify-syntax-entry ?! "w" clojure-mode-syntax-table)

  (with-eval-after-load 'lsp
    (dolist (m '(clojure-mode clojurec-mode clojurescript-mode))
      (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

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

(use-package cider
  :custom
  (cider-download-java-sources t)
  (cider-dynamic-indentation t)
  (cider-eval-result-duration nil)
  (cider-ns-refresh-after-fn "reloaded.repl/resume")
  (cider-ns-refresh-before-fn "reloaded.repl/suspend")
  (cider-print-fn 'puget)
  (cider-prompt-for-symbol nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-history-file "~/.emacs.d/cider-history")
  (cider-repl-use-pretty-printing t)
  (cider-repl-wrap-history t)
  (cider-test-fail-fast nil)
  (nrepl-hide-special-buffers t)

  :config
  (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
  (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)
  (advice-add 'evil-search-highlight-persist-remove-all :after #'cider--remove-result-overlay)

  :general-config
  (:keymaps '(cider-repl-mode-map clojure-mode-map clojurescript-mode-map)
   :states '(normal emacs motion)
   "SPC D" 'cider-grimoire-web
   "SPC E" 'cider-pprint-eval-last-sexp
   "SPC T" 'cider-test-run-project-tests
   "SPC cc" 'cider-connect
   "SPC cd" 'cider-doc-map
   "SPC ceb" 'cider-load-buffer
   "SPC cef" 'cider-eval-defun-at-point
   "SPC cef" 'cider-load-file
   "SPC cel" 'cider-eval-last-sexp
   "SPC cer" 'cider-pprint-eval-last-sexp-to-comment
   "SPC ci" 'cider-inspect
   "SPC cj" 'cider-jack-in
   "SPC cl" 'cider-inspect-last-result
   "SPC cm" 'cider-macroexpand-1
   "SPC cpf" 'cider-pprint-eval-defun-at-point
   "SPC cpl" 'cider-pprint-eval-last-sexp
   "SPC cq" 'cider-quit
   "SPC cr" 'cider-ns-refresh
   "SPC csn" 'cider-repl-set-ns
   "SPC csr" 'cider-switch-to-repl-buffer
   "SPC ct" 'cider-test-run-project-tests
   "SPC d" 'cider-doc
   "SPC e" 'cider-eval-last-sexp
   "SPC n" 'cider-eval-sexp-at-point
   "SPC t" 'cider-test-run-ns-tests
   "gf" 'cider-find-var)

  (:keymaps 'cider-inspector-mode-map
   :states 'emacs
   "u" 'cider-inspector-pop
   "j" 'evil-next-line
   "k" 'evil-previous-line
   "h" 'evil-backward-char
   "l" 'evil-forward-char
   "o" 'cider-inspector-operate-on-point)

  (:keymaps 'cider-repl-mode-map
   :states 'normal
   ";" 'cider-repl-shortcuts-help
   "<return>" 'cider-repl-return))

(use-package clj-refactor
  :hook
  ((clojure-mode . clj-refactor-mode)
   (clojurec-mode . clj-refactor-mode)
   (clojurescript-mode . clj-refactor-mode))

  :custom
  (cljr-auto-sort-ns t)
  (cljr-insert-newline-after-require nil)
  (cljr-magic-requires nil)
  (cljr-favor-prefix-notation nil)
  (cljr-favor-private-functions nil)
  (cljr-project-clean-prompt nil)
  (cljr-clojure-test-declaration "[clojure.test :refer [deftest is testing]]")

  :general-config
  (:states '(normal emacs motion)
   "SPC rn" 'cljr-rename-file))

(provide 'stx-module-lang-clojure)
