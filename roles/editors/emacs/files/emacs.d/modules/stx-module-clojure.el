;;;  -*- lexical-binding: t -*-
(require 'subr-x)

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo)
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
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]node_modules")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]resources")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]target")
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (keys :prefix "SPC l"
    "R" 'lsp-rename
    "r" 'lsp-find-references)

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
  (defun portal.api/open ()
    (interactive)
    (cider-nrepl-sync-request:eval
     "(require 'portal.api) (portal.api/tap) (portal.api/open)"))

  (defun portal.api/clear ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/clear)"))

  (defun portal.api/close ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/close)"))

  (setq cider-ns-refresh-before-fn "reloaded.repl/suspend"
        cider-ns-refresh-after-fn "reloaded.repl/resume"
        cider-print-fn 'puget)

  (setq cider-prompt-for-symbol nil
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

  (add-to-list 'evil-emacs-state-modes 'cider-inspector-mode)
  (add-to-list 'evil-emacs-state-modes 'cider-connections-buffer-mode)
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

  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

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
    "c" 'lsp-execute-code-action
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
    "po" 'portal.api/open
    "pc" 'portal.api/clear
    "pq" 'portal.api/close
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
    "r n" 'cljr-rename-file)
  (add-to-list 'cljr-magic-require-namespaces '("r"  . "reagent.core"))
  (add-to-list 'cljr-magic-require-namespaces '("rf" . "re-frame.core"))
  (add-to-list 'cljr-magic-require-namespaces '("u" . "bm.tools.utils"))
  (add-to-list 'cljr-magic-require-namespaces '("ui.text-input"  . "bm.frontend.components.form.text-input"))
  (add-to-list 'cljr-magic-require-namespaces '("ui.textarea"  . "bm.frontend.components.form.textarea"))
  (add-to-list 'cljr-magic-require-namespaces '("ui.button"  . "bm.frontend.components.button"))
  (add-to-list 'cljr-magic-require-namespaces '("ui.form"  . "bm.frontend.components.form.primitives"))
  (add-to-list 'cljr-magic-require-namespaces '("ui.segmented-group"  . "bm.frontend.components.segmented-group"))
  (add-to-list 'cljr-magic-require-namespaces '("ui.select"  . "bm.frontend.components.form.select"))
  (add-to-list 'cljr-magic-require-namespaces '("ui.radios"  . "bm.frontend.components.form.radios"))
  (add-to-list 'cljr-magic-require-namespaces '("styleguide"  . "bm.frontend-styleguide.macros")))

(provide 'stx-module-clojure)
