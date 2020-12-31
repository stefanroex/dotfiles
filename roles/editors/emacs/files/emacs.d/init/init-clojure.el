;;;  -*- lexical-binding: t -*-
(require 'subr-x)

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :defer t
  :config
  (require 'flycheck-clj-kondo)

  (define-clojure-indent
    (GET 'defun)
    (POST 'defun)
    (PATCH 'defun)
    (DELETE 'defun)

    (assoc 1)
    (do-at 1)
    ;; Custom
    (try-let 'defun)
    (for-all 'defun)

    ;; scope capture
    (letsc 'defun)

    ;; core.match
    (match 'defun)

    ;; midje
    (fact 'defun)
    (facts 'defun)
    (fact-group 'defun)
    (silent-fact 'defun)
    (future-fact 'defun)
    (tabular 'defun)
    (against-background 'defun)
    (provided 0)

    ;; om & om-tools indenting
    (display-name 'defun)
    (init-state 'defun)
    (will-mount 'defun)
    (did-mount 'defun)
    (will-unmount 'defun)
    (render 'defun)
    (render-state 'defun)
    (should-update 'defun)
    (will-update 'defun)
    (will-receive-props 'defun)
    (did-update 'defun)
    ;; om.next
    ;; (params 'defun)
    ;; (query 'defun)
    ;; (ident 'defun)
    ;; (componentWillReceiveProps 'defun)
    ;; (componentDidMount 'defun)
    ;; (componentWillUnmount 'defun)
    ))

(use-package clojure-mode-extra-font-locking
  :defer t)

(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]node_modules$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]target$")
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (keys :prefix "SPC l"
    "R" 'lsp-rename
    "r" 'lsp-find-references)
  (setq lsp-keymap-prefix "SPC l"
        lsp-enable-indentation nil
        lsp-completion-enable nil
        lsp-enable-symbol-highlighting nil
        lsp-eldoc-enable-hover nil
        lsp-completion-provider :capf
        lsp-diagnostics-provider :none))

(use-package lsp-ui
  :ensure t)

(use-package cider
  :pin melpa-stable
  :defer t
  :config
  (setq cider-ns-refresh-before-fn "reloaded.repl/suspend"
        cider-ns-refresh-after-fn "reloaded.repl/resume")

  (setq cider-prompt-for-symbol nil
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
    "c" 'cider-repl-switch-to-other
    "d" 'cider-doc-map
    "eb" 'cider-load-buffer
    "ef" 'cider-eval-defun-at-point
    "ef" 'cider-load-file
    "el" 'cider-eval-last-sexp
    "er" 'cider-eval-last-sexp-and-replace
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

  (advice-add 'evil-search-highlight-persist-remove-all :after #'cider--remove-result-overlay))

(use-package clj-refactor
  :pin melpa-stable
  :defer t
  :init
  (setq cljr-auto-sort-ns t
        cljr-favor-prefix-notation nil
        cljr-favor-private-functions nil
        cljr-project-clean-prompt nil
        cljr-clojure-test-declaration
        "[clojure.test :refer [deftest is testing]]")

  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
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
  (add-to-list 'cljr-magic-require-namespaces '("subs" . "bm.client.subs"))
  (add-to-list 'cljr-magic-require-namespaces '("events" . "bm.client.events")))

(provide 'init-clojure)
