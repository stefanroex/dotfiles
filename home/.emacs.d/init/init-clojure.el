(use-package clojure-mode
  :defer t)

(use-package clojure-mode-extra-font-locking
  :defer t)

(use-package cider
  :defer t
  :init
  (setq cider-show-error-buffer t
        cider-prompt-for-symbol nil
        cider-repl-display-help-banner nil
        cider-eval-result-duration nil
        cider-auto-select-error-buffer t
        cider-request-dispatch 'static
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t)

  :config
  (defvar cider-mode-maps
    '(cider-repl-mode-map
      clojure-mode-map
      clojurescript-mode-map))

  (keys :keymaps cider-mode-maps
        "gf" 'cider-find-var
        "\\" 'cider-eval-defun-at-point)

  (keys-l :keymaps cider-mode-maps
          "E" 'cider-pprint-eval-last-sexp
          "T" 'cider-test-run-test
          "d" 'cider-doc
          "D" 'cider-grimoire-web
          "e" 'cider-eval-last-sexp)

  (keys :keymaps cider-mode-maps
        :prefix ",c"
        "I" 'cider-display-connection-info
        "J" 'cider-jack-in-clojurescript
        "R" 'cider-restart
        "C" 'cider-connection-browser
        "d" 'cider-doc-map
        "eb" 'cider-load-buffer
        "ef" 'cider-eval-defun-at-point
        "ef" 'cider-load-file
        "el" 'cider-eval-last-sexp
        "er" 'cider-eval-last-sexp-and-replace
        "i" 'cider-inspect
        "j" 'cider-jack-in
        "m" 'cider-macroexpand-1
        "pf" 'cider-pprint-eval-defun-at-point
        "pl" 'cider-pprint-eval-last-sexp
        "q" 'cider-quit
        "r" 'cider-refresh
        "sc" 'cider-rotate-default-connection
        "sn" 'cider-repl-set-ns
        "sr" 'cider-switch-to-repl-buffer
        "t" 'cider-test-run-tests)

  (add-to-list 'evil-emacs-state-modes 'cider-connections-buffer-mode)
  (evil-add-hjkl-bindings cider-connections-buffer-mode-map 'emacs)

  (advice-add 'evil-search-highlight-persist-remove-all :after #'cider--remove-result-overlay))

(use-package clj-refactor
  :defer t
  :init
  (setq cljr-auto-sort-ns t
        cljr-favor-prefix-notation nil
        cljr-favor-private-functions nil
        cljr-clojure-test-declaration
        "[cljs.test :refer-macros [deftest is]]")
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  :config
  (dolist (details cljr--all-helpers)
    (let ((key (car details))
          (fn (cadr details)))
      (keys :prefix ",r"
            :keymaps '(clojure-mode-map clojurescript-mode-map)
            key fn)))
  (add-to-list 'cljr-magic-require-namespaces
               '("reagent"  . "reagent.core")))

(use-package kibit-helper)

(provide 'init-clojure)
