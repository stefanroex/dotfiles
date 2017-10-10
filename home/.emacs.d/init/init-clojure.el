;;;  -*- lexical-binding: t -*-
(require 'subr-x)

(use-package clojure-mode
  :defer t
  :config
  (define-clojure-indent
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
    (params 'defun)
    (query 'defun)
    (ident 'defun)
    (componentWillReceiveProps 'defun)
    (componentDidMount 'defun)
    (componentWillUnmount 'defun)))

(use-package clojure-mode-extra-font-locking
  :defer t)

(use-package cider
  :defer t
  :config
  (defun cider-refresh-eval ()
    (nrepl-make-response-handler (current-buffer)
                                 (lambda (_buffer value)
                                   (message "refresh: %s"
                                            (string-trim
                                             (cider-font-lock-as-clojure value))))
                                 (lambda (_buffer out)
                                   (message "refresh: %s"

                                            (string-trim
                                             (cider-font-lock-as-clojure out))))
                                 (lambda (buffer err)
                                   (cider-handle-compilation-errors err buffer))
                                 '()))

  (defun my-cider-refresh ()
    (interactive)
    (cider-ensure-connected)
    (cider-save-project-buffers)
    (let ((log-buffer (or (get-buffer cider-refresh-log-buffer)
                          (cider-make-popup-buffer cider-refresh-log-buffer))))
      (cider-interactive-eval
       "(dev/reset)"
       (cider-refresh-eval))))

  (setq cider-prompt-for-symbol nil
        cider-repl-display-help-banner nil
        cider-eval-result-duration nil
        cider-repl-use-pretty-printing t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t)

  (defvar cider-mode-maps
    '(cider-repl-mode-map
      clojure-mode-map
      clojurescript-mode-map))

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
        :prefix ",c"
        "C" 'cider-connection-browser
        "d" 'cider-doc-map
        "eb" 'cider-load-buffer
        "ef" 'cider-eval-defun-at-point
        "ef" 'cider-load-file
        "el" 'cider-eval-last-sexp
        "er" 'cider-eval-last-sexp-and-replace
        "i" 'cider-inspect
        "j" 'cider-jack-in
        "J" 'cider-restart
        "m" 'cider-macroexpand-1
        "pf" 'cider-pprint-eval-defun-at-point
        "pl" 'cider-pprint-eval-last-sexp
        "q" 'cider-quit
        "r" 'my-cider-refresh
        "R" 'cider-refresh
        "sc" 'cider-rotate-default-connection
        "sn" 'cider-repl-set-ns
        "sr" 'cider-switch-to-repl-buffer
        "t" 'cider-test-run-project-tests)

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

(provide 'init-clojure)
