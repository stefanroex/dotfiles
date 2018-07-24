;;;  -*- lexical-binding: t -*-
(require 'subr-x)

(use-package clojure-mode
  :defer t
  :config
  (put-clojure-indent 'assoc 1)
  (put-clojure-indent 'try-let 'defun)

  (define-clojure-indent
    (GET 'defun)
    (POST 'defun)
    (PATCH 'defun)
    (DELETE 'defun)

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
  (defun re-frame-jump-to-reg ()
    (interactive)
    (let* ((kw (cider-symbol-at-point 'look-back))
           (ns-qualifier (and
                          (string-match "^:+\\(.+\\)/.+$" kw)
                          (match-string 1 kw)))
           (kw-ns (if ns-qualifier
                      (cider-resolve-alias (cider-current-ns) ns-qualifier)
                    (cider-current-ns)))
           (kw-to-find (concat "::" (replace-regexp-in-string "^:+\\(.+/\\)?" "" kw))))

      (when (and ns-qualifier (string= kw-ns (cider-current-ns)))
        (error "Could not resolve alias \"%s\" in %s" ns-qualifier (cider-current-ns)))

      (progn (cider-find-ns "-" kw-ns)
             (search-forward-regexp (concat "reg-[a-zA-Z-]*[ \\\n]+" kw-to-find) nil 'noerror))))

  (defun my-cider-refresh ()
    (interactive)
    (cider-ensure-connected)
    (cider-save-project-buffers)
    (let ((log-buffer (or (get-buffer cider-refresh-log-buffer)
                          (cider-make-popup-buffer cider-refresh-log-buffer))))
      (cider-interactive-eval
       "(dev/reset)"
       (cider-refresh-eval))))

  (defun cider-switch-to-clj-repl ()
    (interactive)
    (cider-ensure-connected)
    (cider-eval- ":cljs/quit"))
  (defun cider-switch-to-cljs-repl ()
    (interactive)
    (cider-ensure-connected)
    (cider-interactive-eval "(cljs)"))

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
        "gF" 're-frame-jump-to-reg)

  (keys-l :keymaps '(clojurescript-mode-map)
          "C" 'cider-switch-to-cljs-repl)

  (keys-l :keymaps '(clojure-mode-map)
          "C" 'cider-switch-to-clj-repl)

  (keys :keymaps cider-mode-maps
        :prefix "SPC c"
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
        "sn" 'cider-repl-set-ns
        "sr" 'cider-switch-to-repl-buffer
        "l" 'cider-inspect-last-result
        "t" 'cider-test-run-project-tests)

  (advice-add 'evil-search-highlight-persist-remove-all :after #'cider--remove-result-overlay))

(use-package clj-refactor
  :defer t
  :init
  (setq cljr-auto-sort-ns t
        cljr-favor-prefix-notation nil
        cljr-favor-private-functions nil
        cljr-clojure-test-declaration
        "[clojure.test :refer :all]")

  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  :config
  (dolist (details cljr--all-helpers)
    (let ((key (car details))
          (fn (cadr details)))
      (keys :prefix "SPC r"
            :keymaps '(clojure-mode-map clojurescript-mode-map)
            key fn)))
  (add-to-list 'cljr-magic-require-namespaces '("r"  . "reagent.core"))
  (add-to-list 'cljr-magic-require-namespaces '("rf" . "re-frame.core"))
  (add-to-list 'cljr-magic-require-namespaces '("subs" . "bm.client.subs"))
  (add-to-list 'cljr-magic-require-namespaces '("events" . "bm.client.events")))

(provide 'init-clojure)
