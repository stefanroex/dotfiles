(use-package elm-mode
  :init
  (setq elm-format-on-save t)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm)
  (keys :states nil
        "M-n" 'elm-indent-cycle))

(provide 'init-elm)
