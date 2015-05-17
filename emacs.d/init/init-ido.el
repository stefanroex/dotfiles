(provide 'init-ido)

;; Ido fuzzy matching
(flx-ido-mode 1)
(setq flx-ido-threshhold 1000)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-ignore-buffers
      '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*" "^\*"))
