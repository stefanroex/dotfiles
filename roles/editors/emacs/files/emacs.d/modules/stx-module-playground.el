;;;  -*- lexical-binding: t -*-

(use-package lua-mode)

(use-package iflipb
  :custom
  (iflipb-wrap-around t)

  :general
  (:states '(normal emacs motion)
   "SPC z" 'iflipb-previous-buffer
   "SPC x" 'iflipb-next-buffer))

(provide 'stx-module-playground)
