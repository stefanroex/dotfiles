;;;  -*- lexical-binding: t -*-

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(defun startup/reset-gc ()
  (setq gc-cons-threshold 100000000 ; 100mb
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/reset-gc)

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Make Emacs Native-compile .elc files asynchronously
(setq native-comp-jit-compilation t)

;; Disable certain byte compiler warnings to cut down on the noise.
;; This is a personal choice and can be removed if you would like to see
;; any and all byte compiler warnings.
(setq byte-compile-warnings '(not obsolete free-vars unresolved noruntime lexical make-local))

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; `file-name-handler-alist' is consulted on each `require', `load' and
;; various path/io functions. You get a minor speed up by unsetting this.
;; Some warning, however: this could cause problems on builds of Emacs where
;; its site lisp files aren't byte-compiled and we're forced to load the
;; *.el.gz files (e.g. on Alpine).
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)

;; Premature redisplays can substantially affect startup times and produce
;; ugly flashes of unstyled Emacs.
(setq-default inhibit-redisplay t
              inhibit-message t)

(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; Site files tend to use `load-file', which emits "Loading X..." messages in
;; the echo area, which in turn triggers a redisplay. Redisplays can have a
;; substantial effect on startup times and in this case happens so early that
;; Emacs may flash white while starting up.
;; Undo our `load-file' advice above, to limit the scope of any edge cases it
;; may introduce down the road.
(define-advice load-file (:override (file) silence)
               (load file nil 'nomessage))

(define-advice startup--load-user-init-file (:before (&rest _) init-doom)
  (advice-remove #'load-file #'load-file@silence))

;; Removes the titlebar by setting the titlebar to transparent and
;; removing never displaying an icon or title in the title-bar.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
(setq frame-resize-pixelwise t)

;; Remove menu, toolbars etc
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; Only use left fringe
(fringe-mode '(8 . 0))
