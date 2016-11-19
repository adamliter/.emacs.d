;; -*- mode: elisp; fill-column: 72 -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; `my-loadpackages.el`
;; to be loaded by
;; `init.el`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load `my-packages.el`
;; This creates a list of packages
;; to be installed at runtime if
;; not already installed
(load "~/.emacs.d/my-packages.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Package options, bindings, settings, etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AucTeX options
(load "~/.emacs.d/my-auctex-options.el")

;; Get $MANPATH, $PATH, and exec-path from
;; the shell (but only in OS X)
;; Requires the `exec-path-from-shell` packages
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Fill-Column-Indicator
(require 'fill-column-indicator)
(setq-default fill-column 72)
;; Enable fci-mode by default
(define-globalized-minor-mode
 global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)

;; Magit
(require 'magit)
;; Bind `magit-status` to `C-c m`
(define-key global-map (kbd "C-c m") 'magit-status)
;; Prevent warning message from version 1.4.0
;; from showing on startup
(setq magit-last-seen-setup-instructions "1.4.0")

;; Require `ess`
;; and other
;; `ess` options
(require 'ess-site)
(setq ess-ask-for-ess-directory nil)
; Fix ggplot indentation problem
(add-hook 'ess-mode-hook
          (lambda ()
            (setq ess-first-continued-statement-offset 2)
            (setq ess-continued-statement-offset 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; end `my-loadpackages.el`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
