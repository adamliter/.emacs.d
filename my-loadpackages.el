;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; Set file extensions for Markdown mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Get $MANPATH, $PATH, and exec-path from
;; the shell (but only in OS X)
;; Requires the `exec-path-from-shell` packages
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Require `magit`
;; and bind `magit-status`
;; to `C-c m`
(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)

;; Require `ess`
(require 'ess)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; end `my-loadpackages.el`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
