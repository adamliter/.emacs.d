;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; my-loadpackages.el
;; to be loaded by
;; `init.el`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load `my-packages.el`
;; This creates a list of packages
;; to be installed at runtime if
;; not already installed
(load "~/.emacs.d/my-packages.el")

;; Require `magit`
;; and bind `magit-status`
;; to `C-c m`
(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)

;; Require `ess`
(require 'ess)
