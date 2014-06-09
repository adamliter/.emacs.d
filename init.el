;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adam Liter's Emacs initialization file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the Emacs pacakges that I want
(load "~/.emacs.d/my-loadpackages.el")

;; Line numbers in every buffer
(global-linum-mode 1)

;; IDO mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1) 

;; end `init.el`
