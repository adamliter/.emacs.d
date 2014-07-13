;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adam Liter's Emacs initialization file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the Emacs pacakges that I want
;; This will also load any options I have
;; set for these packages
(load "~/.emacs.d/my-loadpackages.el")

;; Transparent background and dark color theme
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))
(load-theme 'manoj-dark t)

;; Line numbers in every buffer
(global-linum-mode 1)

;; Matching parentheses
(show-paren-mode 1)

;; IDO mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;; List of files for global todo list
(setq org-agenda-files (list "~/.emacs.d/org/Misc.org"
			     "~/.emacs.d/org/Work.org"
			     )
      )

;; Set major modes for some file extensions
(setq auto-mode-alist
      (append
       ;; Markdown
       '(("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
       ;; R
	 ("\\.R\\'" . R-mode)
	 ("\\.r\\'" . R-mode))
       auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; end `init.el`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
