;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adam Liter's Emacs initialization file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the Emacs pacakges that I want
;; This will also load any options I have
;; set for these packages

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/my-loadpackages.el")

;; No home screen upon opening Emacs
(setq inhibit-splash-screen t)

;; This will display the tool bar iff the argument is positive
(tool-bar-mode 0)

;; Maximize Emacs on startup
;; see http://stackoverflow.com/a/7765654/2571049
(when window-system
  (let (
        (px (display-pixel-width))
        (py (display-pixel-height))
        (fx (frame-char-width))
        (fy (frame-char-height))
        tx ty
        )
    ;; Next formulas discovered empiric on Windows host with default font.
    (setq tx (- (/ px fx) 7))
    (setq ty (- (/ py fy) 4))
    (setq initial-frame-alist '((top . 2) (left . 2)))
    (add-to-list 'initial-frame-alist (cons 'width tx))
    (add-to-list 'initial-frame-alist (cons 'height ty))
    ))

;; And split the window horizontally on startup
;; (split-window-right)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Kill the *ESS* buffer on startup
(kill-buffer "*ESS*")

;; Remove the message from *scratch*
(setq initial-scratch-message "")

;; Transparent background and dark color theme
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))
(load-theme 'manoj-dark t)

;; Line numbers in every buffer
(global-linum-mode 1)
(setq linum-format "%4d \u2502 ")

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fill-column-indicator scss-mode password-store markdown-mode magit exec-path-from-shell ess auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
