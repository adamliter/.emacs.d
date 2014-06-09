;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adam Liter's Emacs initialization file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the Emacs pacakges that I want
(load "~/.emacs.d/my-loadpackages.el")

;; Transparent background and dark color theme
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))
(load-theme 'manoj-dark t)

;; Get $MANPATH, $PATH, and exec-path from
;; the shell (but only in OS X)
;; Requires the `exec-path-from-shell` packages
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Make default compiler for LaTeX editing `pdflatex`
(setq TeX-PDF-mode t)

;; Open documents compiled with LaTeX in Skim
;; with SyncTeX enabled
;; See http://tex.stackexchange.com/questions/11613/launching-an-external-pdf-viewer-from-emacs-auctex-on-a-mac-osx-fails
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq TeX-source-correlate-method 'synctex)

(add-hook 'LaTeX-mode-hook
      (lambda()
        (add-to-list 'TeX-expand-list
             '("%q" skim-make-url))))

(defun skim-make-url () (concat
        (TeX-current-line)
        " "
        (expand-file-name (funcall file (TeX-output-extension) t)
            (file-name-directory (TeX-master-file)))
        " "
        (buffer-file-name)))

(setq TeX-view-program-list
      '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))

(setq TeX-view-program-selection '((output-pdf "Skim")))

;; Line numbers in every buffer
(global-linum-mode 1)

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

;; end `init.el`
