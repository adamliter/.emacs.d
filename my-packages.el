;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; `my-packages.el`
;; to be loaded by
;; `my-loadpackages.el`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Need Common Lisp in order
;; for the method definied below
;; for ensuring `my-required-packages`
;; are installed at launch to work
(require 'cl)

(require 'package)
;; For using MELPA as a package archive
;; This needs to come before the call to `package-initialize`
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; For old Emacs versions
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; And now we have the
;; call to `package-initialzie`
(package-initialize)

;; Now let's create a list of
;; required packages that I always
;; want in my Emacs environment
(defvar my-required-packages
  '(
    fill-column-indicator ; for column rule
    magit ; implements some `git` functionality in Emacs
    auctex ; for editing LaTeX
    ess ; for editing R
    scss-mode ; for editing Sass
    markdown-mode ; for editing Markdown
    exec-path-from-shell ; provides ability to get executable paths from shell in OS X
    password-store ; Emacs interface for `pass` (http://www.passwordstore.org/)
    ) "My list of packages that I wish to ensure are always present at launch.")

;; Define a method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in my-required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; If not all of the packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p my-required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; end `my-packages.el`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
