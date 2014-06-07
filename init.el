;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adam Liter's Emacs initialization file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
;; For using MELPA as a package archive
;; This needs to come before the call to `package-initialize`
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
    magit ; implements some git functionality in Emacs
    auctex ; for editing LaTeX
    ) "My list of packages that I wish to ensure are always present at launch.")
    


;; end init.el
