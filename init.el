(unless (>= emacs-major-version 24)
  (error "Emacs version 24 or higher is required"))
(message "Loading ~/.emacs.d/init.el")
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives
        '(("ELPA" . "http://tromey.com/elpa/")
          ("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "http://melpa.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")
          ;("marmalade" . "http://marmalade-repo.org/packages/")
          ("org" . "http://orgmode.org/elpa/")
          ("elpy" . "http://jorgenschaefer.github.io/packages/")
          ))

  ;; Check if we're on Emacs 24.4 or newer, if so, use the pinned package feature
  (when (boundp 'package-pinned-packages)
    (setq package-pinned-packages
          '((org . "org")
            (magit . "melpa-stable")
            (markdown-mode . "melpa-stable")
            )))

  (package-initialize))
(setq package-archive-priorities
      '(("org" . 30)
        ("elpy" . 30)
        ("melpa-stable" . 20)
        ;("marmalade" . 10)
        ("gnu" . 10)
        ("melpa" . 5)))

(setq package-menu-hide-low-priority t)
(unless (package-installed-p 'use-package)
  (message "** bootstrapping the installation of use-package")
  (package-refresh-contents)
  (package-install 'use-package)
  (message "** successfully installed use-package"))
(defun package-from-archive (f &rest args)
  (and (apply f args)
       (assq (car args) package-alist)))

(advice-add 'package-installed-p :around #'package-from-archive)
(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c C-x C-o" . org-clock-out))
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-link-abbrev-alist
        '(;; General stuff on the internet
          ("google"                      . "http://www.google.com/search?q=")
          ("startpage"                   . "https://www.startpage.com/do/search?query=")
          ("tex-sx-search"               . "https://tex.stackexchange.com/search?q=")
          ("gh-gitignore"                . "https://github.com/github/gitignore")
          ("gh-gitignore-mac"            . "https://github.com/github/gitignore/blob/master/Global/macOS.gitignore")
          ("gh-gitignore-linux"          . "https://github.com/github/gitignore/blob/master/Global/Linux.gitignore")
          ("gh-gitignore-windows"        . "https://github.com/github/gitignore/blob/master/Global/Windows.gitignore")
          ("gh-gitignore-emacs"          . "https://github.com/github/gitignore/blob/master/Global/Emacs.gitignore")
          ("gh-gitignore-tex"            . "https://github.com/github/gitignore/blob/master/TeX.gitignore")
          ("gh-gitignore-python"         . "https://github.com/github/gitignore/blob/master/Python.gitignore")
          ("gh-gitignore-r"              . "https://github.com/github/gitignore/blob/master/R.gitignore")
          ;; Personal stuff on the internet
          ("adamliter-web"               . "https://www.adamliter.org")
          ("adamlitercv"                 . "https://www.adamliter.org/content/adamlitercv.pdf")
          ("atomicwriting"               . "https://www.atomicwriting.com")
          ("adamliter-github"            . "https://github.com/adamliter")
          ("config-files"                . "https://github.com/adamliter/config-files")
          ("emacs-d"                     . "https://github.com/adamliter/emacs.d")
          ("adamliter-keybase"           . "https://keybase.io/adamliter")
          ("adamliter-se"                . "https://stackexchange.com/users/2978319/adam-liter?tab=accounts")
          ("adamliter-tex-sx"            . "https://tex.stackexchange.com/users/32888/adam-liter")
          ("adamliter-twitter"           . "https://twitter.com/adam_liter")
          ("latex-workshop"              . "http://bit.ly/latex-workshop")
          ;; Emacs packages
          ("use-package"                 . "https://github.com/jwiegley/use-package")
          ("org"                         . "http://orgmode.org/")
          ("fci"                         . "https://github.com/alpaker/Fill-Column-Indicator")
          ("markdown-mode"               . "http://jblevins.org/projects/markdown-mode/")
          ("exec-path-from-shell"        . "https://github.com/purcell/exec-path-from-shell")
          ("magit"                       . "https://magit.vc/")
          ;; General tech stuff
          ("tmux-macos-pasteboard"       . "https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard")))
    (setq org-directory "~/org")
    (setq org-agenda-files '("~/org"))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "FEEDBACK(f@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
    (setq org-log-into-drawer t)
    (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
    (setq org-default-notes-file (concat org-directory "/refile.org"))
    :pin org)
(advice-remove 'package-installed-p #'package-from-archive)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))
(use-package fill-column-indicator
  :ensure t
  :config
  (setq-default fill-column 72)
  (add-hook 'markdown-mode-hook 'fci-mode))
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mdown\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))
(setq inhibit-splash-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq initial-scratch-message nil)
(load-theme 'manoj-dark t)
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))
(when window-system
  (let (
        (px (display-pixel-width))
        (py (display-pixel-height))
        (fx (frame-char-width))
        (fy (frame-char-height))
        tx ty
        )
    (setq tx (- (/ px fx) 7))
    (setq ty (- (/ py fy) 4))
    (setq initial-frame-alist '((top . 2) (left . 2)))
    (add-to-list 'initial-frame-alist (cons 'width tx))
    (add-to-list 'initial-frame-alist (cons 'height ty))))
(global-linum-mode 1)
(setq linum-format "%4d \u2502")
(setq column-number-mode t)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq delete-trailing-lines nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
