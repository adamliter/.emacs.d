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
(use-package hydra
  :ensure t)
(use-package use-package-hydra
  :ensure t)
(defun package-from-archive (f &rest args)
  (and (apply f args)
       (assq (car args) package-alist)))

(advice-add 'package-installed-p :around #'package-from-archive)
(use-package org
  :ensure t
  :after hydra
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
          ;; Referral links
          ("linode-ref"                  . "https://www.linode.com/?r=54ae7f8d79dc2dcea5d7778008242b6be864a8cf")
          ("fastmail-ref"                . "https://www.fastmail.com/?STKI=15818913")
          ;; Emacs packages
          ("use-package"                 . "https://github.com/jwiegley/use-package")
          ("org"                         . "http://orgmode.org/")
          ("fci"                         . "https://github.com/alpaker/Fill-Column-Indicator")
          ("markdown-mode"               . "http://jblevins.org/projects/markdown-mode/")
          ("exec-path-from-shell"        . "https://github.com/purcell/exec-path-from-shell")
          ("yasnippet"                   . "http://joaotavora.github.io/yasnippet/")
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
    ;; org-agenda-mode-map does not exist until org-agenda is loaded,
    ;; so this must be wrapped in a call to eval-after-load, rather than
    ;; defined with use-package's :bind and :map keywords
    (eval-after-load "org-agenda"
      '(progn
         (define-key org-agenda-mode-map "v" 'hydra-org-agenda/body)))
    :pin org
    :hydra (hydra-org-agenda
            (:pre
             (setq which-key-inhibit t)
             :post
             (setq which-key-inhibit nil)
             :hint nil)
            "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
            ;; Entry
            ("hA" org-agenda-archive-default)
            ("hk" org-agenda-kill)
            ("hp" org-agenda-priority)
            ("hr" org-agenda-refile)
            ("h:" org-agenda-set-tags)
            ("ht" org-agenda-todo)
            ;; Visit entry
            ("o"   link-hint-open-link :exit t)
            ("<tab>" org-agenda-goto :exit t)
            ("TAB" org-agenda-goto :exit t)
            ("SPC" org-agenda-show-and-scroll-up)
            ("RET" org-agenda-switch-to :exit t)
            ;; Date
            ("dt" org-agenda-date-prompt)
            ("dd" org-agenda-deadline)
            ("+" org-agenda-do-date-later)
            ("-" org-agenda-do-date-earlier)
            ("ds" org-agenda-schedule)
            ;; View
            ("vd" org-agenda-day-view)
            ("vw" org-agenda-week-view)
            ("vt" org-agenda-fortnight-view)
            ("vm" org-agenda-month-view)
            ("vy" org-agenda-year-view)
            ("vn" org-agenda-later)
            ("vp" org-agenda-earlier)
            ("vr" org-agenda-reset-view)
            ;; Toggle mode
            ("ta" org-agenda-archives-mode)
            ("tA" (org-agenda-archives-mode 'files))
            ("tr" org-agenda-clockreport-mode)
            ("tf" org-agenda-follow-mode)
            ("tl" org-agenda-log-mode)
            ("td" org-agenda-toggle-diary)
            ;; Filter
            ("fc" org-agenda-filter-by-category)
            ("fx" org-agenda-filter-by-regexp)
            ("ft" org-agenda-filter-by-tag)
            ("fr" org-agenda-filter-by-tag-refine)
            ("fh" org-agenda-filter-by-top-headline)
            ("fd" org-agenda-filter-remove-all)
            ;; Clock
            ("cq" org-agenda-clock-cancel)
            ("cj" org-agenda-clock-goto :exit t)
            ("ci" org-agenda-clock-in :exit t)
            ("co" org-agenda-clock-out)
            ;; Other
            ("q" nil :exit t)
            ("gd" org-agenda-goto-date)
            ("." org-agenda-goto-today)
            ("gr" org-agenda-redo)))
(advice-remove 'package-installed-p #'package-from-archive)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))
(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode))
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq git-commit-summary-max-length 50)
  (setq git-commit-fill-column 72)
  (setq magit-log-arguments (quote ("--graph"
                                    "--decorate"
                                    "--color")))
  (setq magit-commit-arguments (quote ("--gpg-sign=98723A2089026CD6")))
  (setq magit-repository-directories
        '(("~/projects" . 3)
          ("~/config-files" . 1)
          ("~/Dropbox/linguistics" . 1))))
(use-package projectile
  :ensure t
  :pin melpa-stable
  :bind (("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/projects/")))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))
(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
(use-package ibuffer
  :ensure t
  :bind (([remap list-buffers] . ibuffer))
  :config
  (setq ibuffer-default-sorting-mode 'major-mode)
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process))))
(use-package ibuffer-vc
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))
(use-package ace-window
  :ensure t
  :bind (([remap other-window] . ace-window))
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  :pin melpa)
(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume))
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t))
(use-package counsel
  :ensure t
  :config
  (counsel-mode t))
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GEM_HOME" "GEM_PATH"))
  (exec-path-from-shell-initialize))
(use-package multi-term
  :ensure t)
(use-package eterm-256color
  :ensure t
  :config
  (add-hook 'term-mode-hook #'eterm-256color-mode))
(use-package all-the-icons
  :ensure t)
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
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (setq LaTeX-section-label
        '(("part" . "part:")
          ("chapter" . "chap:")
          ("section" . "sec:")
          ("subsection" . "subsec:")
          "subsubsection" . "subsubsec:"))
  (add-to-list
   'TeX-command-list
   '("Arara"
     "arara %s"
     TeX-run-command
     nil                       ; ask for confirmation
     t                         ; active in all modes
     :help "Run Arara"))
  (add-to-list
   'TeX-command-list
   '("XeLaTeX"
     "xelatex --file-line-error %s"
     TeX-run-command
     nil
     t
     :help "Run XeLaTeX"))
  (add-to-list
    'TeX-command-list
    '("LuaLaTeX"
      "lualatex --file-line-error %s"
      TeX-run-command
      nil
      t
      :help "Run LuaLaTeX"))
  (add-hook 'TeX-mode-hook (lambda ()
                             (TeX-fold-mode 1))))
(use-package reftex
  :after tex
  :config
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'TeX-mode-hook 'turn-on-reftex))
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1))))
(use-package yasnippet
  :ensure t
  :demand t
  :mode
  ("\\.yasnippet\\'" . snippet-mode)
  :init
  (progn
    (add-hook 'after-save-hook
              (lambda ()
                (when (eql major-mode 'snippet-mode)
                  (yas-reload-all)))))
  :config
  (yas-global-mode t))
(setq initial-major-mode 'text-mode)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq custom-file (make-temp-file "emacs-custom"))
(setq inhibit-splash-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq initial-scratch-message nil)
(set-fringe-mode '(0 . 0))
(when
    (and
       (>= emacs-major-version 26)
       (eq system-type 'darwin))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil))
(when (>= emacs-major-version 26)
  (global-display-line-numbers-mode))
(setq column-number-mode t)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
