(unless (>= emacs-major-version 24)
  (error "Emacs version 24 or higher is required"))
(message "Loading ~/.emacs.d/init.el")
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "http://melpa.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")
          ("org" . "http://orgmode.org/elpa/")))
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
         ("C-c b" . org-switchb)
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
    :hydra
    (hydra-org-agenda
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
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (load-theme 'doom-one t))
(setq adamliter/themes '(doom-one-light doom-one))
(setq adamliter/themes-index 0)

(defun adamliter/cycle-theme ()
  (interactive)
  (setq adamliter/themes-index (% (1+ adamliter/themes-index) (length adamliter/themes)))
  (adamliter/load-indexed-theme))

(defun adamliter/load-indexed-theme ()
  (adamliter/try-load-theme (nth adamliter/themes-index adamliter/themes)))

(defun adamliter/try-load-theme (theme)
  (if (ignore-errors (load-theme theme :no-confirm))
      (mapcar #'disable-theme (remove theme custom-enabled-themes))
    (message "Unable to find theme file for ‘%s’" theme)))

(adamliter/load-indexed-theme)
(global-set-key (kbd "C-c M-t") 'adamliter/cycle-theme)
(use-package doom-modeline
  :ensure t
  :pin melpa-stable
  :hook
  (after-init . doom-modeline-mode))
(use-package company
  :ensure t
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (add-hook 'after-init-hook 'global-company-mode))
(use-package magit
  :ensure t
  :pin melpa
  :bind (("C-x g" . magit-status))
  :config
  (setq git-commit-summary-max-length 50)
  (setq git-commit-fill-column 72)
  (setq magit-repository-directories
        '(("~/projects" . 3)
          ("~/config-files" . 1)
          ("~/Dropbox/linguistics" . 1))))
(use-package forge
  :ensure t
  :pin melpa
  :after magit
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))
(use-package projectile
  :ensure t
  :pin melpa-stable
  :after hydra
  :bind (("C-c p" . hydra-projectile/body))
  :config
  (projectile-mode +1)
  (setq projectile-require-project-root nil)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/projects/" "~/Dropbox/Research"))
  (defun adamliter/projectile-switch-project-magit-status ()
    "Switch to other project and open Magit status there."
    (interactive)
    (let ((projectile-switch-project-action #'magit-status))
      (call-interactively #'projectile-switch-project)))
  (defun adamliter/kill-non-project-buffers (&optional kill-special)
    "Kill buffers that do not belong to a `projectile' project.
With prefix argument (`C-u'), also kill the special buffers."
    (interactive "P")
    (let ((bufs (buffer-list (selected-frame))))
      (dolist (buf bufs)
        (with-current-buffer buf
          (let ((buf-name (buffer-name buf)))
            (when (or (null (projectile-project-p))
                      (and kill-special
                           (string-match "^\*" buf-name)))
              ;; Preserve buffers with names starting with *scratch or *Messages
              (unless (string-match "^\\*\\(\\scratch\\|Messages\\)" buf-name)
                (message "Killing buffer %s" buf-name)
                (kill-buffer buf))))))))
  :hydra
  (hydra-projectile-other-window
   (:pre
        (setq which-key-inhibit t)
    :post
    (setq which-key-inhibit nil)
    :color teal
    :hint nil)
   "projectile-other-window"
   ("b" projectile-switch-to-buffer-other-window "buffer")
   ("D" projectile-find-dir-other-window "dir")
   ("f" projectile-find-file-other-window "file")
   ("F" projectile-find-file-dwim-other-window "file dwim")
   ("q" nil "cancel" :color blue))
  (hydra-projectile
   (:pre
    (setq which-key-inhibit t)
    :post
    (setq which-key-inhibit nil)
    :color teal
    :hint nil)
   "
     PROJECTILE: %(if (fboundp 'projectile-project-root) (projectile-project-root) \"TBD\")
^^^^       Find               ^^   Search            ^^^^       Buffers               ^^   Cache                     ^^^^       Other
^^^^--------------------------^^---------------------^^^^-----------------------------^^------------------------------------------------------------------
_f_/_s-f_: file               _a_: ag                ^^    _i_: Ibuffer               _c_: cache clear               ^^    _E_: edit project's .dir-locals.el
^^    _F_: file dwim          _R_: rg                ^^    _b_: switch to buffer      _x_: remove known project      _s-p_/_p_: switch to other project
^^    _d_: file curr dir      _o_: multi-occur       _K_/_s-k_: kill all buffers      _X_: cleanup non-existing      ^^    _g_: switch to Magit status of other project
^^    _r_: recent file        ^^                     ^^^^                             _z_: cache current             ^^    _P_: switch to an open project
^^    ^^                      ^^                     ^^^^                             ^^                             ^^    _D_: find dir
"
   ("a"   counsel-projectile-ag)
   ("b"   counsel-projectile-switch-to-buffer)
   ("c"   projectile-invalidate-cache)
   ("d"   projectile-find-file-in-directory)
   ("f"   counsel-projectile-find-file)
   ("s-f" counsel-projectile-find-file)
   ("F"   counsel-projectile-find-file-dwim)
   ("D"   counsel-projectile-find-dir)
   ("E"   projectile-edit-dir-locals)
   ("g"   adamliter/projectile-switch-project-magit-status)
   ("i"   projectile-ibuffer)
   ("K"   projectile-kill-buffers)
   ("s-k" projectile-kill-buffers)
   ("m"   projectile-multi-occur)
   ("o"   projectile-multi-occur)
   ("p"   counsel-projectile-switch-project)
   ("s-p" counsel-projectile-switch-project)
   ("P"   projectile-switch-open-project)
   ("s"   counsel-projectile-switch-project)
   ("r"   projectile-recentf)
   ("R"   counsel-projectile-rg)
   ("x"   projectile-remove-known-project)
   ("X"   projectile-cleanup-known-projects)
   ("z"   projectile-cache-current-file)
   ("4"   hydra-projectile-other-window/body "other window")
   ("q" nil "cancel" :color blue)))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))
(use-package treemacs
  :ensure t
  :pin melpa
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

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
        ("." . hydra-dired/body))
  :after hydra
  :hydra
  (hydra-dired
   (:pre
    (setq which-key-inhibit t)
    :post
    (setq which-key-inhibit nil)
    :hint nil)
   "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
   ("\\" dired-do-ispell)
   ("(" dired-hide-details-mode)
   (")" dired-omit-mode)
   ("+" dired-create-directory)
   ("=" diredp-ediff)         ;; smart diff
   ("?" dired-summary)
   ("$" diredp-hide-subdir-nomove)
   ("A" dired-do-find-regexp)
   ("C" dired-do-copy)        ;; Copy all marked files
   ("D" dired-do-delete)
   ("E" dired-mark-extension)
   ("e" dired-ediff-files)
   ("F" dired-do-find-marked-files)
   ("G" dired-do-chgrp)
   ("g" revert-buffer)        ;; read all directories again (refresh)
   ("i" dired-maybe-insert-subdir)
   ("l" dired-do-redisplay)   ;; relist the marked or singel directory
   ("M" dired-do-chmod)
   ("m" dired-mark)
   ("O" dired-display-file)
   ("o" dired-find-file-other-window)
   ("Q" dired-do-find-regexp-and-replace)
   ("R" dired-do-rename)
   ("r" dired-do-rsynch)
   ("S" dired-do-symlink)
   ("s" dired-sort-toggle-or-edit)
   ("t" dired-toggle-marks)
   ("U" dired-unmark-all-marks)
   ("u" dired-unmark)
   ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
   ("w" dired-kill-subdir)
   ("Y" dired-do-relsymlink)
   ("z" diredp-compress-this-file)
   ("Z" dired-do-compress)
   ("q" nil)
   ("." nil :color blue)))
(use-package ibuffer
  :ensure t
  :after hydra
  :bind (([remap list-buffers] . ibuffer)
         :map ibuffer-mode-map
         ("." . hydra-ibuffer-main/body))
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
                filename-and-process)))
  (add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)
  :hydra
  (hydra-ibuffer-main
   (:pre
    (setq which-key-inhibit t)
    :post
    (setq which-key-inhibit nil)
    :hint nil)
   "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
   ("j" ibuffer-forward-line)
   ("RET" ibuffer-visit-buffer :color blue)
   ("k" ibuffer-backward-line)

   ("m" ibuffer-mark-forward)
   ("u" ibuffer-unmark-forward)
   ("*" hydra-ibuffer-mark/body :color blue)

   ("D" ibuffer-do-delete)
   ("S" ibuffer-do-save)
   ("a" hydra-ibuffer-action/body :color blue)

   ("g" ibuffer-update)
   ("s" hydra-ibuffer-sort/body :color blue)
   ("/" hydra-ibuffer-filter/body :color blue)

   ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
   ("q" quit-window "quit ibuffer" :color blue)
   ("." nil "toggle hydra" :color blue))
  (hydra-ibuffer-mark
   (:pre
    (setq which-key-inhibit t)
    :post
    (setq which-key-inhibit nil)
    :columns 5
    :after-exit (hydra-ibuffer-main/body)
    :hint nil)
   "Mark"
   ("*" ibuffer-unmark-all "unmark all")
   ("M" ibuffer-mark-by-mode "mode")
   ("m" ibuffer-mark-modified-buffers "modified")
   ("u" ibuffer-mark-unsaved-buffers "unsaved")
   ("s" ibuffer-mark-special-buffers "special")
   ("r" ibuffer-mark-read-only-buffers "read-only")
   ("/" ibuffer-mark-dired-buffers "dired")
   ("e" ibuffer-mark-dissociated-buffers "dissociated")
   ("h" ibuffer-mark-help-buffers "help")
   ("z" ibuffer-mark-compressed-file-buffers "compressed")
   ("b" hydra-ibuffer-main/body "back" :color blue))
  (hydra-ibuffer-action
   (:pre
    (setq which-key-inhibit t)
    :post
    (setq which-key-inhibit nil)
    :columns 4
    :after-exit
    (if (eq major-mode 'ibuffer-mode)
        (hydra-ibuffer-main/body))
    :hint nil)
   "Action"
   ("A" ibuffer-do-view "view")
   ("E" ibuffer-do-eval "eval")
   ("F" ibuffer-do-shell-command-file "shell-command-file")
   ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
   ("H" ibuffer-do-view-other-frame "view-other-frame")
   ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
   ("M" ibuffer-do-toggle-modified "toggle-modified")
   ("O" ibuffer-do-occur "occur")
   ("P" ibuffer-do-print "print")
   ("Q" ibuffer-do-query-replace "query-replace")
   ("R" ibuffer-do-rename-uniquely "rename-uniquely")
   ("T" ibuffer-do-toggle-read-only "toggle-read-only")
   ("U" ibuffer-do-replace-regexp "replace-regexp")
   ("V" ibuffer-do-revert "revert")
   ("W" ibuffer-do-view-and-eval "view-and-eval")
   ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
   ("b" nil "back"))
  (hydra-ibuffer-sort
   (:pre
    (setq which-key-inhibit t)
    :post
    (setq which-key-inhibit nil)
    :columns 3
    :hint nil)
   "Sort"
   ("i" ibuffer-invert-sorting "invert")
   ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
   ("v" ibuffer-do-sort-by-recency "recently used")
   ("s" ibuffer-do-sort-by-size "size")
   ("f" ibuffer-do-sort-by-filename/process "filename")
   ("m" ibuffer-do-sort-by-major-mode "mode")
   ("b" hydra-ibuffer-main/body "back" :color blue))
  (hydra-ibuffer-filter
   (:pre
    (setq which-key-inhibit t)
    :post
    (setq which-key-inhibit nil)
    :columns 4
    :hint nil)
   "Filter"
   ("m" ibuffer-filter-by-used-mode "mode")
   ("M" ibuffer-filter-by-derived-mode "derived mode")
   ("n" ibuffer-filter-by-name "name")
   ("c" ibuffer-filter-by-content "content")
   ("e" ibuffer-filter-by-predicate "predicate")
   ("f" ibuffer-filter-by-filename "filename")
   (">" ibuffer-filter-by-size-gt "size")
   ("<" ibuffer-filter-by-size-lt "size")
   ("/" ibuffer-filter-disable "disable")
   ("b" hydra-ibuffer-main/body "back" :color blue)))
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
  (setq ivy-use-selectable-prompt t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t))
(use-package counsel
  :ensure t
  :bind
  ("C-c k" . counsel-rg)
  ("C-c u" . counsel-unicode-char)
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
(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'" . csv-mode))
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
  (setq TeX-save-query nil)
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
     "arara --verbose %s"
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
  (add-to-list
   'TeX-command-list
   '("Make"
     "make"
     TeX-run-TeX
     nil
     t
     :help "Run make"))
  (defun adamliter/TeX-make ()
    "Interactive function for running GNU Make on a (La)TeX file."
    (interactive)
    (TeX-command-sequence '("Make" "View") t))
  (defun adamliter/TeX-arara ()
    "Interactive function for running Arara on a (La)TeX file."
    (interactive)
    (TeX-command-sequence '("Arara" "View") t))
  (add-hook 'TeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))
  :bind
  (:map TeX-mode-map
        ("C-c C-m" . adamliter/TeX-make)
        ("C-c M-a" . adamliter/TeX-arara)))
(use-package company-auctex
  :ensure t
  :hook
  (latex-mode . (company-auctex-init)))
(use-package company-math
  :ensure t
  :hook
  (latex-mode . (lambda ()
                  (add-to-list
                   (make-local-variable 'company-backends)
                   '(company-math-symbols-unicode))))
  (org-mode . (lambda ()
                (add-to-list
                 (make-local-variable 'company-backends)
                 '(company-math-symbols-unicode)))))
(use-package reftex
  :after tex
  :config
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'TeX-mode-hook 'turn-on-reftex))
(use-package company-reftex
  :ensure t
  :hook
  (latex-mode . (lambda ()
                  (add-to-list
                   (make-local-variable 'company-backends)
                   '(company-reftex-labels company-reftex-citations))))
  (org-mode . (lambda ()
                (add-to-list
                 (make-local-variable 'company-backends)
                 '(company-reftex-labels company-reftex-citations)))))
(use-package pdf-tools
  :ensure t
  :after hydra
  :pin melpa
  :config
  (pdf-tools-install)
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))
  (add-hook 'pdf-annot-minor-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))
  (define-key pdf-view-mode-map (kbd "\\") 'hydra-pdftools/body)
  (define-key pdf-view-mode-map (kbd "<s-spc>") 'pdf-view-scroll-down-or-next-page)
  (define-key pdf-view-mode-map (kbd "g") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "G") 'pdf-view-list-page)
  (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "h") 'image-back-hscroll)
  (define-key pdf-view-mode-map (kbd "j")  'pdf-view-next-page)
  (define-key pdf-view-mode-map (kbd "k")  'pdf-view-previous-page)
  (define-key pdf-view-mode-map (kbd "e")  'pdf-view-goto-page)
  (define-key pdf-view-mode-map (kbd "u")  'pdf-view-revert-buffer)
  (define-key pdf-view-mode-map (kbd "al") 'pdf-annot-list-annotations)
  (define-key pdf-view-mode-map (kbd "ad") 'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "aa") 'pdf-annot-attachment-dired)
  (define-key pdf-view-mode-map (kbd "am") 'pdf-annot-add-markup-annotation)
  (define-key pdf-view-mode-map (kbd "at") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "y")  'pdf-view-kill-ring-save)
  (define-key pdf-view-mode-map (kbd "i")  'pdf-misc-display-metadata)
  (define-key pdf-view-mode-map (kbd "s")  'pdf-occur)
  (define-key pdf-view-mode-map (kbd "b")  'pdf-view-set-slice-from-bounding-box)
  (define-key pdf-view-mode-map (kbd "r")  'pdf-view-reset-slice)
  :hydra
  (hydra-pdftools
   (:pre
    (setq which-key-inhibit t)
    :post
    (setq which-key-inhibit nil)
    :hint nil)
   "
                                                                   ╭───────────┐
    Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
╭──────────────────────────────────────────────────────────────────┴───────────╯
      ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
      ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
      ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
      ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
 _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
      ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
      ^^_n_^^      ^ ^  _r_eset slice box
      ^^^↓^^^
      ^^_G_^^
--------------------------------------------------------------------------------
"
   ("\\" hydra-master/body "back")
   ("<ESC>" nil "quit")
   ("al" pdf-annot-list-annotations)
   ("ad" pdf-annot-delete)
   ("aa" pdf-annot-attachment-dired)
   ("am" pdf-annot-add-markup-annotation)
   ("at" pdf-annot-add-text-annotation)
   ("y"  pdf-view-kill-ring-save)
   ("+" pdf-view-enlarge :color red)
   ("-" pdf-view-shrink :color red)
   ("0" pdf-view-scale-reset)
   ("H" pdf-view-fit-height-to-window)
   ("W" pdf-view-fit-width-to-window)
   ("P" pdf-view-fit-page-to-window)
   ("n" pdf-view-next-page-command :color red)
   ("p" pdf-view-previous-page-command :color red)
   ("d" pdf-view-dark-minor-mode)
   ("b" pdf-view-set-slice-from-bounding-box)
   ("r" pdf-view-reset-slice)
   ("g" pdf-view-first-page)
   ("G" pdf-view-last-page)
   ("e" pdf-view-goto-page)
   ("o" pdf-outline)
   ("s" pdf-occur)
   ("i" pdf-misc-display-metadata)
   ("u" pdf-view-revert-buffer)
   ("F" pdf-links-action-perfom)
   ("f" pdf-links-isearch-link)
   ("B" pdf-history-backward :color red)
   ("N" pdf-history-forward :color red)
   ("l" image-forward-hscroll :color red)
   ("h" image-backward-hscroll :color red)))
(use-package ess
  :ensure t
  :pin melpa
  :config
  (setq ess-style 'RStudio))
(use-package flycheck
  :ensure t
  :pin melpa
  :init (global-flycheck-mode))
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))
(use-package pipenv
  :ensure t
  :hook
  (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
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
(use-package scratch-ext
  :ensure t
  :hook (after-init . scratch-ext-restore-last-scratch))
(use-package ws-butler
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))
(setq initial-major-mode 'org-mode)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq custom-file (make-temp-file "emacs-custom"))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))
(add-to-list 'default-frame-alist '(width . 100))
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
