;; -*- emacs-lisp -*-

(if (not load-file-name) (error "Load me by M-x load-file RET"))

(setq debug-on-error nil debug-on-quit nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Personal info
(setq user-full-name "Julian Qian"
      user-mail-address "junist@gmail.com")

(unless (getenv "ORGANIZATION")
  (setenv "ORGANIZATION" user-full-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; use-package
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa"        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
(package-initialize)

;;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; A wrapper for `use-package'
;; (use-package dot-emacs-helper :load-path "~/.emacs.d/lisp")

(defvar my/packages nil)
(defun my/save-package-name (orig-func &rest args)
  (let ((name (symbol-name (car args))))
    (when (and (not (assoc-string name my/packages)) load-file-name)
      (add-to-list 'my/packages (cons name load-file-name))
      (apply orig-func args))))
(advice-add #'use-package :around #'my/save-package-name)
;; (advice-remove #'use-package #'my/save-package-name)

(defun my/locate-package (name)
  "Locate package configuration by NAME."
  (interactive
   (list (completing-read "Locate package: " (mapcar (lambda (s) (car s)) my/packages))))
  (let ((pkg (assoc-string name my/packages)) done)
    (if (and pkg (cdr pkg) (file-exists-p (cdr pkg)))
        (progn
          (find-file (cdr pkg)) (goto-char (point-min)) (setq done t)
          (re-search-forward
           (concat "(\\s-*\\use-package\\s-+" (regexp-quote  (car pkg))))
          (recenter-top-bottom 0)))
    (unless done (message "Failed to locate package %s." name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper function
(defmacro my/add-hook (hook &rest forms)
  "Bind all functions in FORMS to the HOOK.

Example:
  (my/add-hook (c-mode-common-hook text-mode-hook)
    (flyspell-prog-mode)
    (auto-fill-mode 1))

  (my/add-hook c-mode-common-hook flymake-minor-mode)
"
  (declare (indent 1))
  (let ((hooks (if (listp hook) hook (list hook))))
    (nconc (list 'progn)
           (mapcar (lambda (hk)
                     (list 'add-hook (list 'quote hk)
                           (if (listp (car forms))
                               `(lambda nil ,@forms)
                             (list 'quote (car forms)))))
                   hooks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Better defaults

;; Echo key strokes quickly
(setq echo-keystrokes 0.1)
;; Indent without tab '\t' but white space
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4 tab-stop-list nil)
;; For morden machine, initiate GC every 20MB allocated
(setq gc-cons-threshold 20000000)
;; Auto fill : M-q
(setq default-justification 'full
      adaptive-fill-mode nil
      default-fill-column 78)
;; Highlight trailing whitespace
(setq show-trailing-whitespace t)
;; Avoid add duplicated string to `kill-ring'
(setq kill-do-not-save-duplicates t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
;; Turn on the features disabled default
(setq disabled-command-function nil)
(setq tab-always-indent 'complete
      save-interprogram-paste-before-kill t
      apropos-do-all t
      require-final-newline t
      load-prefer-newer t)
;; Empty scratch buffer
(setq initial-scratch-message nil
      initial-major-mode 'text-mode)
;; A saner ediff
(setq diff-switches "-ubB"
      ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)
;; Better UX
(setq x-select-enable-clipboard t
      mouse-yank-at-point t
      column-number-mode t
      visible-bell t)
;; Line-by-Line Scrolling
(setq scroll-step 1
      scroll-conservatively 10000)
;; Centrallize backup files
(setq version-control t
      delete-old-versions t)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq vc-follow-symlinks t)

;; Grep ignore
(with-eval-after-load "grep"
  (dolist (f '("*.bak" "*.z" "*.zip" "*.7z" "*.tar" "*.bz2" "*.gz" "*.tgz" "*.class" "*.jar"))
    (add-to-list 'grep-find-ignored-files f)))
;; Auto select help window
(setq help-window-select t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Handy way of getting back to previous places
(bind-key* "C-c p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(my/add-hook (isearch-mode-hook)        ;isearch selected region
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark) (isearch-push-state) (isearch-yank-string region))))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p t)

(my/add-hook (before-save-hook)         ;do something when save buffer
  (when (> 3000 (count-lines (point-min) (point-max)))
    (delete-trailing-whitespace)
    (if (member major-mode
                '(c-mode c++-mode python-mode emacs-lisp-mode))
        (untabify (point-min) (point-max)))
    (copyright-update)
    (time-stamp)))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; build-in

(autoload 'zap-up-to-char "misc" nil t)

(toggle-truncate-lines 1)
(which-function-mode 1)
(show-paren-mode t)
(delete-selection-mode t)               ;Make typing overwrite text selection
(global-auto-revert-mode t)
(when (> emacs-major-version 24)
  (electric-pair-mode t)
  (global-subword-mode t)
  (save-place-mode t))

(use-package dired
  :bind (:map dired-mode-map
              ("M-u"  . my/dired-up-directory))
  :init
  (defun my/dired-up-directory()
    (interactive) (find-alternate-file ".."))
  :config
  (setq dired-isearch-filenames 'dwim
        dired-dwim-target t
        dired-auto-revert-buffer t)
  ;; Open directory in the same buffer
  (put 'dired-find-alternate-file 'disabled nil)
  ;; Sort directories firstly, and truncate long lines
  (my/add-hook dired-after-readin-hook
    (set (make-local-variable 'truncate-lines) t)
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2)
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))))
  )

(use-package dired-x
  :bind* ("C-x C-j" . dired-jump)
  :config (add-hook 'dired-mode-hook #'dired-omit-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        ;; uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator "/"
        uniquify-ignore-buffers-re "^\\*"))

(use-package savehist
  :config
  (setq savehist-additional-variables
        '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring
                    extended-command-history)
        savehist-autosave-interval 60)
  (savehist-mode t))

(use-package recentf
  :config
  (setq recentf-max-saved-items 1000
        recentf-exclude `(,tramp-file-name-regexp))
  (recentf-mode t)

  ;; Also save recent opened directories besides files
  (my/add-hook (dired-mode-hook)
    (recentf-add-file dired-directory)))

(use-package ffap
  :commands (ffap)
  :bind* ("C-c j" . ffap))

(use-package flyspell
  :diminish flyspell-mode
  :defer 3
  :config
  (my/add-hook (markdown-mode-hook text-mode-hook org-mode-hook)
    flyspell-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; third-party packages

(use-package ivy
  :disabled
  :diminish ivy-mode
  :bind (:map ivy-minibuffer-map
              ("C-w" . ivy-backward-kill-word)
              ("C-z" . ivy-dispatching-done) ;Compatible to `helm-select-action'
              ("C-c o" . ivy-occur))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full    ;Show full path of virtual buffers
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil
        ivy-count-format "(%d/%d) ")

  (setq ivy-re-builders-alist           ;M-r, toggle regex
        '((counsel-ag . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))

  (ivy-set-actions                      ;M-o, ivy-dispatching-done
   t '(("I" insert "insert")))

  (ivy-add-actions
   'ivy-switch-buffer
   '(("l" counsel-locate "locate")))

  (custom-set-faces
   '(ivy-current-match ((t (:background "#12b7c0")))))

  (use-package swiper
    :disabled
    :bind (("C-s" . swiper)
           ("C-r" . swiper)))
  )

(use-package helm                       ; http://tuhdo.github.io/helm-intro.html
  :ensure t
  :diminish helm-mode
  ;; :bind-keymap* ("C-c h" . helm-command-prefix)
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-c i" . helm-semantic-or-imenu)
         ("C-c r" . helm-recentf)
         ("C-x b" . helm-mini)          ;switch-to-buffer
         ("C-x C-r" . helm-resume)      ;conflict find-file-read-only
         ("C-x C-f" . helm-find-files)
         ("C-x M-f" . helm-for-files)
         ("C-h SPC" . helm-all-mark-rings)
         :map helm-command-map
         ("i" . helm-semantic-or-imenu)
         ("<tab>" . helm-lisp-completion-at-point)
         ("x" . helm-register)
         ("p" . helm-projectile)
         ("a" . helm-do-grep-ag)
         ("g" . helm-grep-do-git-grep)
         :map minibuffer-local-map
         ("C-c C-l" . helm-minibuffer-history)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
         ("C-i"   . helm-execute-persistent-action) ; make TAB works in terminal
         ("C-z"   . helm-select-action)             ; list actions using C-z
         ("C-w"   . backward-kill-word)
         :map helm-find-files-map
         ("M-u" . helm-find-files-up-one-level)
         ("C-w" . helm-find-files-up-one-level)
         :map helm-read-file-map
         ("C-w" . helm-find-files-up-one-level))
  :init
  (require 'helm-config)
  (helm-mode t)
  (helm-adaptive-mode t)
  (ido-mode -1)          ; Turn off ido mode in case I enabled it accidentally
  :config
  (unbind-key "C-l" helm-read-file-map)
  (unbind-key "C-l" helm-find-files-map)

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (when (eq system-type 'darwin)
    (setq helm-locate-command "mdfind -name %s %s"))

  ;; enable man page at point
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  ;; enable fuzzy matching
  (setq helm-buffers-fuzzy-matching  t   ; helm-mini
        helm-recentf-fuzzy-match     t   ; helm-mini
        helm-semantic-fuzzy-match    t   ; helm-semantic-or-imenu
        helm-imenu-fuzzy-match       t   ; helm-semantic-or-imenu
        helm-locate-fuzzy-match      nil ; helm-locate
        helm-lisp-fuzzy-completion   t   ; helm-lisp-completion-at-point
        helm-find-files-ignore-thing-at-point t ; helm-find-files
        )

  (setq helm-candidate-number-limit 100
        helm-display-header-line nil)
  ;; From https://gist.github.com/antifuchs/9238468
  (setq helm-input-idle-delay 0.01
        helm-split-window-in-side-p t
        helm-scroll-amount 8
        helm-ff-search-library-in-sexp t
        helm-ff-skip-boring-files t
        helm-ff-file-name-history-use-recentf t)

  (helm-autoresize-mode 1)

  (use-package helm-flx
    :config (helm-flx-mode 1))

  (use-package helm-swoop
    :ensure t
    :commands (helm-swoop helm-multi-swoop)
    :bind (("M-i"     . helm-swoop)
           ("C-x c s" . helm-swoop)
           ("M-I"     . helm-swoop-back-to-last-point)
           :map isearch-mode-map
           ("M-i" . helm-swoop-from-isearch)
           :map helm-swoop-map
           ("C-s" . helm-next-line)
           ("C-r" . helm-previous-line)
           ("M-i" . helm-multi-swoop-all-from-helm-swoop)
           ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop)
           :map helm-multi-swoop-map
           ("C-s" . helm-next-line)
           ("C-r" . helm-previous-line))
    :config
    (setq helm-swoop-move-to-line-cycle nil
          helm-swoop-use-line-number-face t)
    )

  (use-package helm-gtags
    :after c-mode
    :if (executable-find "gtags")
    :diminish (helm-gtags-mode . "hG")
    :bind (:map helm-gtags-mode-map
                ("M-." . helm-gtags-find-tag)
                ("M-," . helm-gtags-pop-stack)
                ("M-*" . helm-gtags-pop-stack)
                ("M-s d" . helm-gtags-dwim)
                ("M-s r" . helm-gtags-find-rtag)
                ("M-s s" . helm-gtags-find-symbol)
                ("C-c i" . helm-gtags-parse-file)  ;replace imenu
                ("C-c <" . helm-gtags-previous-history)
                ("C-c >" . helm-gtags-next-history))
    :config
    (setq helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t)
    (add-hook 'c-mode-hook #'helm-gtags-mode)
    (add-hook 'c++-mode-hook #'helm-gtags-mode))

  (use-package helm-c-yasnippet
    :after yasnippet
    :bind ("C-c y" . helm-yas-complete)
    :config
    (setq helm-yas-space-match-any-greedy t
          helm-yas-display-key-on-candidate t))

  (use-package helm-ls-git
    :config
    :bind (:map helm-command-map
                ("G" . helm-ls-git-ls)))

  (use-package helm-projectile
    :after projectile
    :config
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile)
    (helm-projectile-on))

  (use-package helm-dash
    :bind (:map helm-command-map
                ("d" . helm-dash-at-point))
    :config
    ;; Dash docsets feeds: https://github.com/Kapeli/feeds
    (setq helm-dash-browser-func 'browse-url
          helm-dash-common-docsets '("C++" "Python 2"))
    (my/add-hook python-mode-hook
      (setq-local helm-dash-docsets '("Python 2"))))
  )

(use-package company
  :ensure t
  :defer 3
  :diminish company-mode
  ;; :bind ("C-." . company-files)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :bind (:map company-mode-map
              ("<C-return>" . company-complete-common)
              ("C-." . company-files))
  :config
  (setq company-echo-delay 0
        ;; company-idle-delay 0
        ;; company-auto-complete nil
        company-minimum-prefix-length 2
        company-tooltip-flip-when-above t)

  (setq company-global-modes
        '(not magit-status-mode git-commit-mode help-mode Info-mode
              view-mode makefile-mode makefile-gmake-mode Custom-mode
              term-mode compilation-mode))
  (global-company-mode)
  ;; (add-hook 'prog-mode-hook 'company-mode-on)

  (push (apply-partially
         #'cl-remove-if
         (lambda (c)
           (or (string-match-p "[^\x00-\x7F]+" c) ;remove those non-ANSII candidates.
               (string-match-p "[0-9]+" c)        ;remove any completion containing numbers.
               (if (equal major-mode "org")       ;remove any candidate which is longer than 15 in org-mode
                   (>= (length c) 15)))))
        company-transformers)
  )

(use-package yasnippet
  ;; Compile all directories in the list `yas-snippet-dirs' with the
  ;; `yas-recompile-all' function.
  :ensure t
  :defer 3
  :diminish yas-minor-mode
  ;; :init
  ;; (with-eval-after-load 'yasnippet
  ;;   (setq yas-snippet-dirs (remq 'yas-installed-snippets-dir yas-snippet-dirs)))
  :config
  (add-hook 'prog-mode-hook 'yas-minor-mode-on) ; For emacs24+, replace `yas-global-mode'

  (setq yas-expand-only-for-last-commands nil
        yas-key-syntaxes '("w_" "w_." "^ ")
        yas-wrap-around-region t
        yas-indent-line nil)            ; stop auto-indent behavior when expanding snippets

  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-ido-prompt
                               yas-completing-prompt))

  ;; FOR `hippie-try-expand' setting
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)

  (bind-keys
   :map yas-minor-mode-map
   ("C-c <tab>" . yas-expand)
   ("C-c TAB" . yas-expand)
   ("C-c y" . yas-insert-snippet))        ; List all snippets for current mode
  (unbind-key "<tab>" yas-minor-mode-map) ; Remove yas-expand from <tab> keybind
  (unbind-key "TAB" yas-minor-mode-map)

  (defadvice yas-insert-snippet (around use-completing-prompt activate)
    "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
    (let ((yas-prompt-functions '(yas-completing-prompt))) ad-do-it))
  )

(use-package avy
  :ensure t
  :bind*
  ("C-c C-j" . avy-goto-word-1)
  ("M-3"     . avy-goto-word-1)
  ("M-4"     . avy-goto-word-or-subword-1)
  ("M-g w"   . avy-goto-word-1)
  ("M-g f"   . avy-goto-char)
  ("M-g l"   . avy-goto-line)
  :config
  (avy-setup-default)
  (setq avy-background t)
  (setq avy-keys (number-sequence ?a ?z)))

(use-package expand-region
  :ensure t
  :bind
  ("C-1"  .   er/expand-region)
  ("M-2"  .   er/expand-region)
  ("C-M-2"  .   er/contract-region))

(use-package smart-mode-line
  :config
  ;; (setq sml/theme 'dark)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package multi-term
  :ensure t
  :bind*
  ("C-x t c" . multi-term)
  ("C-x t t" . multi-term-dedicated-open-select)
  ("C-x t q" . multi-term-dedicated-close)
  ("C-x t s" . multi-term-dedicated-select)
  ("C-x t g" . multi-term-dedicated-toggle)
  :config
  (setq multi-term-program "/bin/bash")
  (setq multi-term-dedicated-window-height 10
        multi-term-dedicated-max-window-height 10)

  ;; compatible with normal terminal keybinds
  (add-to-list 'term-bind-key-alist '("<M-backspace>" . term-send-backward-kill-word))
  (add-to-list 'term-bind-key-alist '("<C-backspace>" . term-send-backward-kill-word))
  (add-to-list 'term-bind-key-alist '("M-DEL" . term-send-backward-kill-word))
  (add-to-list 'term-bind-key-alist '("<backspace>" . term-send-backspace))
  (add-to-list 'term-bind-key-alist '("C-d" . term-send-del))
  (add-to-list 'term-bind-key-alist '("<delete>" . term-send-del))
  (add-to-list 'term-bind-key-alist '("M-d" . term-send-forward-kill-word))
  (add-to-list 'term-bind-key-alist '("<tab>" . (lambda nil (interactive) (term-send-raw-string "\C-i"))))
  ;; some helpful key bindings
  (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
  (add-to-list 'term-bind-key-alist '("C-y" . term-paste))
  (add-to-list 'term-bind-key-alist '("C-q" . multi-term-dedicated-close))
  ;; unbind keys
  (setq term-unbind-key-list (append term-unbind-key-list '("C-v" "M-v")))

  ;; hack to backward kill word as it does in terminal
  (defun term-send-backward-kill-word ()
    "Backward kill word in term mode."
    (interactive)
    (term-send-raw-string "\e\C-?"))

  (defun multi-term-dedicated-open-select ()
    (interactive)
    (unless (multi-term-dedicated-exist-p)
      (multi-term-dedicated-open))
    (multi-term-dedicated-select))
  )

(use-package highlight-symbol           ; Refer hi-lock.el
  :ensure t
  :commands (highlight-symbol-mode highlight-symbol-at-point)
  :diminish highlight-symbol-mode
  :bind*
  ("M-s h h" . highlight-symbol-at-point) ; "M-s h ."
  ("M-s h n" . highlight-symbol-next)
  ("M-s h p" . highlight-symbol-prev)
  :init
  (when window-system
    (my/add-hook (emacs-lisp-mode-hook python-mode-hook c-mode-common-hook)
      (highlight-symbol-mode 1)))       ; NOTE: maybe performance issue
  :config
  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-on-navigation-p t))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  ;; Hide/Modify some function prefix in which-key show menu
  (dolist (item '(("\\`calc-" . "") ; Hide "calc-" prefixes when listing M-x calc keys
                  ("/body\\'" . "") ; Remove display the "/body" portion of hydra fn names
                  ("\\`hydra-" . "+h/")
                  ("\\`org-babel-" . "ob/")
                  ("\\`my/" . "")))
    (add-to-list 'which-key-description-replacement-alist item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; programming

(use-package flycheck
  :commands (flycheck-mode global-flycheck-mode)
  ;; :init
  ;; (dolist (mode '(python-mode-hook c-mode-common-hook))
  ;;   (add-hook mode 'flycheck-mode))
  :config
  ;;# Rebind flycheck prefix key
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-x f")) ;conflict set-fill-column
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)

  ;;# Workaround to avoid eldoc override flycheck error message
  (setq flycheck-display-errors-delay 1.1)
  ;; (setq flycheck-indication-mode 'right-fringe)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)

  ;;# python flake8 config: ~/.flake8rc
  ;;  http://flake8.readthedocs.org/en/latest/vcs.html#git-hook
  (setq flycheck-python-flake8-executable (executable-find "flake8"))

  (setq flycheck-gcc-language-standard "c++11")
  )

(use-package projectile
  :ensure t
  :defer 3
  :diminish (projectile-mode . "Pj")
  :init (setq projectile-keymap-prefix "p")
  ;; :bind-keymap* ("C-x p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("f" . projectile-find-file)
              ("s" . projectile-switch-project)
              ("a" . projectile-ag)
              ("g" . projectile-grep)
              ("o" . projectile-find-other-file) ; instead of `projectile-multi-occur'
              ("t" . projectile-toggle-between-implementation-and-test))
  :config
  (projectile-mode)

  (setq projectile-enable-caching t)

  (dolist (dir '(".svn" "CVS" "bin" ".git"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (dolist (dir '("ede-project.el"))
    (add-to-list 'projectile-project-root-files dir))
  (dolist (file '("GTAGS" "GPATH" "GRTAGS"))
    (add-to-list 'projectile-globally-ignored-files file))
  (dolist (suffix '(".pyc" ".bak"))
    (add-to-list 'projectile-globally-ignored-file-suffixes suffix))
  )

(use-package magit
  :ensure t
  :commands (magit-status magit-init)
  :bind*
  ("C-c g"  . magit-status)
  :config
  ;; Subtler highlight
  (set-face-background 'diff-file-header "#121212")
  (set-face-foreground 'diff-context "#666666")
  (set-face-foreground 'diff-added "#00cc33")
  (set-face-foreground 'diff-removed "#ff0000")

  (add-hook 'magit-mode-hook 'magit-load-config-extensions)

  (with-eval-after-load 'git-commit
    (define-key git-commit-mode-map (kbd "C-c C-k") 'magit-exit-commit-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c++
;;; https://tuhdo.github.io/c-ide.html
(setq-default c-basic-offset tab-width)
(with-eval-after-load "find-file"
  (dolist (dir '("../../src" "../include/*"))
    (add-to-list 'cc-search-directories dir t)))

(use-package google-c-style :ensure t)

;;# If function name is too long, we will indent the parameters forward.
(defun my/c-lineup-arglist (langelem)
  (let ((c-lineup-maximum-indent 20)
        (ret (c-lineup-arglist langelem)))
    (if (< (elt ret 0) c-lineup-maximum-indent) ret
      (save-excursion
        (goto-char (cdr langelem))
        (vector (+ (current-column) 8))))))
(defun my/c-indent-lineup-arglist ()
  (setcdr (assoc 'arglist-cont-nonempty c-offsets-alist)
          '(c-lineup-gcc-asm-reg my/c-lineup-arglist)))

;;# Convert some .h to c++-mode automatically
(defun my/c-correct-hpp-mode ()
  (if (and (not (derived-mode-p 'c++-mode)) (string-match "\.h$" (buffer-name))
           (save-excursion
             (goto-char (point-min))
             (search-forward-regexp "^\\(class\\|template\\|namespace\\)" nil t)))
      (c++-mode)))

(defun my/c-mode-common-hook ()
  (my/c-indent-lineup-arglist)
  (my/c-correct-hpp-mode)
  (google-set-c-style)
  (google-make-newline-indent)
  (c-toggle-auto-hungry-state 1)
  (c-toggle-hungry-state t)
  (c-toggle-auto-newline nil)
  (eldoc-mode 1)
  (local-unset-key "\C-d")            ; trigger for `c-electric-delete-forward'
  (local-set-key "\C-cca" 'ff-find-other-file))
(add-hook 'c-mode-common-hook 'my/c-mode-common-hook)

(use-package irony
  :after c-mode
  :bind
  ;; Replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's asynchronous function
  (:map irony-mode-map
        ([remap completion-at-point] . irony-completion-at-point-async)
        ([remap complete-symbol]     . irony-completion-at-point-async))
  :config
  (add-to-list 'c++-mode-hook #'irony-mode)

  (use-package company-irony
    :after company
    :config
    (add-to-list 'company-backends 'company-irony))

  (use-package company-irony-c-headers
    :after company
    :config
    (add-to-list 'company-backends 'company-irony-c-headers))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python
;;; http://segmentfault.com/a/1190000004165173
;;; $ pip install virtualenv ipython autopep8 flake8 jedi
(use-package elpy
  :ensure t
  :config
  ;; (unbind-key "C-c C-r" elpy-mode-map)  ;conflict helm-resume
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-python-command "~/.virtualenvs/jedi/bin/python")
  (setq python-shell-interpreter elpy-rpc-python-command)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode)
  (elpy-enable)
  ;; (elpy-use-ipython "ipython3")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defadvice settings
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy the
current single line to `kill-ring' instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill the
current single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convenient functions

(defun my/beginning-of-line ()
  "Hack `beginning-of-line'."
  (interactive)
  (if (eq (point) (line-beginning-position))
      (skip-chars-forward " \t") (beginning-of-line)))

(defun my/end-of-line ()
  "Hack `end-of-line'."
  (interactive)
  (if (eq (point) (line-end-position))
      (skip-chars-backward " \t") (move-end-of-line 1)))

(defun my/comment-or-uncomment-region (&optional line)
  "Comment or uncomment a line or a region."
  (interactive "P")
  (unless (or line (and mark-active (not (equal (mark) (point)))))
    (setq line 1))
  (if line (save-excursion
             (comment-or-uncomment-region
              (progn (beginning-of-line) (point))
              (progn (end-of-line) (point))))
    (call-interactively 'comment-or-uncomment-region)))

(defun my/vi-open-next-line (arg)
  "Move to the next line (like vi) and then open a new line. bind
to \\[my/vi-open-next-line]."
  (interactive "p")
  (end-of-line) (open-line arg) (forward-line 1)
  (indent-according-to-mode))

(defun my/vi-join-lines(&optional arg)
  "Join next line to current line (like J in vi), splitted by
only one space. bind to \\[my/vi-join-lines]."
  (interactive "P")
  (setq arg (abs (if arg (prefix-numeric-value arg) 1)))
  (while (> arg 0)
    (end-of-line)
    (save-excursion
      (delete-char 1) (just-one-space 1))
    (setq arg (- arg 1))))

(defun my/vi-merge-lines(&optional arg)
  "Merge next line to current line (like gJ in vi), without
spaces leaving. bind to \\[my/vi-merge-lines]."
  (interactive "P")
  (setq arg (abs (if arg (prefix-numeric-value arg) 1)))
  (while (> arg 0)
    (save-excursion
      (end-of-line) (delete-char 1) (delete-horizontal-space))
    (setq arg (- arg 1))))

(defun my/delete-char-or-region ()
  "hack `delete-char', delete char or region, skip kill ring."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'delete-char)))

(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push erased text to `kill-ring'.

http://xahlee.org/emacs/emacs_kill-ring.html
"
  (interactive "p")
  (delete-region
   (point) (if (and (boundp 'subword-mode) subword-mode)
               (subword-forward arg) (progn (forward-word arg) (point)))))

(defun my/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push erased text to `kill-ring'."
  (interactive "p")
  (my/delete-word (- arg)))

(defun my/delete-line ()
  "Delete text from current position to end of line char.
If cursor at beginning or end of a line, delete the last RET."
  (interactive)
  (let ((be (or (bolp) (eolp))))
    (delete-region
     (point) (save-excursion (move-end-of-line 1) (point)))
    (if be (delete-char 1))))

(defun my/delete-line-backward ()
  "Delete text between the beginning of the line to the cursor
position.
If cursor at beginning or end of a line, delete the previous RET."
  (interactive)
  (let ((be (or (bolp) (eolp))))
    (delete-region
     (point) (save-excursion (move-beginning-of-line 1) (point)))
    (if be (delete-char -1))))

(defun my/show-buffer-path (&optional copy)
  "Display the absolute path of current buffer in mini-buffer. If
you call this function by prefix 'C-u', the path will be save
into `kill-ring'.

\\[my/show-buffer-path]        display buffer's absolute path
C-u \\[my/show-buffer-path]    copy buffer's absolute path
C-u 1 \\[my/show-buffer-path]  copy buffer's directory name
C-u 2 \\[my/show-buffer-path]  copy buffer's basename
"
  (interactive (list current-prefix-arg))
  (let ((f (buffer-file-name (current-buffer))))
    (if f (case copy
            ((nil) (message "Buffer path: %s" f))
            ;; TODO: prompt what to be copied
            (1 (let ((d (file-name-directory f)))
                 (kill-new d) (message "Copy directory: %s" d)))
            (2 (let ((d (file-name-nondirectory f)))
                 (kill-new d) (message "Copy filename: %s" d)))
            (t (kill-new f) (message "Copy path: %s" f))))))

(defun my/switch-scratch ()
  "switch to *scratch* buffer, bind to \\[my/switch-scratch]."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun my/revert-buffer ()
  "Revert buffer without prompt."
  (interactive)
  (revert-buffer nil t nil))

(defun my/astyle-buffer (pmin pmax)
  (interactive "r")
  (shell-command-on-region pmin pmax
                           "astyle" ;; add options here...
                           (current-buffer) t
                           (get-buffer-create "*Astyle Errors*") t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybinds

(bind-keys
 ([remap delete-char]             . my/delete-char-or-region) ;C-d
 ([remap move-beginning-of-line]  . my/beginning-of-line)     ;C-a
 ([remap move-end-of-line]        . my/end-of-line)           ;C-e
 ([remap kill-line]               . my/delete-line)           ;C-k
 ([remap kill-word]               . my/delete-word)           ;M-d
 ([remap backward-kill-word]      . my/backward-delete-word) ;M-DEL, <C-backspace>

 ("M-d"   .  my/delete-word)            ;M-d
 ("C-S-k" .  my/delete-line-backward)
 ;; ("M-2"  .  extend-selection)           ;alternative er/expand-region
 ("C-2"   .  set-mark-command)
 ("C-m"   .  newline-and-indent)
 ("C-j"   .  newline)
 ("C-o"   .  my/vi-open-next-line)
 ("C-M-o" .  split-line)
 ;; ("C-'"   .  redo)
 ("C-\\"  .  my/comment-or-uncomment-region)
 ("M-5"   .  my/show-buffer-path)
 ("M-0"   .  other-window)
 ("M-'"   .  just-one-space)
 ("M--"   .  delete-blank-lines)
 ("M-J"   .  my/vi-join-lines)
 ("C-M-j" .  my/vi-merge-lines)
 ("M-z"   .  zap-up-to-char)
 ("M-q"   .  compact-uncompact-block)
 ("M-n"    . (lambda() (interactive) (scroll-up-command 1)))
 ("<down>" . (lambda() (interactive) (scroll-up-command 1)))
 ("M-p"    . (lambda() (interactive) (scroll-down-command 1)))
 ("<up>"   . (lambda() (interactive) (scroll-down-command 1)))
 ("C-h j"   . (lambda () (interactive) (info "elisp")))
 ("C-h C-w" .  woman)
 ("<C-mouse-4>" .  text-scale-increase)
 ("<C-mouse-5>" .  text-scale-decrease)
 ("<C-down-mouse-1>" .  undefined)
 ("<S-insert>"  .  yank-unindented)
 )

(bind-keys
 :map ctl-x-map
 ("C-2" . pop-global-mark)
 ("C-b" . ibuffer)
 ("C-k" . kill-this-buffer)
 ;; ("C-o" . my/switch-recent-buffer)
 ("C-o" . mode-line-other-buffer)
 ;; ("C-r" . sudo-edit)
 ("C-t" . transpose-sexps)
 ;; ("C-_" . fit-frame)
 ;; ("t"  . template-expand-template)
 ;; ("m"  . message-mail)
 ("\\"  . align-regexp)
 )

(bind-keys
 :map mode-specific-map                 ;C-c
 ("k" . kmacro-keymap)
 ("$" . toggle-truncate-lines)
 ;; ("f" . completion-at-point)
 ;; ("k" . auto-fill-mode)
 ;; ("q" . refill-mode)
 )

;;; Define ctl-cc-map for 'C-c c' commands
(defvar ctl-cc-map (make-sparse-keymap)
  "Keymap for subcommands of C-c c.")
(defalias 'ctl-cc-prefix ctl-cc-map)
(define-key mode-specific-map "c" 'ctl-cc-prefix)

(bind-keys
 :map ctl-cc-map
 ("b" . my/revert-buffer)
 ("c" . my/switch-scratch)
 ("d" . my/locate-package)
 ("f" . flycheck-mode)
 ("i" . ispell-word)
 ("l" . global-linum-mode)
 ("m" . desktop-menu)
 ("t" . auto-insert)
 ("v" . view-mode)
 ("\t" . ispell-complete-word)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extra configuration
(let ((d (if (boundp 'my/config-directory) my/config-directory
           (file-name-directory load-file-name))))
  (mapc 'load (directory-files d t "^[0-9]+-.*.el$")))
(server-start)
