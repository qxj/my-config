;; -*- emacs-lisp -*-

(if (not load-file-name) (error "Load me by M-x load-file RET"))

(setq debug-on-error t debug-on-quit nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal info
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

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; A wrapper for `use-package'
;; (use-package dot-emacs-helper :load-path "~/.emacs.d/lisp")

(defvar my/packages nil)
(defun my/record-package-name (orig-func &rest args)
  (let ((name (symbol-name (car args))))
    (when (and (not (assoc-string name my/packages)) load-file-name)
      (add-to-list 'my/packages (cons name load-file-name))
      (apply orig-func args))))
(advice-add #'use-package :around #'my/record-package-name)

(defun my/locate-package (name)
  "Locate package configuration by name."
  (interactive
   (list (funcall (if (fboundp 'helm-comp-read) 'helm-comp-read 'completing-read)
                  "Locate package: " (mapcar (lambda (s) (car s)) my/packages))))
  (let ((pkg (assoc-string name my/packages)) done)
    (if (and pkg (cdr pkg) (file-exists-p (cdr pkg)))
        (progn
          (find-file (cdr pkg)) (goto-char (point-min)) (setq done t)
          (re-search-forward
           (concat "(\\s-*\\use-package\\s-+" (regexp-quote  (car pkg))))
          (recenter-top-bottom 0)))
    (unless done (message "Failed to locate package %s." name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper
(defmacro add-hooks (hook &rest forms)
  "Apply some functions for a hook.

Example:
  (add-hooks (c-mode-common-hook text-mode-hook)
    (flyspell-prog-mode)
    (auto-fill-mode 1))

  (add-hooks c-mode-common-hook flymake-minor-mode)
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
;;; better defaults
(show-paren-mode 1)
(icomplete-mode 1)
(when (> emacs-major-version 24)
  (electric-pair-mode 1)
  (subword-mode 1))

;; echo key strokes quickly
(setq echo-keystrokes 0.1)
;; indent without tab '\t' but white space
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4 tab-stop-list nil)
;; for morden machine, initiate GC every 20MB allocated
(setq gc-cons-threshold 20000000)
;; echo key strokes quickly
(setq echo-keystrokes 0.1)
;; auto fill : M-q
(setq default-justification 'full
      adaptive-fill-mode nil
      default-fill-column 78)
;; highlight trailing whitespace
(setq show-trailing-whitespace t)
;; Wrap too long lines
(toggle-truncate-lines 1)
;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
;; Turn on the features disabled default
(setq disabled-command-function nil)
(setq
 tab-always-indent 'complete

 save-interprogram-paste-before-kill t
 apropos-do-all t
 require-final-newline t
 load-prefer-newer t)
;; A saner ediff
(setq diff-switches "-ubB"
      ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)
;; Better UX
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      mouse-yank-at-point t
      visible-bell t)

(setq save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(defalias 'yes-or-no-p 'y-or-n-p)

;; Handy way of getting back to previous places
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(add-hooks (before-save-hook)
  (when (> 3000 (count-lines (point-min) (point-max)))
    (delete-trailing-whitespace)
    (if (member major-mode
                '(c-mode c++-mode python-mode emacs-lisp-mode))
        (untabify (point-min) (point-max)))
    (copyright-update)
    (time-stamp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; internal
(autoload 'zap-up-to-char "misc" nil t)
(autoload 'dired-jump "dired-x" nil t)

(use-package dired
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq dired-recursive-copies 'top
        dired-recursive-deletes 'top
        dired-isearch-filenames t       ; only search filename
        dired-dwim-target t)
  ;; Open directory in the same buffer
  (put 'dired-find-alternate-file 'disabled nil)
  (bind-keys
   :map dired-mode-map
   ("M-u"  . (lambda () (interactive) (find-alternate-file "..")))
   )
  )

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        ;; uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator "/"
        uniquify-ignore-buffers-re "^\\*"))

(use-package saveplace
  :config
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring
                    extended-command-history)
        savehist-autosave-interval 60)
  (savehist-mode t))

(use-package recentf
  :commands recentf-mode
  :bind*
  ("C-c c o" . helm-recentf)
  :init
  (setq recentf-max-saved-items 1000
        recentf-exclude `(,tramp-file-name-regexp))
  (recentf-mode t)

  ;; Also store recent opened directories besides files
  (add-hooks (dired-mode-hook)
    (recentf-add-file dired-directory)))

(use-package ffap
  :commands (ffap)
  :bind*
  ("C-c j" . ffap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packages

(use-package helm                       ; http://tuhdo.github.io/helm-intro.html
  :ensure t
  :diminish helm-mode
  ;; :bind-keymap* ("C-c h" . helm-command-prefix)
  :bind
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)
  ("C-c i" . helm-semantic-or-imenu)
  ("C-x b" . helm-mini)
  ("C-x C-b" . helm-buffers-list)
  ("C-x C-f" . helm-find-files)
  ("C-x M-f" . helm-for-files)
  ("C-h SPC" . helm-all-mark-rings)
  :init
  (require 'helm-config)
  ;; (global-unset-key (kbd "C-x c"))
  (helm-mode)
  (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
  :config
  (bind-keys
   :map minibuffer-local-map
   ("C-c C-l" . helm-minibuffer-history))

  (bind-keys
   :map helm-map
   ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
   ("C-i"   . helm-execute-persistent-action) ; make TAB works in terminal
   ("C-z"   . helm-select-action)             ; list actions using C-z
   ("C-w"   . backward-kill-word))

  (bind-keys
   :map helm-command-map
   ("i" . helm-semantic-or-imenu)
   ("m" . helm-man-woman)
   ("/" . helm-find)
   ("l" . helm-locate)
   ("a" . helm-apropos)
   ("o" . helm-occur)
   ("s" . helm-swoop)                   ;like occur
   ("<tab>" . helm-lisp-completion-at-point)
   ("b" . helm-resume)
   ("x" . helm-register)
   ("p" . helm-projectile)
   ("g" . helm-do-grep-ag)
   )

  (bind-keys
   :map helm-find-files-map
   ("M-u" . helm-find-files-up-one-level))

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  ;; enable man page at point
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  ;; enable fuzzy matching
  (setq helm-M-x-fuzzy-match t          ; helm-M-x
        helm-buffers-fuzzy-matching t   ; helm-mini
        helm-recentf-fuzzy-match    t   ; helm-mini
        helm-semantic-fuzzy-match t     ; helm-semantic-or-imenu
        helm-imenu-fuzzy-match    t     ; helm-semantic-or-imenu
        helm-locate-fuzzy-match nil ; helm-locate
        helm-apropos-fuzzy-match t      ; helm-apropos
        helm-lisp-fuzzy-completion t    ; helm-lisp-completion-at-point
        )

  (setq helm-candidate-number-limit 100)
  ;; From https://gist.github.com/antifuchs/9238468
  (setq helm-idle-delay 0.0         ; update fast sources immediately (doesn't).
        helm-input-idle-delay 0.01  ; this actually updates things reeeelatively quickly.
        helm-quick-update t
        helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        ;; helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-ff-skip-boring-files t
        helm-ff-file-name-history-use-recentf t
        helm-yas-display-key-on-candidate t)
  (helm-autoresize-mode 1)
  (setq helm-autoresize-min-height 3
        helm-autoresize-min-height 10)

  (use-package helm-swoop
    :defer
    :bind*
    ("C-s" . helm-swoop)
    ("C-c M-i" . helm-multi-swoop)
    ("C-x M-i" . helm-multi-swoop-all)
    :config
    (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)
    (bind-key "M-i" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)
    )

  (use-package helm-gtags
    :defer 3
    :if (executable-find "gtags")
    :diminish (helm-gtags-mode . "hG")
    :config
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t)
    (bind-keys
     :map helm-gtags-mode-map
     ("M-." . helm-gtags-find-tag)
     ("M-," . helm-gtags-pop-stack)
     ("M-*" . helm-gtags-pop-stack)
     ("M-s r" . helm-gtags-find-rtag)
     ("M-s s" . helm-gtags-find-symbol)
     ("C-c i" . helm-gtags-parse-file)  ;replace imenu
     ("C-c <" . helm-gtags-previous-history)
     ("C-c >" . helm-gtags-next-history)
     )
    (add-hook 'c-mode-hook #'helm-gtags-mode)
    (add-hook 'c++-mode-hook #'helm-gtags-mode))

  (use-package helm-c-yasnippet
    :after yasnippet
    :bind
    ("C-c y" . helm-yas-complete)
    :config
    (setq helm-yas-space-match-any-greedy t))
  )

(use-package company
  :ensure t
  :diminish company-mode
  ;; :bind
  ;; (("<tab>" . my/complete-or-indent)
  ;;  ("C-." . company-files))
  :config
  (bind-keys
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  (bind-keys
   :map company-mode-map
   ;; ("<tab>" . my/complete-or-indent)
   ("<C-return>" . company-complete-common)
   ("C-." . company-files))

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
  :diminish yas-minor-mode
  ;; :init
  ;; (with-eval-after-load 'yasnippet
  ;;   (setq yas-snippet-dirs (remq 'yas-installed-snippets-dir yas-snippet-dirs)))
  :config
  ;; (yas-global-mode 1)
  (add-hook 'prog-mode-hook 'yas-minor-mode-on) ; for emacs24+

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
  ("M-g w"   . avy-goto-word-1)
  ("C-c C-j" . avy-goto-word-1)
  ("M-4"     . avy-goto-word-or-subword-1)
  ("M-g f"   . avy-goto-char)
  ("M-g l"   . avy-goto-line)
  :config
  (avy-setup-default)
  (setq avy-background t)
  (setq avy-keys (number-sequence ?a ?z)))

(use-package jump-char
  :ensure t
  :commands (jump-char-forward jump-char-backward)
  :bind*
  ("M-m"   . jump-char-forward)         ;override back-to-indentation
  ("S-M-m" . jump-char-backward)
  ("C-c C-f"   . jump-char-forward)
  ("C-c C-M-f" . jump-char-backward)
  ("M-3"   . jump-char-forward)
  ("C-M-3" . jump-char-backward)
  :config
  ;; Don't highlight matches with jump-char - it's distracting
  (setq jump-char-lazy-highlight-face nil))

(use-package expand-region
  :ensure t
  :bind
  ("M-["  .   er/expand-region)
  ("C-1"  .   er/expand-region)
  ("M-2"  .   er/expand-region)
  ("M-]"  .   er/contract-region))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package multi-term
  :ensure t
  :bind*
  ("C-c t c" . multi-term)
  ("C-c t t" . multi-term-dedicated-open-select)
  ("C-c t q" . multi-term-dedicated-close)
  ("C-c t s" . multi-term-dedicated-select)
  ("C-c t g" . multi-term-dedicated-toggle)
  :config
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
  ;; Only close dedicated window
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

  :init
  (defun my/toggle-multi-term ()
    "Toggle dedicated `multi-term' window and select."
    (interactive)
    (if (multi-term-dedicated-exist-p)
        (multi-term-dedicated-close)
      (multi-term-dedicated-open-select)))
  )

(use-package highlight-symbol
  :ensure t
  :commands (highlight-symbol-mode
             highlight-symbol-at-point
             highlight-symbol-remove-all
             highlight-symbol-list-all)
  :diminish highlight-symbol-mode
  :bind*
  ("C-c c h" . highlight-symbol-at-point)
  ("<C-f3>" .  highlight-symbol-at-point)
  ("<f3>"   .  highlight-symbol-next)
  ("<S-f3>" .  highlight-symbol-prev)
  :init
  (when window-system
    (add-hooks (emacs-lisp-mode-hook python-mode-hook c-mode-common-hook)
      (highlight-symbol-mode 1)))       ; NOTE: maybe performance issue
  :config
  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-on-navigation-p t))

(use-package grizzl
  :config
  (setq *grizzl-read-max-results* 30))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  ;; Hide/Modify some function prefix in which-key show menu
  (dolist (item '(("\\`calc-" . "") ; Hide "calc-" prefixes when listing M-x calc keys
                  ("/body\\'" . "") ; Remove display the "/body" portion of hydra fn names
                  ("modi/" . "m/") ; The car is intentionally not "\\`modi/" to cover
                                        ; cases like `hydra-toggle/modi/..'.
                  ("\\`hydra-" . "+h/")
                  ("\\`org-babel-" . "ob/")
                  ("\\`my/" . "")))
    (add-to-list 'which-key-description-replacement-alist item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; programming

(use-package flycheck
  :ensure t
  :commands (flycheck-mode global-flycheck-mode)
  :init
  (dolist (mode '(python-mode-hook c-mode-common-hook))
    (add-hook mode 'flycheck-mode))
  :config
  ;;# rebind flycheck prefix key
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)

  ;;# workaround to avoid eldoc override flycheck error message
  (setq flycheck-display-errors-delay 1.1)
  ;; (setq flycheck-indication-mode 'right-fringe)

  ;; python code style
  ;; flake8 works with git:
  ;;     http://flake8.readthedocs.org/en/latest/vcs.html#git-hook
  (setq flycheck-python-flake8-executable (executable-find "flake8"))

  (setq flycheck-gcc-language-standard "c++11")
  )

(use-package projectile
  :ensure t
  :diminish (projectile-mode . "Pj")
  :bind*
  ("C-c p f" . projectile-find-file)
  ("C-c p s" . projectile-switch-project)
  ("C-c p g" . projectile-grep)
  ("C-c p t" . projectile-toggle-between-implementation-and-test)
  ;; C-c p f projectile-find-file
  ;; C-c p z projectile-cache-current-file
  ;; C-c p s projectile-switch-project
  ;; C-c p g projectile-grep
  ;; C-c p b projectile-switch-to-buffer
  ;; C-c p o projectile-multi-occur
  ;; C-c p r projectile-replace
  ;; C-c p e projectile-recentf
  ;; C-c p R projectile-regenerate-tags
  ;; C-c p c projectile-compile-project
  :config
  (projectile-global-mode)

  (setq projectile-enable-caching t)

  (dolist (dir '(".svn" "CVS" "bin" ".git"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (dolist (dir '("ede-project.el"))
    (add-to-list 'projectile-project-root-files dir))
  (dolist (file '("GTAGS" "GPATH" "GRTAGS"))
    (add-to-list 'projectile-globally-ignored-files file))
  (dolist (suffix '(".pyc" ".bak"))
    (add-to-list 'projectile-globally-ignored-file-suffixes suffix))

  (use-package helm-projectile
    :ensure t
    :config
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile)
    (helm-projectile-on))
  )

;; Crash Course on Emacswiki:
;;
;; - M-x magit-status to see git status, and in the status buffer:
;; - s to stage files
;; - c to commit (type in your commit message then C-c C-c to save the message and commit)
;; - b b to switch to a branch
;;
;; Other handy keys:
;;
;; - P P to do a git push
;; - F F to do a git pull
;;
;; try to press TAB
;;
(use-package magit
  :ensure t
  :bind*
  ("C-c g"  . magit-status)
  ("C-c l"  . magit-log)
  :config
  ;; Subtler highlight
  (set-face-background 'diff-file-header "#121212")
  (set-face-foreground 'diff-context "#666666")
  (set-face-foreground 'diff-added "#00cc33")
  (set-face-foreground 'diff-removed "#ff0000")

  (add-hook 'magit-mode-hook 'magit-load-config-extensions)

  (with-eval-after-load 'git-commit
    (define-key git-commit-mode-map (kbd "C-c C-k") 'magit-exit-commit-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c++
(setq-default tab-width 4
              c-basic-offset tab-width
              indent-tabs-mode nil
              c-hungry-delete-key t)


(defun my/c-mode-common-hook ()
  (c-toggle-auto-hungry-state 1)
  (c-toggle-hungry-state t)
  (c-toggle-auto-newline nil)
  (eldoc-mode 1)
  (local-unset-key "\C-d")            ; trigger for `c-electric-delete-forward'
  (local-set-key "\C-cca" 'ff-find-other-file))
(add-hook 'c-mode-common-hook 'my/c-mode-common-hook)

(use-package irony
  :config
  ;; (dolist (hook '(c++-mode-hook c-mode-hook objc-mode-hook))
  ;;   (add-to-list hook 'irony-mode))
  (add-to-list 'c++-mode-hook 'irony-mode)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's asynchronous function
  (defun my/irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my/irony-mode-hook)

  (use-package company-irony
    :after company
    :config
    (add-to-list 'company-backends 'company-irony))

  (use-package company-irony-c-headers
    :after company
    :config
    (add-to-list 'company-backends 'company-irony-c-headers)
    )
  )

;;# Preparation:
;; 1. $ sudo pip install virtualenv ipython autopep8 flake8 jedi
;;
;; http://segmentfault.com/a/1190000004165173
;;
(use-package elpy
  :ensure t
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode)

  (elpy-enable)
  (elpy-use-ipython)
  )

(use-package py-autopep8
  :ensure t
  :if (executable-find "autopep8")
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defadvice settings
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy the
current single line to `kill-ring' instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill the
current single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convenient functions

(defun my/switch-scratch ()
      "switch to *scratch* buffer, bind to \\[my/switch-scratch]."
      (interactive)
      (switch-to-buffer "*scratch*"))

(defun my/end-of-line ()
  "Hack `end-of-line'."
  (interactive)
  (if (eq (point) (line-end-position))
      (skip-chars-backward " \t")
    (move-end-of-line 1)))

(defun my/beginning-of-line ()
  "Hack `beginning-of-line'."
  (interactive)
  (if (eq (point) (line-beginning-position))
      (skip-chars-forward " \t")
    (beginning-of-line)))

(defun my/comment-or-uncomment-region (&optional line)
  "Comment or uncomment a line or a region."
  (interactive "P")
  (unless (or line (and mark-active (not (equal (mark) (point)))))
    (setq line 1))
  (if line
      (save-excursion
        (comment-or-uncomment-region
         (progn (beginning-of-line) (point))
         (progn (end-of-line) (point))))
    (call-interactively 'comment-or-uncomment-region)))

(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then open a new line. bind
to \\[vi-open-next-line]."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (indent-according-to-mode))

(defun vi-join-lines(&optional arg)
  "Join next line to current line (like J in vi), splitted by
only one space. bind to \\[vi-join-lines]."
  (interactive "P")
  (setq arg (abs (if arg (prefix-numeric-value arg) 1)))
  (while (> arg 0)
    (end-of-line)
    (save-excursion
      ;; (end-of-line)
      (delete-char 1)
      (just-one-space 1))
    (setq arg (- arg 1))))

(defun vi-merge-lines(&optional arg)
  "Merge next line to current line (like gJ in vi), without
spaces leaving. bind to \\[vi-merge-lines]."
  (interactive "P")
  (setq arg (abs (if arg (prefix-numeric-value arg) 1)))
  (while (> arg 0)
    (save-excursion
      (end-of-line)
      (delete-char 1)
      (delete-horizontal-space))
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
  (delete-region (point) (if (and (boundp 'subword-mode) subword-mode)
                             (subword-forward arg)
                           (progn (forward-word arg) (point)))))

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
     (point)
     (save-excursion (move-end-of-line 1) (point)))
    (if be (delete-char 1))))

(defun my/delete-line-backward ()
  "Delete text between the beginning of the line to the cursor
position.
If cursor at beginning or end of a line, delete the previous RET."
  (interactive)
  (let ((be (or (bolp) (eolp))))
    (delete-region
     (point)
     (save-excursion (move-beginning-of-line 1) (point)))
    (if be (delete-char -1))))

(defun my/display-buffer-path (&optional copy)
  "Display the absolute path of current buffer in mini-buffer. If
you call this function by prefix 'C-u', the path will be store
into `kill-ring'.

\\[my/display-buffer-path]        display buffer's absolute path
C-u \\[my/display-buffer-path]    copy buffer's absolute path
C-u 1 \\[my/display-buffer-path]  copy buffer's directory name
C-u 2 \\[my/display-buffer-path]  copy buffer's basename
"
  (interactive (list current-prefix-arg))
  (let ((f (buffer-file-name (current-buffer))))
    (if f (case copy
            ((nil) (message "Buffer path: %s" f))
            ;; TODO: prompt what to be copied
            (1 (let ((d (file-name-directory f)))
                 (kill-new d)
                 (message "Copy directory: %s" d)))
            (2 (let ((d (file-name-nondirectory f)))
                 (kill-new d)
                 (message "Copy filename: %s" d)))
            (t (kill-new f)
               (message "Copy path: %s" f))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybinds
(define-prefix-command 'ctl-c-map nil "Command prefix: C-c")
(define-prefix-command 'ctl-cc-map nil "Command prefix: C-c c")

(bind-keys
 ("C-c" . ctl-c-map)
 ("C-c c" . ctl-cc-map))

(bind-keys
 ([remap delete-char]  .  my/delete-char-or-region)           ;C-d
 ([remap move-beginning-of-line]  .  my/beginning-of-line) ;C-a
 ([remap move-end-of-line]  .  my/end-of-line)             ;C-e
 ([remap kill-line]  .  my/delete-line)                    ;C-k
 ([remap kill-word]  .  my/delete-word)                    ;M-d
 ([remap backward-kill-word] .  my/backward-delete-word) ;M-DEL, <C-backspace>

 ("M-d"  .  my/delete-word)           ;M-d
 ("C-S-k" .  my/delete-line-backward)
 ;; ("C-1"  .  extend-selection)         ; alternative er/expand-region
 ;; ("M-2"  .  extend-selection)
 ("C-2"   .  set-mark-command)
 ("C-m"  .  newline-and-indent)
 ("C-j"  .  newline)
 ("C-o"   .  vi-open-next-line)
 ("C-M-o" .  split-line)
 ("C-'"   .  redo)
 ("C-\\"  .  my/comment-or-uncomment-region)
 ("M-5"   .  my/display-buffer-path)
 ("M-0"   .  other-window)
 ("M-'"   .  just-one-space)
 ("M--"   .  delete-blank-lines)
 ("M-J"   .  vi-join-lines)
 ("M-z"   . zap-up-to-char)
 ("C-M-j" .  vi-merge-lines)
 ;; ("M-m" .  smart-mark)
 ("M-q"  .   compact-uncompact-block)
 ("M-n"  . (lambda() (interactive) (scroll-up-command 1)))
 ("<down>" . (lambda() (interactive) (scroll-up-command 1)))
 ("M-p"  . (lambda() (interactive) (scroll-down-command 1)))
 ("<up>" . (lambda() (interactive) (scroll-down-command 1)))
 ("C-h j"  .  (lambda () (interactive) (info "elisp")))
 ("C-h C-w" .  woman)
 ("<C-mouse-4>" .  text-scale-increase)
 ("<C-mouse-5>" .  text-scale-decrease)
 ("<C-down-mouse-1>" .  undefined)
 ("<S-insert>"  .  yank-unindented)
 )

(bind-keys
 :map ctl-x-map
 ("C-t" . transpose-sexps)
 ("C-r" . sudo-edit)
 ("C-k" . kill-this-buffer)
 ;; ("C-o" . my/switch-recent-buffer)
 ("C-o" . mode-line-other-buffer)

 ("C-_" . fit-frame)
 ;; ("t"  .  template-expand-template)
 ;; ("m"  .  message-mail)
 ("\\"  .  align-regexp)
 ("C-2" .  pop-global-mark)
 )

(bind-keys
 :map ctl-c-map
 ("C-k" . kmacro-keymap)
 ("$" . toggle-truncate-lines)
 ;; ("f" . comint-dynamic-complete)
 ("r" . org-capture)
 ;; ("k" . auto-fill-mode)
 ;; ("q" . refill-mode)
 ;; ("u" . revert-buffer)
 ;; ("v" . imenu-tree)
 ;; ("w" . my/favorite-window-config)
 ;; ("C-b" . browse-url-of-buffer)
 ("C-t" . tv-view-history)
 ("'"   . toggle-quotes)
 )

(bind-keys
 :map ctl-cc-map
 ("b" . my/revert-buffer)
 ("c" . my/switch-scratch)
 ("d" . my/locate-package)
 ("f" . flycheck-mode)
 ("i" . ispell-word)
 ("l" . global-linum-mode)
 ("m" . desktop-menu)
 ("n" . my/clone-buffer)
 ("t" . auto-insert)
 ("v" . view-mode)
 ("\t" . ispell-complete-word)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file "~/.emacs-custom.el")
(load custom-file t)
(let ((d (if (boundp 'my/config-directory) my/config-directory
           (file-name-directory load-file-name))))
  (mapc 'load (directory-files d t "^[0-9]+-.*.el$")))
(server-start)
