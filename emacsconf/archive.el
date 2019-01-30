;;; helm-ivy.el ---

(use-package isearch
  :bind (:map isearch-mode-map
              ("<tab>"  . isearch-complete)
              ("C-u"    . my/delete-line)
              ("C-y"    . my/isearch-symbol-at-point) ; instead of `isearch-yank-line'
              ("C-o"    . isearch-occur))
  :init
  ;; (setq isearch-case-fold-search t)     ; case insensitive
  (add-hook 'isearch-mode-hook #'my/isearch-with-region)
  (defun my/isearch-with-region ()
    (when mark-active
      (let ((region (funcall region-extract-function nil)))
        (deactivate-mark) (isearch-push-state) (isearch-yank-string region))))

  (defun my/isearch-symbol-at-point ()
    (interactive)
    (thing-at-point--beginning-of-sexp)
    (let ((s (thing-at-point 'symbol)))
      (if s (isearch-yank-string s) (isearch-yank-word-or-char))))

  (defun my/delete-line ()
    (interactive)
    (delete-region (save-excursion (move-beginning-of-line 1) (point))
                   (save-excursion (move-end-of-line 1) (point))))
  )

(use-package helm                       ; http://tuhdo.github.io/helm-intro.html
  :ensure t
  :diminish helm-mode
  ;; :bind-keymap* ("C-c h" . helm-command-prefix)
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-c i" . helm-semantic-or-imenu)
         ("C-c r" . helm-recentf)
         ("C-x b" . helm-mini)
         ("C-c C-r" . helm-resume)
         ("C-x C-f" . helm-find-files)
         ("C-x M-f" . helm-for-files)
         ("C-h SPC" . helm-all-mark-rings)
         :map helm-command-map
         ("i" . helm-semantic-or-imenu)
         ("<tab>" . helm-lisp-completion-at-point)
         ("x" . helm-register)
         ("p" . helm-projectile)
         ("a" . helm-do-grep-ag)
         ("j" . helm-grep-do-git-grep)
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
  (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
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
  (setq helm-buffers-fuzzy-matching t   ; helm-mini
        helm-recentf-fuzzy-match    t   ; helm-mini
        helm-semantic-fuzzy-match t     ; helm-semantic-or-imenu
        helm-imenu-fuzzy-match    t     ; helm-semantic-or-imenu
        helm-locate-fuzzy-match nil ; helm-locate
        helm-lisp-fuzzy-completion t    ; helm-lisp-completion-at-point
        helm-ff-guess-ffap-filenames t  ; helm-find-files
        )

  (setq helm-candidate-number-limit 100)
  ;; From https://gist.github.com/antifuchs/9238468
  (setq helm-input-idle-delay 0.01  ; this actually updates things reeeelatively quickly.
        helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        ;; helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-ff-skip-boring-files t
        helm-ff-file-name-history-use-recentf t)

  (helm-autoresize-mode 1)

  (use-package helm-swoop
    :ensure t
    :commands (helm-swoop helm-multi-swoop)
    :bind (("M-i"     . helm-swoop)
           ("C-x c s" . helm-swoop)
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
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
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
                ("g" . helm-ls-git-ls)))

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


(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind*
  ("C-x b" . ivy-switch-buffer)
  ("C-c C-r" . ivy-resume)
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-partial)
              ("C-w" . ivy-backward-kill-word)
              ("C-c o" . ivy-occur))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil
        ivy-count-format "(%d/%d) ")

  (ivy-set-actions                      ;M-o
   t '(("I" insert "insert")))

  (custom-set-faces
   '(ivy-current-match ((t (:background "#12b7c0")))))

  (use-package ivy-rich
    :config
    (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

  (use-package swiper
    :ensure
    :bind (("C-s" . swiper)
           ("C-r" . swiper)
           ("C-M-s" . swiper-all)
           :map isearch-mode-map
           ("M-i" . my/swiper-from-isearch))
    :init
    (defun my/swiper-from-isearch ()
      (interactive)
      (let (($query (if isearch-regexp isearch-string
                      (regexp-quote isearch-string))))
        (isearch-exit) (swiper $query)))
    )

  (use-package counsel
    :bind
    ("M-x" . counsel-M-x)
    ("M-y" . counsel-yank-pop)

    ("C-x d" . counsel-dired-jump)
    ("C-x C-f" . counsel-find-file)

    ("C-c i" . counsel-imenu)
    ("C-c r" . counsel-recentf)

    ("C-h f" . counsel-describe-function)
    ("C-h v" . counsel-describe-variable)
    ("C-h l" . counsel-find-library)
    ("C-h i" . counsel-info-lookup-symbol)
    ("C-h u" . counsel-unicode-char)

    ("C-x c g"  . counsel-git)
    ("C-x c j"  . counsel-git-grep)
    ("C-x c a"  . counsel-ag)
    ("C-x c l"  . counsel-locate)

    :bind
    (:map help-map
          ("f" . counsel-describe-function)
          ("v" . counsel-describe-variable)
          ("l" . counsel-info-lookup-symbol))
    :bind
    (:map read-expression-map
          ("C-r" . counsel-expression-history))

    :config
    (when (eq 'system-type 'darwin)
      (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind))

    (setq counsel-find-file-at-point t)
    )

  (use-package counsel-gtags
    :defer 3
    :if (executable-find "gtags")
    :diminish (counsel-gtags-mode . "cG")
    :bind (:map counsel-gtags-mode-map
                ("M-." . counsel-gtags-find-definition)
                ("M-," . counsel-gtags-pop-stack)
                ("M-s d" . counsel-gtags-dwim)
                ("M-s r" . counsel-gtags-find-reference)
                ("M-s s" . counsel-gtags-find-symbol))
    :config
    (add-hook 'c-mode-hook 'counsel-gtags-mode)
    (add-hook 'c++-mode-hook 'counsel-gtags-mode)
    )

  (use-package counsel-projectile
    :defer 3
    :config
    (counsel-projectile-on))

  (use-package counsel-dash
    :commands counsel-dash
    :bind
    ("C-x c d" . counsel-dash))
  )
