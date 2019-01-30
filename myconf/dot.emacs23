;; -*- emacs-lisp -*-

;;; Created by Julian Qian

;;; better-defaults.el
(progn
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd "C-l") 'ido-delete-backward-word-updir)))

  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  (require 'saveplace)
  (setq-default save-place t)

  (require 'desktop)
  (desktop-save-mode 1)

  (show-paren-mode 1)
  (when (> emacs-major-version 24)
    (electric-pair-mode 1)
    (subword-mode 1))

  (setq-default indent-tabs-mode nil)
  (setq x-select-enable-clipboard t
        x-select-enable-primary t
        save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        save-place-file (concat user-emacs-directory "places")
        backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))

(add-hook 'before-save-hook
          (lambda ()
            (when (> 3000 (count-lines (point-min) (point-max)))
              (delete-trailing-whitespace)
              (if (member major-mode
                          '(c-mode c++-mode python-mode php-mode emacs-lisp-mode))
                  (untabify (point-min) (point-max)))
              (copyright-update)
              (time-stamp))))

;;; dired
(autoload 'dired-jump "dired-x" nil t)

(eval-after-load "dired"
  '(define-key dired-mode-map (kbd "M-u")
     (lambda () (interactive) (find-alternate-file ".."))))

;; Open directory in the same buffer
(put 'dired-find-alternate-file 'disabled nil)

;;; defadvice settings
(defadvice kill-line (before check-position activate)
  "killing the newline between indented lines and remove extra
spaces."
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode python-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

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

;;; convenient functions
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

(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (when (nth 3 (syntax-ppss))
          (if (< arg 0)
              (progn
                (skip-syntax-forward "^\"")
                (goto-char (1+ (point)))
                (incf arg))
            (skip-syntax-backward "^\"")
            (goto-char (1- (point)))
            (decf arg)))
        (up-list (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(defun my/delete-char-or-region ()
  "hack `delete-char', delete char or region, skip kill ring."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'delete-char)))

;; http://xahlee.org/emacs/emacs_kill-ring.html
(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push erased text to `kill-ring'."
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

;;; keybinds
(global-set-key [remap delete-char]  'my/delete-char-or-region)        ;C-d
(global-set-key [remap move-beginning-of-line]  'my/beginning-of-line) ;C-a
(global-set-key [remap move-end-of-line]  'my/end-of-line)             ;C-e
(global-set-key [remap kill-line]  'my/delete-line)                    ;C-k
(global-set-key [remap backward-kill-word] 'my/backward-delete-word)   ;M-DEL, <C-backspace>

(global-set-key (kbd "M-d")     'my/delete-word)
(global-set-key (kbd "C-S-k")   'my/delete-line-backward)
(global-set-key (kbd "C-\\")    'my/comment-or-uncomment-region)
(global-set-key (kbd "C-1")     'extend-selection)
(global-set-key (kbd "C-2")     'set-mark-command)
(global-set-key (kbd "C-x C-2") 'pop-to-mark-command)
(global-set-key (kbd "C-m")     'newline-and-indent)
(global-set-key (kbd "C-j")     'newline)
(global-set-key (kbd "C-o")     'vi-open-next-line)
(global-set-key (kbd "C-M-o")   'split-line)
(global-set-key (kbd "M-0")     'other-window)
(global-set-key (kbd "M-5")     'my/display-buffer-path)
(global-set-key (kbd "M-'")     'just-one-space)
(global-set-key (kbd "M--")     'delete-blank-lines)
(global-set-key (kbd "M-J")     'vi-join-lines)
(global-set-key (kbd "C-M-j")   'vi-merge-lines)

(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "M-z")     'zap-up-to-char)

(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")   'isearch-forward)
(global-set-key (kbd "C-M-r")   'isearch-backward)

(global-set-key (kbd "C-c i")   'imenu)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x C-o") 'mode-line-other-buffer)

(setq custom-file "~/.emacs-custom.el")
(load custom-file t)
