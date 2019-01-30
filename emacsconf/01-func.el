;; -*- emacs-lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end

(defun my/indent-marked-files ()
  "Firstly mark files in `dired-mode', then indent them."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

(defun my/format-region ()
  "Format region, if no region actived, format current buffer.
Like eclipse's Ctrl+Alt+F."
  (interactive)
  (let ((start (point-min))
        (end (point-max)))
    (if (and (fboundp 'region-active-p) (region-active-p))
        (progn (setq start (region-beginning))
               (setq end (region-end)))
      (progn (when (fboundp 'whitespace-cleanup)
               (whitespace-cleanup))
             (setq end (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-min) end)
        (push-mark (point))
        (push-mark (point-max) nil t)
        (goto-char start)
        (when (fboundp 'whitespace-cleanup)
          (whitespace-cleanup))
        (untabify start (point-max))
        (indent-region start (point-max) nil)))))

(defun my/byte-recompile-directory-recursively (&optional specified)
  "Recompile all the .el files under DIR, if they're not up to
date. It can also be run from the command line:

$ emacs -l ~/.emacs -batch -f my/byte-recompile-directory-recursively"
  (interactive
   (list current-prefix-arg))
  (let ((spec-dir (if specified (read-directory-name "Byte compile the directory recursively: ")
                    my/startup-dir)))
    (dolist (dir (find-subdirs-containing spec-dir "\\.el$"))
      (byte-recompile-directory dir 0))))

(defun my/count-ce-word (beg end)
  "Count Chinese and English words in marked region."
  (interactive
   (if (and mark-active transient-mark-mode)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end)
          total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (cn: %d, en: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))

(defun my/print-to-pdf (file)
  "Print current buffer to a pdf file."
  (interactive
   (list (read-file-name "Choose a filename: ")))
  (let ((psfile (make-temp-file "ps"))
        (pdffile (if (string= "pdf" (file-name-extension file))
                     file
                   (format "%s.pdf" file))))
    (ps-spool-buffer-with-faces)
    (switch-to-buffer ps-spool-buffer-name)
    (write-file psfile)
    (shell-command (format "ps2pdf %s %s" psfile pdffile))
    (kill-buffer (file-name-nondirectory psfile))
    (delete-file psfile)
    (message "Saved to: %s" pdffile)))

(defun my/insert-date ()
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun my/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(defun my/clone-buffer (non-indirect)
  "If with prefix argument, clone buffer, other wise, clone
indirect buffer. bind to \\[my/clone-buffer]."
  (interactive "P")
  (if non-indirect
      (call-interactively 'clone-buffer)
    (let ((indir-bufs (mapcar (lambda (buf) (cons buf (buffer-base-buffer buf)))
                              (remove-if-not 'buffer-base-buffer (buffer-list))))
          buf)
      (if (setq buf (assoc (current-buffer) indir-bufs))
          (select-window (display-buffer (cdr buf)))
        (if (setq buf (rassoc (current-buffer) indir-bufs))
            (select-window (display-buffer (car buf)))
          (setq current-prefix-arg nil)
          (call-interactively 'clone-indirect-buffer-other-window))))))

(defun kill-buffer-when-shell-command-exit ()
  "Close current buffer when `shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel
       process
       (lambda (proc change)
         (when (string-match "\\(finished\\|exited\\)" change)
           (kill-buffer (process-buffer proc))))))))

(defun emacs-process-duplicated-p ()
  "Check whether another emacs process is running concorrently by
pgrep, so.. make sure pgrep is already installed in your system."
  (if (executable-find "pgrep")
      (save-excursion
        (let ((buffer (generate-new-buffer (generate-new-buffer-name "*check emacs process*")))
              (process-number 0))
          (set-buffer buffer)
          (when (= 0 (call-process "pgrep" nil t nil "emacs"))
            ;; (setq pid (buffer-substring (point-min) (1- (point-max))))
            (goto-char (point-min))
            (require 'cl)
            (while (search-forward-regexp "^[0-9]+$" nil t)
              (incf process-number)))
          (kill-buffer buffer)
          (> process-number 1)))))

(defun compact-uncompact-block ()
  "Remove or add line endings on the current block of text.
This command similar to a toggle for `fill-paragraph' and `unfill-paragraph'
When there is a text selection, act on the region.

When in text modes, the “current block” is equivalent to the
current paragraph.  When in programing language modes, “current block”
is defined by between empty lines.

Todo: when in a programing lang mode, make the function more
smart, so that it doesn't cut strings.  Right now, the code uses
fill* functions. A proper implementation to compact is replacing
newline chars by space when the newline char is not inside
string. To uncompact, a proper solution needs to know the basic
syntax of each lang. A simple implementation is to simply insert
newline after “}” or “;” for c-like syntaxes."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”, the
  ;; possible values are t and nil. This property is used to easily
  ;; determine whether to compact or uncompact, when this command is
  ;; called again

  (let (bds currentLineCharCount currentStateIsCompact
            (bigFillColumnVal 4333999) (deactivate-mark nil))

    (save-excursion
      ;; currentLineCharCount is used to determine whether current state
      ;; is compact or not, when the command is run for the first time
      (setq currentLineCharCount
            (progn
              (setq bds (bounds-of-thing-at-point 'line))
              (length (buffer-substring-no-properties (car bds) (cdr bds)))
              ;; Note: 'line includes eol if it is not buffer's last line
              ))

      ;; Determine whether the text is currently compact.  when the last
      ;; command is this, then symbol property easily tells, but when
      ;; this command is used fresh, right now we use num of chars of
      ;; the cursor line as a way to define current compatness state
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> currentLineCharCount fill-column) t nil)))

      (if (and transient-mark-mode mark-active)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))))
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil))))

      (put this-command 'stateIsCompact-p
           (if currentStateIsCompact
               nil t)) ) ) )
