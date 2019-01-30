;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; abbrev
(use-package abbrev
  :defer
  :config
  ;; Digested from (Emacswiki)[http://www.emacswiki.org/emacs/AbbrevMode#toc7]
  (require 'cl)
  (defvar my/abbrev-tables nil)
  (defun my/abbrev-hook ()
    (let ((def (assoc (symbol-name last-abbrev) my/abbrev-tables)))
      (when def
        (execute-kbd-macro (cdr def)))
      t))
  (put 'my/abbrev-hook 'no-self-insert t)
  (defmacro declare-abbrevs (table abbrevs)
    (if (consp table)
        `(progn ,@(loop for tab in table
                        collect `(declare-abbrevs ,tab ,abbrevs)))
      `(progn
         ,@(loop for abbr in abbrevs
                 do (when (third abbr)
                      (push (cons (first abbr) (read-kbd-macro (third abbr)))
                            my/abbrev-tables))
                 collect `(define-abbrev ,table
                            ,(first abbr) ,(second abbr) ,(and (third abbr)
                                                               ''my/abbrev-hook))))))
  (put 'declare-abbrevs 'lisp-indent-function 2)

  (with-eval-after-load "sh-script"
    (declare-abbrevs sh-mode-abbrev-table
        (("redx" "\033[1;31m\033[0m" "C-u 4 C-b")
         ("greenx" "\033[1;32m\033[0m" "C-u 4 C-b")
         ("bluex" "\033[1;34m\033[0m" "C-u 4 C-b"))))

  ;; define global abbrev
  (define-abbrev-table 'global-abbrev-table
    '(("alpha" "α" nil 0)
      ("beta" "β" nil 0)
      ("gamma" "γ" nil 0)
      ("delta" "δ" nil 0)
      ("epsilon" "ε" nil 0)
      ("zeta" "ζ" nil 0)
      ("nu" "ν" nil 0)
      ("xi" "ξ" nil 0)
      ("omicron" "ο" nil 0)
      ("pi" "π" nil 0)
      ("rho" "ρ" nil 0)
      ("sigma" "σ" nil 0)
      ("eta" "η" nil 0)
      ("theta" "θ" nil 0)
      ("iota" "ι" nil 0)
      ("kappa" "κ" nil 0)
      ("lambada" "λ" nil 0)            ; avoid conflict with lambda
      ("mu" "μ" nil 0)
      ("tau" "τ" nil 0)
      ("upsilon" "υ" nil 0)
      ("phi" "ϕ" nil 0)
      ("chi" "χ" nil 0)
      ("psi" "ψ" nil 0)
      ("omega" "ω" nil 0)
      ;; upper
      ("Delta" "Δ" nil 0)
      ("Pi" "Π" nil 0)
      ("Sigma" "Σ" nil 0)
      ("Theta" "Θ" nil 0)
      ("Omega" "Ω" nil 0)
      ;; arrow
      ("inf" "∞" nil 0)
      ("ar1" "→" nil 0)
      ("ar2" "⇒" nil 0)
      ("ra1" "←" nil 0)
      ("gt" "»" nil 0)
      ("lt" "«" nil 0)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; skeleton
(use-package skeleton
  :defer
  :config
  ;; avoid abbre-mode when expand skeleton
  (setq skeleton-further-elements '((abbrev-mode nil))
        skeleton-end-hook nil)

  (defmacro define-skel-comment (name comment-start comment-end
                                      &optional char-to-fill)
    "Define a skeleton to insert one line comment as a
`fill-column' wide rectangle into current buffer.

For example: (define-skel-comment \"elisp\" \";;\" \";;\" ?\\;)
"
    (declare (debug t) (indent 2))
    (let ((char-to-fill (or char-to-fill ?*))
          (padding-length (+ (length comment-start) (length comment-end))))
      `(define-skeleton ,(intern (format "skel-%s-comment" name))
         ,(format "Insert a %s comment as a rectangle" name)
         ""
         '(setq str (skeleton-read "Comment: "))
         '(when (string= str "") (setq str " - "))
         '(setq v1 (make-string (- fill-column ,(+ padding-length 2))
                                ,char-to-fill))
         '(setq v2 (- fill-column ,(+ padding-length 6) (length str)))
         ,comment-start " " v1 " " ,comment-end \n
         ,comment-start " " ,(make-string 2 char-to-fill)
         (make-string (floor v2 2) ?\ )
         str
         (make-string (ceiling v2 2) ?\ )
         ,(make-string 2 char-to-fill) " " ,comment-end \n
         ,comment-start " " v1 " " ,comment-end)))

  (define-skel-comment "elisp" ";;" ";;" ?\;)
  (define-skel-comment "c" "/*" "*/")
  (define-skel-comment "c++" "//" "//" ?/)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto insert
(use-package autoinsert
  :config
  ;; Disable auto-insert-mode, it causes trouble with global-auto-revert-mode
  ;; (auto-insert-mode 1)
  (setq auto-insert-query 'function
        auto-insert 'other)

  (define-auto-insert '("\\.h$" . "C/C++ header")
    '((let* ((modes '("c" "c++"))
             (selected (ido-completing-read "C or C++ header? : " modes nil nil nil nil (car modes))))
        selected)
      "/* -*- mode: " str | -13 " -*-" ?\n
      (my/common-header " * ")
      " */" ?\n ?\n
      "#ifndef "
      (setq v1 (my/ifndef-header-guard-string))
      ?\n
      "#define " v1 "\n\n"
      _
      "\n\n#endif // " v1
      '(progn (set-auto-mode))))

  (define-auto-insert '("\\.\\(hh\\|hpp\\)$" . "C++ header")
    '(nil
      "// -*- mode: c++ -*-" ?\n
      (my/common-header "// ")
      "//" ?\n ?\n
      "#ifndef "
      (setq v1 (my/ifndef-header-guard-string))
      ?\n
      "#define " v1 "\n\n"
      _
      "\n\n#endif // " v1))

  (define-auto-insert '("\\.c$" . "C program")
    '(nil
      "/* -*- mode: c -*-" ?\n
      (my/common-header " * ")
      " */" ?\n ?\n
      "#include \""
      (let ((stem (file-name-sans-extension buffer-file-name)))
        (if (file-exists-p (concat stem ".h"))
            (file-name-nondirectory (concat stem ".h"))))
      & "\"\n" | -10
      _))

  (define-auto-insert '("\\.\\(cc\\|cpp\\)$" . "C++ program")
    '(nil
      "// -*- mode: c++ -*-" ?\n
      (my/common-header "// ")
      "//" ?\n ?\n
      "#include \""
      (let ((stem (file-name-sans-extension buffer-file-name)))
        (cond ((file-exists-p (concat stem ".h"))
               (file-name-nondirectory (concat stem ".h")))
              ((file-exists-p (concat stem ".hpp"))
               (file-name-nondirectory (concat stem ".hpp")))
              ((file-exists-p (concat stem ".hh"))
               (file-name-nondirectory (concat stem ".hh"))))
        )
      & "\"\n" | -10
      _))

  (define-auto-insert '("\\.ede$" . "Project.ede")
    '("Project name: "
      "-*- mode: emacs-lisp -*-\n"
      "(ede-cpp-root-project \"" str "\"\n"
      > ":name \"" str "\"\n"
      > ":file \"" (file-name-directory buffer-file-name) "Makefile\"\n"
      > ":include-path '(\"" (read-string "project include path: ")"\"" _ ")\n"
      > ":system-include-path '(\"/usr/local/include\")\n"
      > ":spp-table '((\"DEBUG\" . \"\")\n"
      > "(\"SYMBOL\" . \"\")\n"
      > "(\"ZERO\" . 0)\n"
      > "(\"ONE\" . 1)\n"
      > "(\"PROJECT_NAME\" . \"" str "\")\n"
      > "(\"PROJECT_VERSION\" . \"1.0.0\")))"))

  ;; (define-auto-insert '(makefile-mode . "Makefile")
  ;;   ["makefile.tpl"])

  (define-auto-insert '(makefile-mode . "Makefile")
    '(nil
      (my/common-header "# ")
      "\n" _))
  (define-auto-insert '("make\\.inc$" . "make.inc")
    ["make.inc"])
  (define-auto-insert '("make\\.rules$" . "make.rules")
    ["make.rules"])

  (define-auto-insert '(python-mode . "Python script")
    '(nil
      "#!/usr/bin/env python" ?\n
      "# -*- coding: utf-8; tab-width: 4; -*-" ?\n
      (my/common-header "# ")
      "#\n\n"
      ;; "import sys" ?\n ?\n
      ;; "def main():" ?\n
      > _ ?\n ?\n
      ;; "if __name__ == \"__main__\":" ?\n
      ;; > "main()"
      ))

  (define-auto-insert '(php-mode . "PHP script")
    '(nil
      "<?php" ?\n
      (my/common-header "// ")
      "//\n\n"
      _ ?\n ?\n
      "?>"
      ))

  (define-auto-insert '(sh-mode . "Shell script")
    '(nil
      "#!/usr/bin/env bash" ?\n
      (my/common-header "# ")
      "#\n\n"
      "set -e -o pipefail\n\n"
      _
      ))

  (define-auto-insert '(sql-mode . "SQL script")
    '(nil
      "-- -*- coding: utf-8; tab-width: 2; -*-" ?\n
      (my/common-header "-- ")
      "--\n\n"
      _
      ))

  (define-auto-insert '(org-mode . "Org document")
    '("Title: "
      "#+TITLE: " str & ?\n | -9
      "#+AUTHOR: " (progn user-full-name) ?\n
      "#+EMAIL: " (progn user-mail-address) ?\n
      "#+DATE: " (format-time-string "%Y-%m-%d") ?\n
      (let* ((modes '("org" "latex" "beamer"))
             (selected (ido-completing-read "Which kind of document? : " modes nil nil nil nil (car modes))))
        (if (string= selected "org")
            ""
          (concat "#+LATEX_HEADER: \\setmainfont{Big Caslon}\n"
                  "#+LATEX_HEADER: \\setsansfont{Optima}\n"
                  "#+LATEX_HEADER: \\setmonofont{American Typewriter}\n"
                  "#+LATEX_HEADER: \\setCJKmainfont{Kai}\n"
                  "#+LATEX_HEADER: \\setCJKsansfont{Hei}\n"
                  "#+LATEX_HEADER: \\setCJKmonofont{STFangsong}\n"
                  (if (string= selected "beamer")
                      (concat "#+LATEX_CLASS_OPTIONS: [presentation]\n"
                              "#+BEAMER_FRAME_LEVEL: "
                              "#+BEAMER_HEADER_EXTRA: \\usetheme{"
                              (let ((themes '("default"
                                              "Berkeley"
                                              "CambridgeUS"
                                              "Frankfurt"
                                              "PaloAlto"
                                              "Montpellier"
                                              "Pittsburgh"
                                              "Rochester"
                                              "boxes"
                                              "Goettingen")))
                                (ido-completing-read "Select a theme: " themes nil nil nil nil (car themes)))
                              "}"
                              "\\usecolortheme{"
                              (let ((colors '("default"
                                              "albatross"
                                              "beaver"
                                              "beetle"
                                              "crane"
                                              "dolphin"
                                              "dove"
                                              "fly"
                                              "lily"
                                              "orchid"
                                              "rose"
                                              "seagull"
                                              "seahorse"
                                              "sidebartab"
                                              "structure"
                                              "whale"
                                              "wolverine"
                                              "default")))
                                (ido-completing-read "Select a color: " colors nil nil nil nil (car colors)))
                              "}\n"
                              "#+COLUMNS: %35ITEM %10BEAMER_env(Env) %10BEAMER_envargs(Env Args) %4BEAMER_col(Col) %8BEAMER_extra(Extra)\n"
                              "#+OPTIONS: tags:nil\n"))) ) )
      ?\n _ ?\n ?\n
      "#+COMMENT: Local Variables:" ?\n
      "#+COMMENT: mode: org" ?\n
      "#+COMMENT: coding: utf-8" ?\n
      "#+COMMENT: fill-column: 78" ?\n
      "#+COMMENT: End:")
    )
  ;; helper functions
  (defun my/common-header (comment-string &optional encoding)
    (concat
     (mapconcat (lambda (line) (concat comment-string line))
                `(
                  ;; ,(format "@(#) %s %s Time-stamp: <>"
                  ;;          (file-name-nondirectory (buffer-file-name))
                  ;;          (if encoding (concat " -*- coding: " encoding " -*-") ""))
                  ,(if encoding (concat " -*- coding: " encoding " -*-") "")
                  ,(format "Copyright (C) %s %s"
                           (substring (current-time-string) -4)
                           (or (getenv "ORGANIZATION") user-full-name))
                  ""
                  ,(format "@file      %s"
                           (file-name-nondirectory (buffer-file-name)))
                  ,(format "@author    %s <%s>"
                           user-full-name
                           user-mail-address)
                  ,(format "@created   %s"
                           (format-time-string "%Y-%m-%d %H:%M:%S"))
                  ;; ,(format "$Id: %s, v0.1 %s %s $"
                  ;;          (file-name-nondirectory (buffer-file-name))
                  ;;          (format-time-string "%Y-%m-%d %H:%M:%S")
                  ;;          (user-login-name))
                  )
                "\n")
     "\n"))
  ;;# copy from template-simple.el
  (defun my/update-header ()
    "Auto update filename in header, refer to `my/common-header'."
    (interactive)
    (when buffer-file-name
      (save-excursion
        (goto-char (point-min))
        (let ((end (progn (forward-line 10) (point))) ; check only first 10 lines in header
              (regexp "@file[ ]+\\([^ \n]+\\)") ; refer: `my/common-header'
              (fn (file-name-sans-versions (file-name-nondirectory buffer-file-name))))
          (goto-char (point-min))
          (while (search-forward-regexp regexp end t)
            (and (not (string= (match-string 1) fn))
                 ;; (y-or-n-p (format "Update file header %s to %s? "
                 ;;                   (match-string 1) fn))
                 (message "Replace filename %s to %s in header"
                          (match-string 1) fn)
                 (replace-match fn nil t nil 1)))))))
  (add-hook 'before-save-hook 'my/update-header)
  (defun my/ifndef-header-guard-string ()
    "ifndef header guard for BLADE"
    (let ((blade-root (locate-dominating-file buffer-file-name "BLADE_ROOT")))
      (concat (upcase (replace-regexp-in-string
                       "[^a-zA-Z0-9]" "_"
                       (if blade-root
                           (substring buffer-file-name (length (expand-file-name blade-root)))
                         (file-name-nondirectory buffer-file-name))))
              "_")))
  )
