;; -*- emacs-lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C++
(use-package buffer-action
  :commands (buffer-action-compile buffer-action-run)
  :bind*
  ("C-c c r" . buffer-action-run)
  ("C-c c s" . buffer-action-compile))

(use-package compile
  :defer
  :config
  (setq compilation-auto-jump-to-first-error t
        compilation-scroll-output t)
  ;;# Close complication buffer if succeed to compile
  (setq compilation-finish-functions
        (lambda (buf str)
          (when (and (string= (buffer-name buf) "*compilation*")
                     (not (string-match "exited abnormally" str))
                     (not (string-match "warning" str)))
            (run-at-time 0.5 nil 'delete-windows-on buf)))))

(use-package hideif
  :defer
  :config
  (setq hide-ifdef-initially t
        hide-ifdef-shadow t))
