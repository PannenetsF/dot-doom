;;; org-latex-preview-test.el --- Preview subprocess PATH tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(defvar org-preview-latex-default-process)
(defvar org-format-latex-options)
(defconst org-preview-test-config
  (expand-file-name "../config.el" (file-name-directory load-file-name)))

(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../config.el" (file-name-directory load-file-name)))
  (goto-char (point-min))
  (search-forward "(defun +org-latex-preview-shell-a ")
  (beginning-of-line)
  (eval (read (current-buffer)) t))

(ert-deftest org-latex-preview-preserves-path-despite-zshenv ()
  (skip-unless (file-executable-p "/bin/zsh"))
  (let* ((root (make-temp-file "org-preview-shell-" t))
         (tool (expand-file-name "org-preview-test-tool" root))
         (process-environment (copy-sequence process-environment))
         (shell-file-name "/bin/zsh")
         (shell-command-switch "-c"))
    (unwind-protect
        (progn
          (with-temp-file tool (insert "#!/bin/sh\nprintf preview-ok"))
          (set-file-modes tool #o700)
          (with-temp-file (expand-file-name ".zshenv" root)
            (insert "export PATH=/usr/bin:/bin\n"))
          (setenv "ZDOTDIR" root)
          (setenv "PATH" (concat root ":/usr/bin:/bin"))
          ;; Reproduce Org's old path: TeX passes executable-find, but Zsh
          ;; discards that PATH before actually running the command.
          (should (string-match-p "not found"
                                  (shell-command-to-string "org-preview-test-tool 2>&1")))
          (should (equal (+org-latex-preview-shell-a
                          #'shell-command-to-string "org-preview-test-tool")
                         "preview-ok"))
          (should (equal shell-file-name "/bin/zsh")))
      (delete-directory root t))))

(ert-deftest org-latex-preview-shell-binding-unwinds-on-error ()
  (let ((shell-file-name "/bin/zsh") (shell-command-switch "-custom"))
    (should-error (+org-latex-preview-shell-a (lambda () (error "Compile failed"))))
    (should (equal shell-file-name "/bin/zsh"))
    (should (equal shell-command-switch "-custom"))))

(ert-deftest org-latex-preview-preserves-scale-with-transparent-vector-images ()
  (let ((org-preview-latex-default-process 'dvipng)
        (org-format-latex-options '(:scale 2.5 :background default :foreground default)))
    (with-temp-buffer
      (insert-file-contents org-preview-test-config)
      (goto-char (point-min))
      (search-forward "(setq org-preview-latex-default-process ")
      (goto-char (match-beginning 0))
      (eval (read (current-buffer)) t))
    (should (eq org-preview-latex-default-process 'dvisvgm))
    (should (= (plist-get org-format-latex-options :scale) 2.5))
    (should (equal (plist-get org-format-latex-options :background) "Transparent"))
    (should (eq (plist-get org-format-latex-options :foreground) 'default))))
