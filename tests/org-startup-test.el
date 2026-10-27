;;; org-startup-test.el --- Org first-use tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(defvar org-test-syntax-enabled t)
(defmacro modulep! (&rest _) 'org-test-syntax-enabled)

(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../config.el" (file-name-directory load-file-name)))
  (goto-char (point-min))
  (search-forward "(defun +org-prepare-lint-h ()")
  (beginning-of-line)
  (eval (read (current-buffer)) t))

(ert-deftest org-startup-prepares-in-process-checker-before-input ()
  (let (loaded)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (push feature loaded))))
      (+org-prepare-lint-h)
      (should (equal (nreverse loaded) '(org-lint ox-beamer))))))

(ert-deftest org-startup-skips-disabled-flycheck-module ()
  (let ((org-test-syntax-enabled nil))
    (cl-letf (((symbol-function 'require)
               (lambda (&rest _) (ert-fail "Unexpected lint dependencies"))))
      (+org-prepare-lint-h))))
