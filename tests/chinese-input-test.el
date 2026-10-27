;;; chinese-input-test.el --- Chinese writing context tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)

;; Use installed packages, not the bundled Org or a mocked input engine.
(let ((build (expand-file-name
              (format "~/.config/emacs/.local/straight/build-%d.%d/"
                      emacs-major-version emacs-minor-version))))
  (dolist (directory (directory-files build t "^[^.]"))
    (when (file-directory-p directory)
      (add-to-list 'load-path directory))))
(require 'org)
(require 'pyim-probe)
(require 'python)

(defconst chinese-test-config
  (expand-file-name "../config.el" (file-name-directory load-file-name)))

(defun chinese-test-eval-form (start)
  (with-temp-buffer
    (insert-file-contents chinese-test-config)
    (search-forward start)
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)) t)))

(defmacro after! (_feature &rest body) `(progn ,@body))
(chinese-test-eval-form "(defun +chinese-probe-org-src-block ")

(defmacro chinese-test-with-probes (&rest body)
  `(let ((pyim-english-input-switch-functions nil))
     (chinese-test-eval-form "(after! pyim\n")
     ,@body))

(ert-deftest chinese-input-probes-preserve-existing-settings-and-are-idempotent ()
  (let ((pyim-english-input-switch-functions '(ignore)))
    (dotimes (_ 2) (chinese-test-eval-form "(after! pyim\n"))
    (should (equal pyim-english-input-switch-functions
                   '(ignore pyim-probe-program-mode
                     +chinese-probe-org-src-block pyim-probe-org-latex-mode)))))

(ert-deftest chinese-input-code-is-english-but-comments-and-strings-allow-chinese ()
  (chinese-test-with-probes
   (with-temp-buffer
     (python-mode)
     (insert "value = foo\n# comment\nlabel = \"text\"\n")
     (goto-char (point-min))
     (search-forward "foo")
     (should (pyim-process-auto-switch-english-input-p))
     (search-forward "comment")
     (should-not (pyim-process-auto-switch-english-input-p))
     (search-forward "text")
     (should-not (pyim-process-auto-switch-english-input-p)))))

(ert-deftest chinese-input-org-code-and-math-are-english-but-prose-is-not ()
  (chinese-test-with-probes
   (with-temp-buffer
     (org-mode)
     (insert "Prose before.\n#+begin_src python\nvalue = foo\n#+end_src\n"
             "Prose after.\nInline $x + y$ done.\n"
             "Display \\(a + b\\) done.\n"
             "\\begin{equation}\nc = d\n\\end{equation}\n"
             "Macro \\alpha{} done.\nFinal prose.\n")
     (goto-char (point-min))
     (dolist (sample '(("before" . nil) ("foo" . t) ("after" . nil)
                       ("x +" . t) ("a +" . t) ("c =" . t)
                       ("\\alpha{" . t) ("Final prose" . nil)))
       (search-forward (car sample))
       (ert-info ((format "Context after %s" (car sample)))
         (should (eq (not (null (pyim-process-auto-switch-english-input-p)))
                     (cdr sample))))))))

(ert-deftest chinese-input-prose-does-not-load-org ()
  (with-temp-buffer
    (text-mode)
    (cl-letf (((symbol-function 'org-in-src-block-p)
               (lambda (&rest _) (ert-fail "Unexpected Org parsing"))))
      (should-not (+chinese-probe-org-src-block)))))

(ert-deftest chinese-wrapping-changes-display-only ()
  (let ((previous (default-value 'word-wrap-by-category)))
    (unwind-protect
        (progn
          (chinese-test-eval-form "(setq-default word-wrap-by-category t)")
          (with-temp-buffer
            (text-mode)
            (insert "\u4e2d\u6587English\u6df7\u6392")
            (set-buffer-modified-p nil)
            (visual-line-mode 1)
            (should word-wrap-by-category)
            (should word-wrap)
            (should-not truncate-lines)
            (should-not (buffer-modified-p))
            (should (equal (buffer-string) "\u4e2d\u6587English\u6df7\u6392"))))
      (setq-default word-wrap-by-category previous))))
