;;; python-lsp-test.el --- Python capability regression tests -*- lexical-binding: t; -*-

;; Run: emacs --batch -Q -l tests/python-lsp-test.el -f ert-run-tests-batch-and-exit
;; Uses installed lsp-mode, without loading Doom or starting language servers.
(require 'ert)
(setenv "LSP_USE_PLISTS" "1")
(unless (locate-library "lsp-mode")
  (let ((build-dir
         (expand-file-name
          (format "emacs/.local/straight/build-%s/" emacs-version)
          (or (getenv "XDG_CONFIG_HOME") "~/.config/"))))
    (dolist (dir (directory-files build-dir t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))
;; Evaluate the deferred definition before LSP loads, as real startup does.
(defmacro after! (feature &rest body)
  `(with-eval-after-load ',feature ,@body))
(let ((config (expand-file-name "../config.el"
                                (file-name-directory load-file-name))))
  (with-temp-buffer
    (insert-file-contents config)
    (goto-char (point-min))
    (search-forward "(after! lsp-mode\n  ;; Quoting prevents")
    (beginning-of-line)
    ;; Loading config.el eagerly expands nested macros before LSP is present.
    (eval (macroexpand-all (read (current-buffer))) t)))
(require 'lsp-mode)
(require 'flycheck)
(require 'lsp-diagnostics)
(+python-register-lsp-clients)

(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../config.el" (file-name-directory load-file-name)))
  (goto-char (point-min))
  (search-forward "(defun +python-start-lsp-h ()")
  (beginning-of-line)
  (eval (read (current-buffer)) t))

(defvar python-test-modules-enabled nil)
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../config.el" (file-name-directory load-file-name)))
  (goto-char (point-min))
  (search-forward "(defun +python-prepare-lsp-checker-h ()")
  (beginning-of-line)
  (eval (read (current-buffer)) t))

(defmacro modulep! (&rest _) 'python-test-modules-enabled)
(defvar +python-opening-visible-file nil)
(ert-deftest python-lsp-visible-start-does-not-wait-for-idle ()
  (dolist (visible '(t nil))
    (with-temp-buffer
      (let ((lsp-mode nil) started deferred)
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) visible))
                  ((symbol-function 'lsp) (lambda () (setq started t)))
                  ((symbol-function 'lsp-deferred) (lambda () (setq deferred t))))
          (+python-start-lsp-h)
          (should (eq started visible))
          (should (eq deferred (not visible))))))))

(ert-deftest python-lsp-file-open-starts-before-display ()
  (let ((lsp-mode nil) (+python-opening-visible-file t) started)
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
              ((symbol-function 'lsp) (lambda () (setq started t))))
      (+python-start-lsp-h)
      (should started))))

(ert-deftest python-lsp-visible-start-skips-connected-buffer ()
  (let ((lsp-mode t))
    (cl-letf (((symbol-function 'lsp) (lambda () (ert-fail "Unexpected LSP start")))
              ((symbol-function 'lsp-deferred) (lambda () (ert-fail "Unexpected deferred start"))))
      (+python-start-lsp-h))))

;; Exercise the same deferred configuration block that real Doom evaluates.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../config.el" (file-name-directory load-file-name)))
  (goto-char (point-min))
  (search-forward "(defun +python-load-lsp-client-bundle-a ")
  (beginning-of-line)
  (eval (read (current-buffer)) t))

(let ((config (expand-file-name "../config.el"
                                (file-name-directory load-file-name))))
  (with-temp-buffer
    (insert-file-contents config)
    (goto-char (point-min))
    (search-forward "(after! lsp-mode\n  ;; Never block")
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)) t)))

(ert-deftest python-lsp-registration-survives-deferred-client-bundle ()
  (let ((lsp--client-packages-required nil)
        (lsp-client-packages '(lsp-python-ty lsp-ruff))
        (lsp-clients (make-hash-table :test 'eql)))
    ;; Simulate the built-in registration replacing ours during bundle load.
    (puthash 'ty-ls (make-lsp-client :server-id 'ty-ls :add-on? nil)
             lsp-clients)
    (lsp--require-packages)
    (should (gethash 'jedi lsp-clients))
    (should (lsp--client-add-on? (gethash 'ty-ls lsp-clients)))))

(ert-deftest python-lsp-small-bundle-preserves-other-languages ()
  (let ((lsp--client-packages-required nil)
        (lsp-client-packages '(lsp-bash lsp-clangd lsp-python-ty lsp-ruff))
        (lsp-clients (make-hash-table :test 'eql))
        (real-featurep (symbol-function 'featurep))
        loaded)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (push feature loaded)))
              ((symbol-function 'featurep)
               (lambda (feature &rest args)
                 (unless (memq feature '(lsp-bash lsp-clangd lsp-python-ty lsp-ruff))
                   (apply real-featurep feature args)))))
      (with-temp-buffer
        (setq major-mode 'python-mode)
        (lsp--require-packages)
        (should (equal (delete-dups (nreverse loaded)) '(lsp-python-ty lsp-ruff)))
        (should-not lsp--client-packages-required)
        (should (gethash 'jedi lsp-clients))
        (setq loaded nil major-mode 'sh-mode)
        (lsp--require-packages)
        (should lsp--client-packages-required)
        (should (memq 'lsp-bash loaded))
        (should (memq 'lsp-clangd loaded))
        (should (lsp--client-add-on? (gethash 'ty-ls lsp-clients)))))))

(defun python-lsp-test--workspace (caps)
  "Initialize a ty workspace with CAPS using the production callback."
  (let* ((client (gethash 'ty-ls lsp-clients))
         (workspace (make-lsp--workspace :client client
                                        :server-capabilities caps)))
    (funcall (lsp--client-initialized-fn client) workspace)
    workspace))

(ert-deftest python-lsp-ty-removes-disabled-capabilities ()
  ;; Put disabled keys first: deleting a plist head must update the workspace.
  (let* ((workspace (python-lsp-test--workspace
                     (list :completionProvider '(:triggerCharacters ["."])
                           :referencesProvider t :hoverProvider t
                           :diagnosticProvider '(:interFileDependencies t)
                           :inlayHintProvider t)))
         (caps (lsp--workspace-server-capabilities workspace)))
    (should-not (lsp-member? caps :completionProvider))
    (should-not (lsp-member? caps :referencesProvider))
    (should (lsp-get caps :hoverProvider))
    (should (lsp-get caps :diagnosticProvider))
    (should (lsp-get caps :inlayHintProvider))))

(ert-deftest python-lsp-completion-and-references-route-only-to-jedi ()
  (let* ((ty (python-lsp-test--workspace
             (list :completionProvider nil :referencesProvider t
                   :hoverProvider t :inlayHintProvider t)))
         (jedi (make-lsp--workspace
                :client (gethash 'jedi lsp-clients)
                :server-capabilities
                (list :completionProvider '(:triggerCharacters ["."])
                      :referencesProvider t :hoverProvider t)))
         (lsp--cur-workspace nil)
         (lsp--buffer-workspaces (list ty jedi)))
    (should (equal (lsp--find-workspaces-for "textDocument/completion")
                   (list jedi)))
    (should (equal (lsp--find-workspaces-for "textDocument/references")
                   (list jedi)))
    (should (equal (lsp--find-workspaces-for "textDocument/hover")
                   (list ty jedi)))
    (should (equal (lsp--find-workspaces-for "textDocument/inlayHint")
                   (list ty)))))

(ert-deftest python-lsp-ty-keeps-inlay-fallback-and-is-an-addon ()
  (let* ((workspace (python-lsp-test--workspace (list :hoverProvider t)))
         (caps (lsp--workspace-server-capabilities workspace)))
    (should (lsp--client-add-on? (lsp--workspace-client workspace)))
    (should (lsp-get caps :inlayHintProvider))
    (should-not (lsp-member? caps :completionProvider))
    (should-not (lsp-member? caps :referencesProvider))))

(ert-deftest python-lsp-preselect-checker-respects-user-and-server-failure ()
  (dolist (connected '(t nil))
    (dolist (checker '(nil python-mypy))
      (with-temp-buffer
        (let ((lsp--buffer-workspaces connected)
              (python-test-modules-enabled t)
              configured)
          (setq-local flycheck-checker checker)
          (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                    ((symbol-function 'lsp-diagnostics-lsp-checker-if-needed) #'ignore)
                    ((symbol-function 'lsp-flycheck-add-mode)
                     (lambda (&rest _) (setq configured t))))
            (+python-prepare-lsp-checker-h)
            (should (eq (and configured t) (and connected (not checker))))))))))

(ert-deftest python-lsp-preselect-checker-respects-disabled-diagnostics ()
  (dolist (settings '((:none t nil) (:flymake t nil) (:auto nil nil)
                      (:auto t (fundamental-mode))))
    (with-temp-buffer
      (let ((lsp--buffer-workspaces '(pending))
            (python-test-modules-enabled t)
            (lsp-diagnostics-provider (nth 0 settings))
            (lsp-auto-configure (nth 1 settings))
            (lsp-diagnostics-disabled-modes (nth 2 settings)))
        (cl-letf (((symbol-function 'lsp-flycheck-add-mode)
                   (lambda (&rest _) (ert-fail "Diagnostics should stay disabled"))))
          (+python-prepare-lsp-checker-h)
          (should-not flycheck-checker))))))
