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
(require 'lsp-mode)

;; Load only the production client registration, not unrelated GUI settings.
(let ((config (expand-file-name "../config.el"
                                (file-name-directory load-file-name))))
  (with-temp-buffer
    (insert-file-contents config)
    (goto-char (point-min))
    (search-forward "(defun +python-register-lsp-clients ()")
    (beginning-of-line)
    (eval (read (current-buffer)) t)))
(+python-register-lsp-clients)

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
