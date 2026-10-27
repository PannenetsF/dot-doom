;;; startup-latency-probe.el --- Real command-loop probe -*- lexical-binding: t; -*-
;; Loaded by startup-latency.py in a disposable interactive terminal Emacs.
(require 'json)
(defvar startup-probe-root (getenv "DOOM_STARTUP_PROBE_ROOT"))
(defvar startup-probe-armed nil)
(defvar startup-probe-start nil)
(defvar startup-probe-samples nil)
(defvar startup-probe-input-events nil)
(defvar startup-probe-loads nil)
(defvar startup-probe-timers nil)
(defvar startup-probe-profile nil)
(defvar startup-probe-gc 0)
(defvar startup-probe-gc-time 0)
(defvar startup-probe-native nil)
(defvar startup-probe-feature-ms 0)
(defvar startup-probe-feature-checks nil)
(defvar startup-probe-buffer nil)
(defvar startup-probe-stage "startup")
(defvar startup-probe-python-wait-start nil)
(defvar startup-probe-servers-ready nil)
(defvar startup-probe-python-check-scheduled nil)
(defvar startup-probe-input-finished nil)
(defvar startup-probe-feature-finished nil)
(defvar startup-probe-diagnostics-verified nil)

(defun startup-probe-write (name object)
  (with-temp-file (expand-file-name name startup-probe-root)
    (insert (json-encode object))))

(defun startup-probe-require (fn feature &rest args)
  (let ((start (float-time)))
    (prog1 (apply fn feature args)
      (when startup-probe-stage
        (let ((ms (* 1000 (- (float-time) start))))
          (when (> ms 5)
            (push `((feature . ,(symbol-name feature))
                    (stage . ,startup-probe-stage) (ms . ,ms))
                  startup-probe-loads)))))))

(defun startup-probe-timer (fn timer)
  (let ((start (float-time)))
    (prog1 (funcall fn timer)
      (let ((ms (* 1000 (- (float-time) start))))
        (when (> ms 5)
          (push `((callback . ,(format "%S" (timer--function timer)))
                  (stage . ,startup-probe-stage) (ms . ,ms))
                startup-probe-timers))))))

(when (getenv "DOOM_STARTUP_PROBE_TRACE")
  (advice-add 'require :around #'startup-probe-require)
  (advice-add 'timer-event-handler :around #'startup-probe-timer))

(defun startup-probe-pre ()
  (when (and startup-probe-armed
             (memq this-command '(evil-next-line evil-previous-line)))
    (setq startup-probe-start (float-time)
          startup-probe-gc gcs-done
          startup-probe-gc-time gc-elapsed)))

(defun startup-probe-finish ()
  (condition-case err
      (progn
        (setq startup-probe-stage "verification")
        (when (getenv "DOOM_STARTUP_PROBE_TRACE")
          (maphash (lambda (stack count)
                     (push (cons count (format "%S" stack)) startup-probe-profile))
                   (profiler-cpu-log))
          (profiler-stop))
        (when (equal (getenv "DOOM_STARTUP_PROBE_SCENARIO") "python")
          (unless startup-probe-diagnostics-verified
            (error "Python diagnostic verification did not complete"))
          (with-current-buffer startup-probe-buffer
            (flycheck-buffer)
            (let ((groups (delete-dups (mapcar #'flycheck-error-group
                                               flycheck-current-errors))))
              (unless (and lsp-diagnostics-mode
                           (member "ty" groups) (member "Ruff" groups))
                (error "Missing rendered Flycheck diagnostics: %S" groups))
              (push (cons 'flycheck_sources (vconcat groups))
                    startup-probe-feature-checks)))
          (when lsp--client-packages-required
            (error "Python prematurely loaded the global LSP bundle"))
          (with-temp-buffer
            (setq major-mode 'sh-mode)
            (lsp--require-packages)
            (unless (and lsp--client-packages-required
                         (featurep 'lsp-bash) (featurep 'lsp-clangd)
                         (lsp--client-add-on? (gethash 'ty-ls lsp-clients)))
              (error "Non-Python bundle or ty registration regressed"))))
        (when (equal (getenv "DOOM_STARTUP_PROBE_SCENARIO") "org")
          (with-current-buffer startup-probe-buffer
            (unless (and flycheck-mode (eq (flycheck-get-checker-for-buffer) 'org-lint))
              (error "Org lint integration is unavailable"))
            (flycheck-buffer)
            (unless (eq flycheck-last-status-change 'finished)
              (error "Org lint did not finish: %S" flycheck-last-status-change))
            (push '(lint_checker . "org-lint") startup-probe-feature-checks))
          ;; Validate the deferred configuration without starting a timer,
          ;; web server, browser, or writing to the real notes database.
          (require 'org-pomodoro)
          (require 'org-roam-ui)
          (unless (and (= org-pomodoro-length 30)
                       (= org-pomodoro-long-break-length 20)
                       org-roam-ui-sync-theme org-roam-ui-follow
                       org-roam-ui-update-on-save org-roam-ui-open-on-start
                       (not (autoloadp (symbol-function 'org-roam-ui-mode))))
            (error "Deferred Org tool configuration did not run")))
        (startup-probe-write
         "result.json"
         `((samples . ,(vconcat (nreverse startup-probe-samples)))
           (feature_open_ms . ,startup-probe-feature-ms)
           (feature_checks . ,startup-probe-feature-checks)
           (input_events . ,(vconcat (nreverse startup-probe-input-events)))
           (loads . ,(vconcat (nreverse startup-probe-loads)))
           (timers . ,(vconcat (nreverse startup-probe-timers)))
           (profile . ,(vconcat (mapcar (lambda (entry)
                                         `((count . ,(car entry)) (stack . ,(cdr entry))))
                                       (seq-take (sort startup-probe-profile
                                                       (lambda (a b) (> (car a) (car b)))) 20))))
           (native . ,startup-probe-native)))
        (kill-emacs 0))
    (error (startup-probe-error err))))

(defun startup-probe-maybe-finish ()
  (when (and startup-probe-input-finished startup-probe-feature-finished)
    (run-with-timer 0.05 nil #'startup-probe-finish)))

(defun startup-probe-post ()
  (when startup-probe-start
    (let ((sample `((command . ,(format "%s" this-command))
            (ms . ,(* 1000 (- (float-time) startup-probe-start)))
            (gcs . ,(- gcs-done startup-probe-gc))
            (gc_ms . ,(* 1000 (- gc-elapsed startup-probe-gc-time))))))
      (push sample startup-probe-input-events)
      ;; Evil remaps `this-command' to next/previous-line while executing.
      (when (memq this-command '(evil-next-line evil-previous-line
                                next-line previous-line))
        (let* ((file (expand-file-name
                      (format "input-%02d.json" (length startup-probe-samples))
                      startup-probe-root))
               (sent (with-temp-buffer
                       (insert-file-contents file)
                       (json-read))))
          (push (cons 'dispatch_ms (* 1000 (- (float-time) sent))) sample))
        (push sample startup-probe-samples)))
    (setq startup-probe-start nil)
    (when (>= (length startup-probe-samples) 20)
      (setq startup-probe-armed nil)
      (setq startup-probe-input-finished t)
      ;; Do not exit in the middle of a command's post-command dispatch.
      (startup-probe-maybe-finish))))

;; Count from before Doom's lazy dispatch, not just around evil-next-line.

(defun startup-probe-first-input (fn &rest hooks)
  (let ((start (float-time)))
    (prog1 (apply fn hooks)
      (when (and startup-probe-armed (memq 'doom-first-input-hook hooks))
        (push `((command . "doom-first-input-hook")
                (ms . ,(* 1000 (- (float-time) start))))
              startup-probe-input-events)))))
(advice-add 'doom-run-hooks :around #'startup-probe-first-input)

(defun startup-probe-error (err)
  (startup-probe-write "result.json" `((error . ,(error-message-string err))))
  (kill-emacs 1))

(defun startup-probe-ready ()
  (condition-case err
      (progn
        (goto-char (point-min))
        (evil-local-mode 1)
        (evil-normal-state)
        ;; Major-mode/Evil setup can replace local command hooks. Install the
        ;; probes afterwards, locally, so every measured motion reaches them.
        (add-hook 'pre-command-hook #'startup-probe-pre -1000 t)
        (add-hook 'post-command-hook #'startup-probe-post 1000 t)
        (unless (and (eq (key-binding "j") 'evil-next-line)
                     (eq (key-binding "k") 'evil-previous-line))
          (error "j/k are not Evil motions in probe buffer"))
        (let ((native 0) bytecode)
          (mapatoms
           (lambda (symbol)
             (when (and (fboundp symbol)
                        (string-match-p "\\`\\(?:evil-\\|pyim-\\|company-\\)"
                                        (symbol-name symbol)))
               (let ((fn (indirect-function symbol)))
                 (cond ((native-comp-function-p fn) (cl-incf native))
                       ((and (byte-code-function-p fn)
                             (not (advice--p fn)))
                        (push (symbol-name symbol) bytecode)))))))
          (setq startup-probe-native
                `((native_functions . ,native)
                  (bytecode_functions . ,(vconcat (sort bytecode #'string<))))))
        (setq startup-probe-armed t)
        (when (getenv "DOOM_STARTUP_PROBE_TRACE")
          (require 'profiler)
          (profiler-start 'cpu))
        (setq startup-probe-stage "input")
        (startup-probe-write "ready.json" '((ready . t))))
    (error (startup-probe-error err))))

(defun startup-probe-python-initialized ()
  (let ((id (lsp--workspace-server-id lsp--cur-workspace)))
    (push (cons id (float-time)) startup-probe-servers-ready)
    (when (and (not startup-probe-python-check-scheduled)
               (cl-every (lambda (server) (assq server startup-probe-servers-ready))
                         '(jedi ruff ty-ls)))
      (setq startup-probe-python-check-scheduled t)
      (run-with-timer 0 nil #'startup-probe-python-ready))))

(defun startup-probe-python-diagnostics (tick)
  (condition-case err
      (with-current-buffer startup-probe-buffer
        (let (sources)
          (dolist (ws lsp--buffer-workspaces)
            (when (gethash (lsp--uri-to-path (lsp--buffer-uri))
                           (lsp--workspace-diagnostics ws))
              (push (lsp--workspace-server-id ws) sources)))
          (cond
           ((and (memq 'ty-ls sources) (memq 'ruff sources))
            (unless (and flycheck-mode (eq flycheck-checker 'lsp))
              (error "LSP diagnostics did not retain Flycheck integration"))
            (setq startup-probe-diagnostics-verified t
                  startup-probe-feature-finished t)
            (push (cons 'diagnostic_servers (vconcat sources)) startup-probe-feature-checks)
            (if (getenv "DOOM_STARTUP_PROBE_DURING_INIT")
                (startup-probe-maybe-finish)
              (startup-probe-ready)))
           ((>= tick 100) (error "Missing ty/Ruff diagnostics: %S" sources))
           (t (run-with-timer 0.1 nil #'startup-probe-python-diagnostics (1+ tick))))))
    (error (startup-probe-error err))))

(defun startup-probe-python-ready ()
  (condition-case err
      (with-current-buffer startup-probe-buffer
        (let ((ids (mapcar #'lsp--workspace-server-id
                          (seq-filter
                           (lambda (ws) (eq (lsp--workspace-status ws) 'initialized))
                           lsp--buffer-workspaces))))
          (cond
           ((and (memq 'jedi ids) (memq 'ruff ids) (memq 'ty-ls ids))
            (when (with-current-buffer "*Messages*"
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward "Error processing message" nil t)))
              (error "LSP initialization logged a protocol callback error"))
            (let* ((start (float-time))
                   (server-wait-ms (* 1000 (- start startup-probe-python-wait-start)))
                   (params (save-excursion
                             (goto-char (point-min)) (forward-line 2) (end-of-line)
                             (lsp--text-document-position-params)))
                   (providers (mapcar #'lsp--workspace-server-id
                                      (lsp--find-workspaces-for "textDocument/completion"))))
              (lsp-request-async
               "textDocument/completion" params
               (lambda (response)
                 (condition-case err
                     (let* ((items (if (vectorp response) response (lsp-get response :items)))
                            (hits (seq-filter
                                   (lambda (it) (equal (lsp-get it :label) "getatime")) items)))
                       (unless (and (= (length hits) 1) (equal providers '(jedi))
                                    (string-match-p "\\${1:" (lsp-get (elt hits 0) :insertText)))
                         (error "Python completion lost its unique Jedi argument snippet"))
                       (setq startup-probe-feature-checks
                             `((servers . ,(vconcat ids))
                               (server_wait_ms . ,server-wait-ms)
                               (completion_ms . ,(* 1000 (- (float-time) start)))
                               (completion_providers . ,(vconcat providers))))
                       (startup-probe-python-diagnostics 0))
                   (error (startup-probe-error err))))
               :mode 'detached
               :error-handler (lambda (err) (startup-probe-error (list 'error err))))))
           (t (error "Python servers did not initialize: %S" ids)))))
    (error (startup-probe-error err))))

(defun startup-probe-prepare ()
  (condition-case err
      (let ((scenario (getenv "DOOM_STARTUP_PROBE_SCENARIO")))
        (setq startup-probe-stage scenario)
        (setq startup-probe-feature-finished (not (equal scenario "python")))
        (when (featurep 'lsp-mode)
          (error "Startup eagerly loaded LSP before opening a code buffer"))
        (if (equal scenario "startup")
            (progn
              (switch-to-buffer (get-buffer-create " *startup-latency-probe*"))
              (fundamental-mode)
              (dotimes (_ 200) (insert "disposable startup latency test\n"))
              (startup-probe-ready))
          (let* ((default-directory (file-name-as-directory startup-probe-root))
                 (file (expand-file-name
                        (if (equal scenario "org") "probe.org" "probe.py")
                        startup-probe-root)))
            (unless (zerop (call-process "git" nil nil nil "init" "-q"))
              (error "Cannot initialize disposable project"))
            (with-temp-file file
              (insert (if (equal scenario "org")
                          "* Heading\nParagraph.\n** Subheading\nText.\n"
                        "import os\nx: int = \"hello\"\nresult = os.path.get\nimport sys\n")))
            (setq lsp-auto-guess-root t
                  lsp-enable-file-watchers nil
                  lsp-enable-suggest-server-download nil
                  lsp-session-file (expand-file-name "lsp-session" startup-probe-root))
            (when (equal scenario "python")
              (add-hook 'lsp-after-initialize-hook #'startup-probe-python-initialized))
            (let ((start (float-time)))
              (if (equal scenario "treemacs") (treemacs) (find-file file))
              (setq startup-probe-feature-ms (* 1000 (- (float-time) start))))
            (setq startup-probe-buffer (current-buffer))
            (pcase scenario
              ("python"
               (setq startup-probe-python-wait-start (float-time))
               (when (getenv "DOOM_STARTUP_PROBE_DURING_INIT")
                 (startup-probe-ready)))
              ("org"
               (unless (and (derived-mode-p 'org-mode)
                            (featurep 'org-lint) (featurep 'ox-beamer)
                            (commandp 'org-pomodoro)
                            (commandp 'org-roam-ui-mode))
                 (error "Org commands are not available"))
               (setq startup-probe-feature-checks
                     `((pomodoro_loaded . ,(featurep 'org-pomodoro))
                       (roam_ui_loaded . ,(featurep 'org-roam-ui))))
               (when (or (featurep 'org-pomodoro) (featurep 'org-roam-ui))
                 (error "Ordinary Org editing eagerly loaded optional tools"))
               (startup-probe-ready))
              ("treemacs"
               ;; Treemacs has its own j/k commands. Measure Evil motions in
               ;; the original window after verifying the tree really opened.
               (unless (derived-mode-p 'treemacs-mode)
                 (error "Treemacs did not open"))
               (other-window 1)
               (switch-to-buffer (get-buffer-create " *startup-latency-probe*"))
               (dotimes (_ 200) (insert "disposable startup latency test\n"))
               (startup-probe-ready))))))
    (error (startup-probe-error err))))

(add-hook 'window-setup-hook
          (lambda () (run-with-timer 0 nil #'startup-probe-prepare)) 100)
