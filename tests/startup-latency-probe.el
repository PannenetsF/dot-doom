;;; startup-latency-probe.el --- Real command-loop probe -*- lexical-binding: t; -*-
;; Loaded by startup-latency.py in a disposable interactive terminal Emacs.
(require 'json)
(defvar startup-probe-root (getenv "DOOM_STARTUP_PROBE_ROOT"))
(defvar startup-probe-armed nil)
(defvar startup-probe-start nil)
(defvar startup-probe-samples nil)
(defvar startup-probe-input-events nil)
(defvar startup-probe-loads nil)
(defvar startup-probe-gc 0)
(defvar startup-probe-gc-time 0)
(defvar startup-probe-native nil)

(defun startup-probe-write (name object)
  (with-temp-file (expand-file-name name startup-probe-root)
    (insert (json-encode object))))

(defun startup-probe-require (fn feature &rest args)
  (let ((start (float-time)))
    (prog1 (apply fn feature args)
      (when startup-probe-armed
        (let ((ms (* 1000 (- (float-time) start))))
          (when (> ms 5)
            (push `((feature . ,(symbol-name feature)) (ms . ,ms))
                  startup-probe-loads)))))))

(when (getenv "DOOM_STARTUP_PROBE_TRACE")
  (advice-add 'require :around #'startup-probe-require))

(defun startup-probe-pre ()
  (when (and startup-probe-armed
             (memq this-command '(evil-next-line evil-previous-line)))
    (setq startup-probe-start (float-time)
          startup-probe-gc gcs-done
          startup-probe-gc-time gc-elapsed)))

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
        (push sample startup-probe-samples)))
    (setq startup-probe-start nil)
    (when (>= (length startup-probe-samples) 20)
      (setq startup-probe-armed nil)
      (startup-probe-write
       "result.json"
       `((samples . ,(vconcat (nreverse startup-probe-samples)))
         (input_events . ,(vconcat (nreverse startup-probe-input-events)))
         (loads . ,(vconcat (nreverse startup-probe-loads)))
         (native . ,startup-probe-native)))
      ;; Do not exit in the middle of a command's post-command dispatch.
      (run-with-timer 0.05 nil (lambda () (kill-emacs 0))))))

;; Count from before Doom's lazy dispatch, not just around evil-next-line.

(defun startup-probe-first-input (fn &rest hooks)
  (let ((start (float-time)))
    (prog1 (apply fn hooks)
      (when (and startup-probe-armed (memq 'doom-first-input-hook hooks))
        (push `((command . "doom-first-input-hook")
                (ms . ,(* 1000 (- (float-time) start))))
              startup-probe-input-events)))))
(advice-add 'doom-run-hooks :around #'startup-probe-first-input)

(defun startup-probe-ready ()
  (condition-case err
      (progn
        (switch-to-buffer (get-buffer-create " *startup-latency-probe*"))
        (fundamental-mode)
        (dotimes (_ 200) (insert "disposable startup latency test\n"))
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
        (startup-probe-write "ready.json" '((ready . t))))
    (error
     (startup-probe-write "result.json" `((error . ,(error-message-string err))))
     (kill-emacs 1))))

(add-hook 'emacs-startup-hook
          (lambda () (run-with-timer 0 nil #'startup-probe-ready)) 100)
