;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18 ))

(defun private-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "LXGW WenKai Mono"))))

(add-hook 'after-setting-font-hook #'private-cjk-font)

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)



;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Here we set up some additional configurations for Doom Emacs.
;; 1. the which-key delay is set to 0.1 seconds, which means that the key hints
(setq which-key-idle-delay 0.1)

;; 2. disable the framebar
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; 3. disable the splash 
;; https://www.cleanpng.com/png-spacemacs-computer-software-command-line-interface-3947037/
(setq fancy-splash-image (concat doom-user-dir "Nuvola_apps_emacs_vector.svg"))

;; 4. setup org-mode
(use-package! org-modern
  :after org
  :init 
  (with-eval-after-load 'org (global-org-modern-mode))
  :config
  (setq org-modern-fold-stars '(("▶" . "▼") ("▷" . "▽") ("⏵" . "⏷") ("▹" . "▿") ("▸" . "▾"))))

(use-package! zotxt
  ;; if you omit :defer, :hook, :commands, or :after, then the package is loaded
  ;; immediately. By using :hook here, the `hl-todo` package won't be loaded
  ;; until prog-mode-hook is triggered (by activating a major mode derived from
  ;; it, e.g. python-mode)
  :after org
  :hook ('org-mode-hook . 'org-zotxt-mode)
  :init
  ;; code here will run immediately
  (require 'request)
  (require 'deferred)
  ;; (require 'org-zotxt-noter)
  ;; (require 'org-noter)
  (require 'org-zotxt)
  (setq org-zotxt-notes-directory (expand-file-name "zotero/" org-roam-directory)
        org-zotxt-link-description-style :citekey
        zotxt-default-bibliography-style "ieee"
        zotxt-default-library :user)


  (defvar org-roam-zotero-note--file-name ""
    "The name of the zotero note file to be created or opened.")

  (defun jump-to-zotxt-note-by-search ()
    "Go to Zotero note via searching. Create the note file if it does not exist"
    (interactive)
    (deferred:$
     ;; step1: search for Zotero items
     (zotxt-search-deferred :title-creator-year)
     (deferred:nextc it
                     (lambda (items)
                       (when (null items)
                         (error "No Zotero items found."))
                       (car items)))

     ;; step2: get the full item details
     (deferred:nextc it
                     (lambda (item)
                       (zotxt-get-item-deferred item :full)))

     ;; step3: generate note file path and pass context
     (deferred:nextc it
                     (lambda (full-item)
                       (let* ((item-key (plist-get full-item :key))
                              (note-directory org-zotxt-notes-directory)
                              (note-file (concat note-directory item-key ".org")))
                         (make-directory note-directory t)
                         (cons full-item note-file))))

     ;; step4: create or open the note file
     (deferred:nextc it
                     (lambda (full-item-note-file)
                       (let* (
                              (full-item (car full-item-note-file))
                              (note-file (cdr full-item-note-file))
                              (json-object-type 'hash-table)
                              (json-array-type 'list)
                              (json-key-type 'string)
                              (json-data (json-read-from-string (plist-get full-item :full)))
                              (item-data (if (listp json-data) (car json-data) json-data))
                              (item-title (gethash "title" item-data))
                              )
                         (make-directory org-zotxt-notes-directory t)
                         (if (file-exists-p note-file)
                             (find-file note-file)
                           (with-temp-file note-file
                             (insert (format "#+TITLE: %s\n" (concat "PAPER: " item-title)))
                             (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
                             (insert "* Meta Info\n")
                             ))
                         (find-file note-file)
                         (setq org-roam-zotero-note--file-name note-file)
                         full-item
                         )
                       ))

     ;; step5: get the properties and paths
     (deferred:nextc it
                     (lambda (item)
                       (zotxt-get-item-deferred item :paths)))
     (deferred:nextc it
                     (lambda (item)
                       ;; (message (prin1-to-string item))
                       (org-zotxt-get-item-link-text-deferred item)))
     (deferred:nextc it
                     (lambda (resp)
                       (find-file org-roam-zotero-note--file-name)
                       (goto-char (point-min))
                       (if-let ((id (org-entry-get (point) "ID")))
                           (message "id found in the DB, jump to it directly")
                         (progn
                           (org-entry-put (point) "ID" (org-id-new))
                           (org-entry-put (point) org-zotxt-noter-zotero-link (org-zotxt-make-item-link resp))
                           (org-roam-db-sync)
                           ;; (let ((path (org-zotxt-choose-path (cdr (assq 'paths (plist-get resp :paths))))))
                           ;;   (org-entry-put (point) org-noter-property-doc-file path)
                           ;;   (save-buffer)
                           ;;   (message "Zotxt Linking note to Zotero item %s" (plist-get resp :key))
                           ;;   )
                           )
                         )
                       ))


     ;; step6: error handling
     (deferred:error it
                     (lambda (err)
                       (message "Zotxt Failed for : %s" (error-message-string err))
                       ;; (signal 'user-error (list "Canceled"))
                       ))
     ))


  (defun notify-via-macos (title msg)
    (call-process "terminal-notifier"
                  nil 0 nil
                  "-group" "Emacs"
                  "-title" title
                  "-sender" "org.gnu.Emacs"
                  "-message" msg
                  "-active" "org.gnu.Emacs"))

  (advice-add 'org-pomodoro-notify :after (lambda (title message)
                                            (notify-via-macos title message)))


  (defun org-zotero-open-via-macos (path _)
    (call-process "open" nil nil nil (concat "zotero:" path)))


  (defun extract-zotero-link-from-path (zotero-link-with-desc)
    ;; "[[zotero://select/items/1_BYJ73MHG][@llama3-grattafioriLlama3Herd2024]]"
    ;; "[[zotero://select/items/1_BYJ73MHG]]"
    (if (string-match "\\(zotero://[^]]+\\)" zotero-link-with-desc)
        (match-string 1 zotero-link-with-desc)
      nil)
    )


  (defun extract-zotero-desc-from-path (zotero-link-with-desc)
    "Extract description from Zotero link.
Example:
\"[[zotero://select/items/1_BYJ73MHG][@llama3-grattafioriLlama3Herd2024]]\"
  -> \"@llama3-grattafioriLlama3Herd2024\"
If no description exists (e.g. \"[[zotero://select/items/1_BYJ73MHG]]\"),
return nil."
    (when (string-match "\\[\\[zotero://.*\\]\\[\\(.*\\)\\]\\]" zotero-link-with-desc)
      (match-string 1 zotero-link-with-desc)))


  (defun org-id-of-zotero-note-export-maybe (path desc format)
    "Export function for org-id links that may contain Zotero links."
    (let ((file-name (car (org-roam-id-find path))))
      (if (and file-name (file-exists-p file-name))
          (with-current-buffer (find-file-noselect file-name)
            (save-excursion
              (goto-char (point-min))
              (let* (
                     (zotero-link-prop (org-entry-get (point) org-zotxt-noter-zotero-link))
                     (zotero-link (extract-zotero-link-from-path zotero-link-prop))
                     (zotero-desc (extract-zotero-desc-from-path zotero-link-prop))
                     )
                (if zotero-link
                    (progn
                      (org-zotxt--link-export (substring zotero-link 8) (or zotero-desc desc) format)
                      )
                  nil
                  ))

              ))
        nil
        )))

  (defun org-zotxt-get-cite-key-from-zotero-id (zurl)
    (let* ((zurl-id (substring zurl 22))
           (response (request
                       (format "%s/items" zotxt-url-base)
                       :params `(("key" . ,zurl-id)
                                 ("format" . "citekey"))
                       :sync t
                       :parser 'json-read))
           (data (request-response-data response))
           (result (if (and (vectorp data) (> (length data) 0))
                       (aref data 0)
                     (error "Unexpected response: empty or not a vector"))))
      (message "Citation key: %s" result)
      result))

  (defun org-zotero-update-citekey-inline (&optional filename)
    "Update the Zotero citekey of the current org file."
    (interactive)
    (let ((file (or filename (buffer-file-name))))
      (unless file
        (error "No file is associated with this buffer"))
      (message "Updating Zotero citekey for file: %s" file)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (let* ((zurl-with-desc (org-entry-get nil org-zotxt-noter-zotero-link))
                 (zurl (when zurl-with-desc
                         (extract-zotero-link-from-path zurl-with-desc)))
                 (cite-key (when zurl
                             (org-zotxt-get-cite-key-from-zotero-id zurl)))
                 (new-link (when cite-key
                             (org-link-make-string zurl cite-key))))
            (if cite-key
                (progn
                  (message "Zotero link: %s" zurl)
                  (message "Cite key: %s" cite-key)
                  (org-entry-put nil org-zotxt-noter-zotero-link new-link)
                  (save-buffer)
                  (message "Citekey updated to: %s" cite-key))
              (message "No cite-key found for Zotero link: %s" zurl)))))))


  (add-hook 'org-zotxt-mode-hook
            (lambda ()
              (progn
                (org-link-set-parameters "zotero" :follow #'org-zotero-open-via-macos :export #'org-zotxt--link-export)
                (org-link-set-parameters "id" :follow #'org-id-open :store #'org-id-store-link-maybe :export #'org-id-of-zotero-note-export-maybe)
                )
              ))

  )

;; setup org-roam-ui
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;         :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)

  (setq org-roam-dailies-directory "weekbook/")

  (setq org-roam-dailies-capture-templates
        '(
          ("d" "Weekbook" entry "%?" :if-new
           (file+head+olp "%<%Y-week%g>.org" "#+title: %<%Y-week%g>\n" ("Weekbook" "%<%A/week%g %Y-%m-%d>"))
           :unnarrowed t
           )
          ("i" "Ideas" entry "** %?" :if-new
           (file+head+olp "%<%Y-week%g>.org" "#+title: %<%Y-week%g>\n" ("Ideas"))
           :unnarrowed t
           )
          )
        org-roam-capture-templates
        '(
          ("d" "Normal Notes" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n") :unnarrowed t)

          ("t" "Quick To-do" plain "no %?" :if-new
           (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n" ("TODOs" "%<%Y%m%d%H%M%S>"))
           :unnarrowed t)
          )
        )
  )

(setq org-directory "~/Documents/org/")

(after! org
  (require 'ox-beamer)
  (require 'ox-latex)
  (setq org-checkbox-hierarchical-statistics t
        org-agenda-todo-list-sublevels t
        org-todo-keywords '((sequence "TODO(t)" "PEND(p)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)"))
        org-capture-templates
        '(
          ("t" "Quick To-do" entry
           (file+headline +org-capture-notes-file "TO-DO Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("w" "Weekly workbook" entry
           (file+olp+datetree +org-capture-notes-file "Workbook")
           "* %?\n" :tree-type week :prepend t)
          ("p" "Project note" entry
           (file+headline +org-capture-notes-file "Projects")
           "* TODO %^{ProjectName}\n%u\n%a\n")
          ("i" "Quick Thought" entry
           (file+olp +org-capture-notes-file "Ideas")
           "* %?\n"))
        org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-log-done t
        org-log-into-drawer t
        )

  (map! :leader
        (:prefix-map ("X" . "Org")
         :desc "org refresh statistics of current line" "R" #'org-update-statistics-cookies
         :desc "org cycle todo state" "t" #'org-todo
         :desc "org toggle checkbox" "x" #'org-toggle-checkbox
         :desc "org roam refile" "r" #'org-roam-refile
         :desc "org roam insert" "i" #'org-roam-node-insert
         :desc "org roam capture" "c" #'org-roam-capture
         :desc "org roam weekbook" "w" #'org-roam-dailies-capture-today
         :desc "org roam capture paper" "p" #'jump-to-zotxt-note-by-search
         :desc "org roam update zotero citekey of current file" "z" #'org-zotero-update-citekey-inline
         :desc "org roam find" "f" #'org-roam-node-find
         :desc "org roam ui toggle" "u" #'org-roam-ui-mode
         :desc "org roam ui follow toggle" "F" #'org-roam-ui-follow-mode
         :desc "org capture" "C" #'org-capture
         )
        )
  )

(after! org-pomodoro
  (setq org-pomodoro-length 30
        org-pomodoro-short-break-length 5
        org-pomodoro-long-break-length 20
        org-pomodoro-short-break-sound nil
        org-pomodoro-long-break-sound nil
        )
  )

;; 5. disable quit confirmation
(setq confirm-kill-emacs nil)

;; 6. accelerate tramp ~disable for now~
;; (use-package tramp
;;   :config
;;   (setq tramp-auto-save-directory "/tmp/tramp-autosaves/"
;;         tramp-terminal-type "tramp"
;;         tramp-backup-directory-alist backup-directory-alist
;;         remote-file-name-inhibit-cache 60 ; 加速，允许 cache
;;         remote-file-name-inhibit-locks t ; 加速，不会使用文件锁
;;         tramp-verbose 0 ; 加速，更少的 tramp 信息
;;         vc-handled-backends '(SVN Git) ; 加速，禁用一些版本控制后端
;;         )
;;   (setq tramp-use-ssh-controlmaster-options nil)
;;   (setq tramp-chunksize 2000)
;;   (connection-local-set-profile-variables
;;    'remote-direct-async-process
;;    '((tramp-direct-async-process . t)))
;;   (connection-local-set-profiles
;;    '(:application tramp :protocol "ssh")
;;    'remote-direct-async-process)
;;   )

;; vim-like keybindings
(map! :leader
      :desc "Open File Tree" "e" #'+treemacs/toggle)

;; for copilot
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-E" . 'copilot-accept-completion)
              ("C-e" . 'copilot-accept-completion)))

;; for auto-save
(setq auto-save-visited-interval 5)
(auto-save-visited-mode +1)

;; for dired
(map! :map dirvish-mode-map
      :n "+" #'dired-create-empty-file)

;; for eglot lsp config
(after! eglot
  (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy"))
  (set-eglot-client! 'python-mode '("pyright-langserver" "--stdio"))
  (set-eglot-client! 'python-ts-mode '("pyright-langserver" "--stdio"))
  )

;; load doom env
(doom-load-envvars-file (expand-file-name "doomenv" doom-user-dir))
;; setup company
(after! company
  ;; for org
  (set-company-backend! 'org-mode 'company-files 'company-capf)
  ;; for prog
  (set-company-backend! 'prog-mode 'company-capf 'company-files 'company-yasnippet 'company-dabbrev-code 'company-dabbrev)
  )
