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
(setq doom-font (font-spec :family "CaskaydiaCove Nerd Font Mono" :size 18 ))
(setq use-default-font-for-symbols nil)

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

(defun my-banner-for-nw-mode ()
  (let* ((banner '(
	           "████████ ██   ██ ██ ███    ██ ██   ██     ████████ ██     ██ ██  ██████ ███████ "
	           "   ██    ██   ██ ██ ████   ██ ██  ██         ██    ██     ██ ██ ██      ██      "
	           "   ██    ███████ ██ ██ ██  ██ █████          ██    ██  █  ██ ██ ██      █████   "
	           "   ██    ██   ██ ██ ██  ██ ██ ██  ██         ██    ██ ███ ██ ██ ██      ██      "
	           "   ██    ██   ██ ██ ██   ████ ██   ██        ██     ███ ███  ██  ██████ ███████ "
	           "                                                                                "
	           "                                                                                "
	           "      ██████  ██████  ██████  ███████      ██████  ███    ██  ██████ ███████    "
	           "     ██      ██    ██ ██   ██ ██          ██    ██ ████   ██ ██      ██         "
	           "     ██      ██    ██ ██   ██ █████       ██    ██ ██ ██  ██ ██      █████      "
	           "     ██      ██    ██ ██   ██ ██          ██    ██ ██  ██ ██ ██      ██         "
	           "      ██████  ██████  ██████  ███████      ██████  ██   ████  ██████ ███████    "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'my-banner-for-nw-mode)

;; 4. setup org-mode
;; (use-package! org-latex-impatient
;;   :after org
;;   :defer
;;   :hook (org-mode . org-latex-impatient-mode)
;;   :init 
;;   (setq org-latex-impatient-tex2svg-bin
;;         ;; location of tex2svg executable
;;         "~/node_modules/mathjax-node-cli/bin/tex2svg"))
(use-package! org
  :config
  (setq org-directory "~/Documents/org/")
  )

(use-package! org-elp)

(use-package! org-modern
  :after org
  :init 
  (with-eval-after-load 'org (global-org-modern-mode))
  :config
  (setq org-modern-fold-stars '(("▶" . "▼") ("▷" . "▽") ("⏵" . "⏷") ("▹" . "▿") ("▸" . "▾"))))

(use-package! org-roam
  :after org
  :config
  (setq org-roam-directory (expand-file-name "roam" org-directory))
  (setq org-roam-dailies-directory "weekbook/")
  (setq org-roam-dailies-capture-templates
        '(
          ("d" "Weekbook" entry "** %?"
           :target (file+head+olp "%<%Y-week%U>.org" "#+title: %<%Y-week%U>\n" ("Weekbook" "%<%A/week%U %Y-%m-%d>"))
           :unnarrowed t
           )
          )
        org-roam-capture-templates
        '(
          ("d" "Normal Notes" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          )
        )
  )


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
  )



(use-package! zotxt
  ;; if you omit :defer, :hook, :commands, or :after, then the package is loaded
  ;; immediately. By using :hook here, the `hl-todo` package won't be loaded
  ;; until prog-mode-hook is triggered (by activating a major mode derived from
  ;; it, e.g. python-mode)
  :after org-roam
  :hook ('org-mode-hook . 'org-zotxt-mode)
  :config
  ;; code here will run immediately
  (require 'request)
  (require 'deferred)
  ;; (require 'org-zotxt-noter)
  ;; (require 'org-noter)
  (message "now the org-dir is %s" org-directory)
  (message "now the org-roam-dir is %s" org-roam-directory)
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
                           (org-roam-alias-add
                            (cdr (assq 'title-short
                                       (aref (json-read-from-string (plist-get resp :full)) 0))))
                           (org-roam-db-sync)
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

    (when (eq format 'latex)
      (let ((file-name (car (org-roam-id-find path))))
        (if (and file-name (file-exists-p file-name))
            (with-current-buffer (find-file-noselect file-name)
              (save-excursion
                (goto-char (point-min))
                (let* (
                       (zotero-link-prop (org-entry-get (point) org-zotxt-noter-zotero-link))
                       )
                  (if zotero-link-prop
                      (progn
                        (org-zotxt--link-export (substring (extract-zotero-link-from-path zotero-link-prop) 8) (or (extract-zotero-desc-from-path zotero-link-prop) desc) format)
                        )
                    nil
                    ))

                ))
          nil
          )))
    )

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
                             (org-link-make-string zurl (concat "@" cite-key)))))
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
  (setq org-mode-ligatures
        '((org-mode "|||>" "<|||" "<==>" "<!--" "####" "~~>" "||=" "||>"
           ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
           "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
           "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
           "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
           "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
           "~>" "~-" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
           "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
           ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
           "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
           "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
           "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
           "\\\\" "://")))
  (push (cons 'org-mode (cdr (assq 'org-mode org-mode-ligatures)))
        +ligatures-alist)

  (dolist (lig +ligatures-alist)
    (ligature-set-ligatures (car lig) (cdr lig)))
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
  ;; :hook (prog-mode . copilot-mode) ;; disable auto-copilot when prog-mode
  :after-call prog-mode
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
(after! org
  ;; for org
  ;; (set-company-backend! 'org-mode 'company-math-symbols-latex 'company-latex-commands '(:separate company-ispell company-dabbrev company-dabbrev-code) 'company-files 'company-capf 'company-yasnippet)
  (set-company-backend! 'org-mode '(company-files company-capf) 'company-math-symbols-latex 'company-latex-commands)
  )
(after! company
  ;; for prog
  (set-company-backend! 'prog-mode 'company-capf 'company-files 'company-yasnippet 'company-dabbrev-code 'company-dabbrev)
  )

;; fix ein 
(after! ein
  (defun pm--visible-buffer-name ()
    (pm--buffer-name))
  )

;; set pdf view color to be solarized
(setq pdf-view-midnight-colors '("#655370" . "#fbf8ef"))

;; 
(setq! imagemagick-types-inhibit '(C HTML HTM INFO M TXT PDF SVG))

;; autoflow from https://emacs-china.org/t/autoflow-el/25381

(defvar autoflow-list nil)
(defvar autoflow-curr-nth 0)
(defvar autoflow-curr-flow nil)

(defmacro define-autoflow (name &rest funcs)
  `(progn
     (if-let ((match (assoc ,name autoflow-list)))
         (unless (equal (cdr match) ',funcs)
           (setcdr match ',funcs))
       (push (append (list ,name) ',funcs) autoflow-list))
     autoflow-list))

(defun autoflow-set-header-info ()
  (let* ((name autoflow-curr-flow)
         (funcs (autoflow-flows name)))
    (setq-local header-line-format
                (format "Autoflow %s/%s [%s] "
                        (1+ autoflow-curr-nth) (length funcs) name))))

(defun autoflow-flows (name)
  (cdr (assoc name autoflow-list)))

(defun autoflow--curr-func (nth funcs)
  "Return the current applying function as a list."
  (if-let* ((func (nth nth funcs))
            (_ (functionp func)))
      (list func)
    func))

(defun autoflow--next ()
  (cl-incf autoflow-curr-nth)
  (let* ((flow-name autoflow-curr-flow)
         (flow-funcs (autoflow-flows flow-name)))
    (if (< autoflow-curr-nth (length flow-funcs))
        (progn
          (setq-local header-line-format nil)
          (apply (autoflow--curr-func autoflow-curr-nth flow-funcs))
          (autoflow-set-header-info))
      (message "autoflow %s over!" autoflow-curr-flow)
      (setq autoflow-curr-flow nil)
      (setq autoflow-curr-nth 0)
      (setq-local header-line-format nil))))

;;;###autoload
(defun autoflow-start (&optional name)
  (interactive)
  (if autoflow-curr-flow
      (autoflow--next)
    (let* ((flow-name (completing-read "Choose a autoflow: "
                                  autoflow-list nil t))
           (flow-funcs (autoflow-flows flow-name)))
      (setq autoflow-curr-flow flow-name)
      (setq autoflow-curr-nth 0)
      (apply (autoflow--curr-func autoflow-curr-nth flow-funcs))
      (autoflow-set-header-info))))

(define-autoflow "come"
                 (lambda ()
                   (shell-command (concat "cd " org-directory " && make come")))
                 (org-todo-list)
                 (org-roam-dailies-capture-today)
                 )

(define-autoflow "go"
                 (org-todo-list)
                 (org-roam-dailies-find-today)
                 (lambda ()
                   (shell-command (concat "cd " org-directory " && make go")))
                 )

(after! org-roam

  (require 'find-lisp)
  (defun hugo-page-publish (file)
    (with-current-buffer (find-file-noselect file)
      (setq org-hugo-base-dir org-directory)
      (let ((org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$")))
        (org-hugo-export-wim-to-md))))

  (defun org-roam-export-all-as-hugo ()
    "re-exports all org-roam files to hugo markdown."
    (interactive)
    (dolist (f (org-roam--list-files org-roam-directory))
      (hugo-page-publish f)
      ))
  )

(use-package! lsp-ui
              :config
              (setq lsp-ui-auto-refresh t))

(use-package! keyfreq
  :config
  (require 'keyfreq)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(after! vulpea

  ;;* dynamic agenda https://github.com/brianmcgillion/doomd/blob/master/config.org
  ;; https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
  ;; The 'roam-agenda' tag is used to tell vulpea that there is a todo item in this file
  (add-to-list 'org-tags-exclude-from-inheritance "roam-agenda")

  (require 'vulpea)

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-project-update-tag (&optional arg)
    "Update PROJECT tag in the current buffer."
    (interactive "P")
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "roam-agenda" tags))
            (setq tags (remove "roam-agenda" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  ;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
  (defun my/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

  (defun my/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
            (seq-filter
             (my/org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))

  (defun dynamic-agenda-files-advice (orig-val)
    (let ((roam-agenda-files (delete-dups (my/org-roam-list-notes-by-tag "roam-agenda"))))
      (cl-union orig-val roam-agenda-files :test #'equal)))

  (add-hook 'before-save-hook #'vulpea-project-update-tag)
  (advice-add 'org-agenda-files :filter-return #'dynamic-agenda-files-advice)

        )
