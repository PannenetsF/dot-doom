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
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")


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
  :hook ('org-mode-hook . 'org-zotxt-mode)
  :init
  ;; code here will run immediately
  (require 'request)
  (require 'deferred)
  (require 'org-zotxt-noter)
  (require 'org-noter)
  (require 'org-zotxt)
  (setq org-zotxt-notes-directory (expand-file-name "zotero/" org-roam-directory)
   org-zotxt-link-description-style :citation 
   zotxt-default-bibliography-style "ieee")

  (defun jump-to-zotxt-note-by-search ()
    "Go to Zotero note via searching. Create the note file if it does not exist"
    (interactive)
    (deferred:$
     ;; step1: search for Zotero items
     (zotxt-search-deferred :title-creator-year)
     (deferred:nextc it
                     (lambda (items)
                       (message "Zotxt step1")
                       (when (null items)
                         (error "No Zotero items found."))
                       (car items)))

     ;; step2: get the full item details
     (deferred:nextc it
                     (lambda (item)
                       (message "Zotxt step2")
                       (zotxt-get-item-deferred item :full)))

     ;; step3: generate note file path and pass context
     (deferred:nextc it
                     (lambda (full-item)
                       (message "Zotxt step3")
                       (let* ((item-key (plist-get full-item :key))
                              (note-directory org-zotxt-notes-directory)
                              (note-file (concat note-directory item-key ".org")))
                         (make-directory note-directory t)
                         (cons full-item note-file))))

     ;; step4: create or open the note file
     (deferred:nextc it
                     (lambda (full-item-note-file)
                       (message "Zotxt step4")
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
                             (insert (format "#+TITLE: %s\n" item-title))
                             (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
                             (insert "* Meta Info\n")
                             ))
                         (find-file note-file)
                         full-item
                         )
                       ))

     ;; step5: get the properties and paths
     (deferred:nextc it
                     (lambda (item)
                       (message "Zotxt step5.1")
                       (zotxt-get-item-deferred item :paths)))
     (deferred:nextc it
                     (lambda (item)
                       (message "Zotxt step5.2")
                       (message (prin1-to-string item))
                       (org-zotxt-get-item-link-text-deferred item)))
     (deferred:nextc it
                     (lambda (resp)
                       (message "Zotxt step5.3")
                       (let ((path (org-zotxt-choose-path (cdr (assq 'paths (plist-get resp :paths))))))
                         (beginning-of-buffer)
                         (org-entry-put (point) org-zotxt-noter-zotero-link (org-zotxt-make-item-link resp))
                         (org-entry-put (point) org-noter-property-doc-file path)
                         (save-buffer)
                         )
                       ))


     ;; step6: error handling
     (deferred:error it
                     (lambda (err)
                       (message "Zotxt Failed for : %s" (error-message-string err))
                       (signal 'user-error (list "Canceled"))))
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
        org-roam-ui-open-on-start t
        )
  )


(after! org
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
        )

  (map! :leader
        (:prefix-map ("X" . "Org")
         :desc "org refresh statistics of current line" "R" #'org-update-statistics-cookies
         :desc "org cycle todo state" "t" #'org-todo
         :desc "org toggle checkbox" "x" #'org-toggle-checkbox
         :desc "org roam refile" "r" #'org-roam-refile
         :desc "org roam insert" "i" #'org-roam-node-insert
         :desc "org roam capture" "c" #'org-roam-capture
         :desc "org roam find" "f" #'org-roam-node-find
         :desc "org roam ui toggle" "u" #'org-roam-ui-mode
         :desc "org roam ui follow toggle" "F" #'org-roam-ui-follow-mode
         :desc "org capture" "C" #'org-capture
         )
        )
  )

;; 5. disable quit confirmation
(setq confirm-kill-emacs nil)

;; 6. accelerate tramp
(use-package tramp
  :config
  (setq tramp-auto-save-directory "/tmp/tramp-autosaves/"
        tramp-terminal-type "tramp"
        tramp-backup-directory-alist backup-directory-alist
        remote-file-name-inhibit-cache 60 ; 加速，允许 cache
        remote-file-name-inhibit-locks t ; 加速，不会使用文件锁
        tramp-verbose 0 ; 加速，更少的 tramp 信息
        vc-handled-backends '(SVN Git) ; 加速，禁用一些版本控制后端
        )
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq tramp-chunksize 2000)
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  )

;; vim-like keybindings
(map! :leader
      :desc "Open File Tree" "e" #'+treemacs/toggle)
