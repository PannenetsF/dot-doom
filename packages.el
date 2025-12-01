;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

;; zotxt for zotero
(package! zotxt
  :recipe (:host github :repo "egh/zotxt-emacs"))
;; dependency for zotxt: request, deferred 
(package! request
  :recipe (:host github :repo "tkf/emacs-request"))
(package! deferred
  :recipe (:host github :repo "kiwanami/emacs-deferred"))


(package! org-modern
  :recipe (:host github :repo "minad/org-modern"))

(package! all-the-icons
  :recipe (:host github :repo "domtronn/all-the-icons.el" )) ;; :files ("*.el")))

(package! nerd-icons
  :recipe (:host github :repo "rainstormstudio/nerd-icons.el")) ;; :files ("*.el")))

;; for org-roam visualization
(unpin! org-roam)
(package! org-roam-ui)

;; for github copilot
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

;; for mermaid org mode integration
(package! mermaid-mode)
(package! ob-mermaid)

;; org latex impatient 
(package! org-latex-impatient
  :recipe (:host github :repo "yangsheng6810/org-latex-impatient")) ;; :files ("*.el")))

(package! org-elp)

;; key freq
(package! keyfreq
  :recipe (:host github :repo "dacap/keyfreq" :files ("*.el")))

;; org-fragtog
(package! org-fragtog
  :recipe (:host github :repo "io12/org-fragtog" :files ("*.el")))

(package! vulpea
  :recipe (:host github :repo "d12frosted/vulpea" :files ("*.el")))
