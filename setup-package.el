(require 'package)
(require 'dash)

;; add the path(always http) to fetch packages.
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ("org" . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives source t)) ; t for add to the end of list

(package-initialize)
(setq package-enable-at-startup nil)

;; informs Emacs about the latest versions of all packages
(unless package-archive-contents
  (package-refresh-contents))

;; All packages should be managed at here.
(let ((packages '(
                  ;; package manage
                  use-package
                  ;; window & buffer
                  winner
                  windmove
                  ;; mode line
                  smart-mode-line
                  diminish
                  ;; mini buffer
                  miniedit              ;
                  ;; key map
                  guide-key
                  ;; good further
                  helm
                  ido
                  smex                  ;M-x interface with Ido-style fuzzy matching.
                  ;; search
                  helm-swoop
                  grep-a-lot
                  wgrep
                  iedit                 ;Highlight certain contents.Edit multiple regions in the same way simultaneously.
                  recentf
                  openwith
                  undo-tree
                  browse-kill-ring
                  smartscan
                  smartparens
                  rainbow-delimiters
                  rainbow-identifiers
                  highlight-symbol
                  expand-region
                  projectile
                  helm-projectile
                  hippie-exp-ext
                  yasnippet
                  )))
  (dolist (package packages)
    (let ((package-archives (if repository
				(list (assoc repository package-archives))
			      package-archives)))
      (package-install package))))

;; allow single install
(defun my/package-install (package &optional repository)
  "Install PACKAGE if it has not yet been installed.
If REPOSITORY is specified, use that."
  (unless (package-installed-p package)
    (let ((package-archives (if repository
				(list (assoc repository package-archives))
			      package-archives)))
      (package-install package))))

(defun my/byte-recompile ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0)
  (byte-recompile-directory "~/.emacs.d/elisp" 0))

(my/package-install 'use-package)
(require 'use-package)


(provide 'setup-package)
