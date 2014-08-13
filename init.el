;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fly in EMACS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://pages.sachachua.com/.emacs.d/Sacha.html
(setq emacs-load-start-time (current-time))

;; (require 'setup-package)
;; (require 'setup-backup-history)
;; (require 'setup-window)
;; (require 'setup-modeline)

;; This sets up the load path so that we can override it
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
;----------------------------------------------------------------------------
; Functions (load all files in defuns-dir)
; Copied from https://github.com/magnars/.emacs.d/blob/master/init.el
;----------------------------------------------------------------------------
;; (setq defuns-dir (expand-file-name "~/.emacs.d"))
;; (dolist (file (directory-files defuns-dir t "\\w+"))
;;   (when (file-regular-p file)
;;       (load file)))

(package-initialize)
(setq package-enable-at-startup nil)

;; (solarized-dark)
;; (setq custom-enabled-themes (quote (solarized-dark)))
;; (setq custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package management
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add the path(always http) to fetch packages.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t) ;t for add to the end of list
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; Always load newest byte code
(setq load-prefer-newer t)

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



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; files management
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length 1024)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windows management
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1))
(setq inhibit-startup-screen t)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))			;default 8
;; frame buffer - buffer or file name
(setq frame-title-format
      '("" invocation-name " xxx - " (:eval (if (buffer-file-name)
						   (abbreviate-file-name (buffer-file-name))
						 "%b"))))
(setq visible-bell t)			;no audio but flash
(setq-default indicate-empty-lines t)	;indicate the empty lines
(setq-default show-trailing-whitespace t) ;show tailing whitespace
(setq line-number-mode t)
(setq column-number-mode t)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; (global-visual-line-mode 1) ; 1 for on, 0 for off.

;; add var in .bashrc
;; export ALTERNATE_EDITOR=""
;; export EDITOR=emacsclient
;; for emacs daemon start without frame, set for "emacsclient -c"
(setq window-system-default-frame-alist
      '(
        ;; if frame created on x display
        (x
	 (menu-bar-lines . 1)
	 (tool-bar-lines . nil)
	 ;; mouse
	 (mouse-wheel-mode . 1)
	 ;; (mouse-wheel-follow-mouse . t)
	 (mouse-avoidance-mode . 'exile)
	 ;; face
	 ;; (font . "文泉驿等宽微米黑 8")
	 )
        ;; if on term
        (nil
	 (menu-bar-lines . 0) (tool-bar-lines . 0)
	 ;; (background-color . "black")
	 ;; (foreground-color . "white")
	 )
	)
      )

;; ------------------------------------------------------------
;; winner-->C-c <left> C-c <right>
;; ------------------------------------------------------------
(require 'winner)
(winner-mode 1)

;; ------------------------------------------------------------
;; move among windows
;; ------------------------------------------------------------
(require 'windmove)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)
(global-set-key (kbd "C-x C-<left>") 'windmove-left)
(global-set-key (kbd "C-x C-<up>") 'windmove-up)
(global-set-key (kbd "C-x C-<down>") 'windmove-down)

;; optmize window/buffer split
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vetically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)


(defun swap-windows ()
  (interactive)
  (let ((current-buf (current-buffer))
        (other-buf (progn
                     (other-window 1)
                     (current-buffer))))
    (switch-to-buffer current-buf)
    (other-window -1)
    (switch-to-buffer other-buf)))

(defun change-split (&optional arg)
  "Change arrangement of two windows from 'stacked' to 'side-by-side'.
With a prefix arg, change arrangement from 'side-by-side' to 'stacked'."
  (interactive "P")
  (let ((split-function (progn
                          (if arg
                              (lambda () (split-window-below))
                            (lambda () (split-window-right)))))
        (current-buf (current-buffer))
        (other-buf (progn
                     (other-window 1)
                     (current-buffer))))
    (delete-other-windows)
    (funcall split-function)
    (switch-to-buffer current-buf)))

;; M-s w[indow]
(global-set-key (kbd "M-s w") nil) ;; Remove the old keybinding
(global-set-key (kbd "M-s w x") 'swap-windows)
(global-set-key (kbd "M-s w s") 'change-split)

;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
;; (defun split-window-func-with-other-buffer (split-function)
;;   (lexical-let ((s-f split-function))
;;     (lambda ()
;;       (interactive)
;;       (funcall s-f)
;;       (set-window-buffer (next-window) (other-buffer)))))

;; (global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
;; (global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
;; (defun split-window-horizontally-instead ()
;;   (interactive)
;;   (save-excursion
;;     (delete-other-windows)
;;     (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

;; (defun split-window-vertically-instead ()
;;   (interactive)
;;   (save-excursion
;;     (delete-other-windows)
;;     (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

;; (global-set-key "\C-x|" 'split-window-horizontally-instead)
;; (global-set-key "\C-x_" 'split-window-vertically-instead)


;; modeline
(require 'smart-mode-line)

(sml/setup)
(sml/apply-theme 'dark)
;; (sml/apply-theme 'light)
;; (sml/apply-theme 'respectful)

;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-front-space
;;                 mode-line-mule-info
;;                 mode-line-client
;;                 mode-line-modified
;;                 mode-line-remote
;;                 mode-line-frame-identification
;;                 mode-line-buffer-identification
;;                 "   "
;;                 mode-line-position
;;                 (vc-mode vc-mode)
;;                 "  "
;;                 mode-line-modes
;;                 mode-line-misc-info
;;                 mode-line-end-spaces))

;; (add-to-list 'auto-mode-alist '("routes$" . conf-space-mode))

;; diminish
;; to minimize the mode name to fewer text
(require 'diminish)

(diminish 'abbrev-mode "Abv")
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))


;; minibuffer
;; type ‘C-M-e’ to go do your additions in a nice full buffer (with text mode) instead
(require 'miniedit)
(miniedit-install)


;; speedbar
;; Start speedbar automatically if we're using a window system like X, etc
;; (when window-system
;;    (speedbar t))


;; undo tree
;; This lets you use C-x u (undo-tree-visualize) to visually walk
;; through the changes you've made, undo back to a certain point
;;(or redo), and go down different branches.
;; (use-package undo-tree
;;   :init
;;   (progn
;;     (global-undo-tree-mode)
;;     (setq undo-tree-visualizer-timestamps t)
;;     (setq undo-tree-visualizer-diff t)))
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)


;; It's hard to remember keyboard shortcuts. The guide-key package pops up help after a short delay.
(require 'guide-key)
(setq guide-key/guide-key-sequence
      '("C-x" "C-x r" "C-x 4"
        "C-c" "C-c p" "C-c g"
        "M-s" "M-s w"
        "C-h"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'right)

;; Openwith
(my/package-install 'openwith)
(use-package openwith
  :init
  (openwith-mode t)
  :config
  (setq openwith-associations
	(quote (("\\.\\(?:pdf\\|ps\\)\\'" "okular" (file))
		("\\.\\(?:mp3\\|wav\\|flac\\)\\'" "gmusicbrowser" (file))
		("\\.\\(?:mpe?g\\|avi\\|wmv\\|flv\\|mov\\|mp4\\|ogg\\)\\'" "smplayer" (file))
		;; ("\\.\\(?:jpe?g\\|png\\|bmp\\)\\'" "gwenview" (file))
		("\\.chm\\'" "kchmviewer" (file))
		("\\.\\(?:odt\\|doc\\|docx\\)\\'" "libreoffice" ("--writer" file))
		("\\.\\(?:ods\\|xls\\|xlsx\\)\\'" "libreoffice" ("--calc" file))
		("\\.\\(?:odp\\|pps\\|ppt\\|pptx\\)\\'" "libreoffice" ("--impress" file))
		("\\.dia\\'" "dia" (file))))));; helm-swoop--a fast way to search things
(my/package-install 'helm-swoop)
(use-package helm-swoop
  :bind (("C-S-s" . helm-swoop)))

;; grep-a-lot--mult grep buffer
(my/package-install 'grep-a-lot)
(use-package grep-a-lot
  :init
  (grep-a-lot-setup-keys))

;; Browse-kill-ring - see what you've cut so that you can paste it
(my/package-install 'browse-kill-ring)
(use-package browse-kill-ring
  :init
  (progn
    (browse-kill-ring-default-keybindings) ;; M-y
    (setq browse-kill-ring-quit-action 'save-and-restore)))

;; samrtscan
(my/package-install 'smartscan)
(use-package smartscan
  :init (global-smartscan-mode t)
  :bind (("M-n" . smartscan-symbol-go-forwar)
	 ("M-p" . smartscan-symbol-go-backward)))

;; (require 'find-dired)
;; (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))


;; yasnippet for make fast insert
(my/package-install 'yasnippet)
(use-package yasnippet
  :init
  (progn
    (yas-global-mode 1)
    ))


;; auto insert pairs
(my/package-install 'smartparens)
(use-package smartparens
  :init
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)))

(my/package-install 'rainbow-delimiters)
(use-package rainbow-delimiters
  :init (global-rainbow-delimiters-mode))

;; Highlight certain contents - by press C-;
;; Edit one of the occurrences The change is applied to other occurrences simultaneously
;; Finish - by pressing C-; again
(my/package-install 'iedit)
(use-package iedit)

;; expand-region
(my/package-install 'expand-region)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; helm/ido mode
(my/package-install 'helm)
(use-package helm
  :init
  (progn
    (require 'helm-config)
    ;; (setq helm-candidate-number-limit 10)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
	  helm-input-idle-delay 0.01  ; this actually updates things
					; relatively quickly.
	  helm-quick-update t
	  helm-M-x-requires-pattern nil
	  helm-ff-skip-boring-files t)
    (helm-mode))
  :config
  (progn
    ;; I don't like the way switch-to-buffer uses history, since
    ;; that confuses me when it comes to buffers I've already
    ;; killed. Let's use ido instead.
    (add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . ido))))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally


;; ido-mode is awesome. Let's make it awesomer.
;; I usually want to go to recently-opened files first.
(my/package-install 'ido)
(use-package ido
  :init
  (progn
    ;; (ido-mode 1)
    ;; (ido-mode (quote both) nil (ido))
    (ido-mode (quote both))
    (setq ido-everywhere t)
    (setq ido-enable-flex-matching t)
    (setq ido-max-directory-size 100000)
    (setq ido-default-buffer-method 'selected-window)
    ;; (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
    ;; (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
    (defun ido-sort-mtime ()
      (setq ido-temp-list
	    (sort ido-temp-list
		  (lambda (a b)
		    (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
			  (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
		      (if (= (nth 0 ta) (nth 0 tb))
			  (> (nth 1 ta) (nth 1 tb))
			(> (nth 0 ta) (nth 0 tb)))))))
      (ido-to-end  ;; move . files to end (again)
       (delq nil (mapcar
		  (lambda (x) (if (string-equal (substring x 0 1) ".") x))
		  ido-temp-list))))))

;; recent mode
(my/package-install 'recentf)
(use-package recentf
  :init
  (progn
    (setq recentf-max-saved-items 200
          recentf-exclude '("/tmp/"
                            "/ssh:"
                            "/sudo:"
                            "/home/[a-z]\+/\\.")
	  recentf-max-menu-items 15)
    (recentf-mode t)))
(setq recentf-keep '(file-remote-p file-readable-p))

(defun file-name-with-one-directory (file-name)
  (concat (cadr (reverse (split-string file-name "/"))) "/"
          (file-name-nondirectory file-name)))

(defun recentf--file-cons (file-name)
  (cons (file-name-with-one-directory file-name) file-name))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let* ((recent-files (mapcar 'recentf--file-cons recentf-list))
         (files (mapcar 'car recent-files))
         (file (ido-completing-read "Choose recent file: " files))) ;philip add ido
    (find-file (cdr (assoc file recent-files)))))

;; ;; ido settings
;; (defun ido-recentf-open ()
;;   "Use `ido-completing-read' to \\[find-file] a recent file"
;;   (interactive)
;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;;       (message "Opening file...")
;;     (message "Aborting")))

;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; (global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

;; ------------------------------------------------------------
;; Smart M-x is smart
;; ------------------------------------------------------------
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)



;; (require 'server)
;; (or (server-running-p)
;;     (server-start))

(require 'rainbow-identifiers)
;; (rainbow-identifiers-mode t)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(define-key emacs-lisp-mode-map (kbd "M-s e b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "M-s e r") 'eval-region)
(fset 'yes-or-no-p 'y-or-n-p)		;replace yes/no with y/n
(setq uniquify-buffer-name-style (quote post-forward))
(show-paren-mode t)
(icomplete-mode 1)
(setq dired-recursive-copies (quote always))
(setq tab-width 4)
(global-set-key (kbd "RET") 'newline-and-indent)
(delete-selection-mode t)		;delete the selection with a keypress
;; (set-cursor-color "#FF5A0E")		;cursor color is orange
(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 8)	    ;; but maintain correct appearance

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; text size
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

;; codeing system
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; file association
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; killing text
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))
(bind-key "C-x p" 'pop-to-mark-command)	;goto previous mark position
(setq set-mark-command-repeat-pop t)

;; smarter-move-beginning-of-line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
		'smarter-move-beginning-of-line)

;; (my/package-install 'multiple-cursors)
;; (use-package multiple-cursors
;;   :bind
;;    (("C->" . mc/mark-next-like-this)
;;     ("C-<" . mc/mark-previous-like-this)
;;     ("C-*" . mc/mark-all-like-this)))

;; tranpose
(global-set-key (kbd "M-t") nil) ;; Remove the old keybinding
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t e") 'transpose-sexps)
(global-set-key (kbd "M-t s") 'transpose-sentences)
(global-set-key (kbd "M-t p") 'transpose-paragraphs)

;; occur settings
(defun occur-rename-buffer-after-search-string ()
  "Uniquify name of *Occur* buffer by appending search string to it."
  (let* ((beg-end (match-data (string-match "\".+\"" (buffer-string))))
         (beg (+ (car beg-end) 2))
         (end (cadr beg-end))
         (search-string (buffer-substring-no-properties beg end)))
    (rename-buffer (format "*Occur-%s*" search-string))))

(add-hook 'occur-hook 'occur-rename-buffer-after-search-string)

(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

;; make grep buffer writable
(require 'wgrep)
(setq wgrep-enable-key "r")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq projectile-keymap-prefix (kbd "C-c p"))
(my/package-install 'projectile)
(use-package projectile
  :init
  (progn
    (projectile-global-mode)
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)))
(my/package-install 'helm-projectile)
(use-package helm-projectile)

;; Don't show whitespace in diff, but show context
(setq vc-diff-switches '("-b" "-B" "-u"))
(which-function-mode 1)

;; lisp
(setq edebug-trace t)			;edebug trace function
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(defun my/set-up-emacs-lisp-mode ()
  "Set up some conveniences for Emacs Lisp."
  (turn-on-eldoc-mode)
  (local-set-key (kbd "C-c f") 'find-function)
  (define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point))
(add-hook 'emacs-lisp-mode-hook 'my/set-up-emacs-lisp-mode)
(add-hook 'lisp-interaction-mode-hook 'my/set-up-emacs-lisp-mode)
(add-hook 'ielm-mode-hook 'my/set-up-emacs-lisp-mode)

;; gtags
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'gtags-mode-hook
          '(lambda ()
             ;;                            ; Local customization (overwrite key mapping)
             ;; (define-key gtags-mode-map "\C-f" 'scroll-up)
             ;; (define-key gtags-mode-map "\C-b" 'scroll-down)
             ))
(add-hook 'gtags-select-mode-hook
          '(lambda ()
             (setq hl-line-face 'underline)
             (hl-line-mode 1)
             ))
(defun my/gtags-mode
  (lambda ()
    (gtags-mode t)))
(add-hook 'c-mode-hook
          '(lambda ()
             (gtags-mode t)))
(add-hook 'c++-mode-hook
          '(lambda ()
             (gtags-mode t)))
(add-hook 'java-mode-hook
          '(lambda ()
             (gtags-mode t)))
(add-hook 'asm-mode-hook
          '(lambda ()
             (gtags-mode t)))
(add-hook 'makefile-mode-hook
          '(lambda ()
             (gtags-mode t)))

(setq gtags-suggested-key-mapping t)    ;gtags key mapping
(setq gtags-auto-update t)

;; (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
;; (local-set-key "\M-f" 'c-forward-into-nomenclature)
;; (local-set-key "\M-b" 'c-backward-into-nomenclature)
(setq cc-search-directories '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include"))
(setq c-basic-offset 4)
(setq c-style-variables-are-local-p nil)
;give me NO newline automatically after electric expressions are entered
(setq c-auto-newline nil)
;if (0) becomes if (0)
; { {
; ; ;
; } }
(c-set-offset 'substatement-open 0)
;first arg of arglist to functions: tabbed in once
;(default was c-lineup-arglist-intro-after-paren)
(c-set-offset 'arglist-intro '+)
;second line of arglist to functions: tabbed in once
;(default was c-lineup-arglist)
(c-set-offset 'arglist-cont-nonempty '+)
;switch/case: make each case line indent from switch
(c-set-offset 'case-label '+)
;make the ENTER key indent next line properly
(local-set-key "\C-m" 'newline-and-indent)
;syntax-highlight aggressively
;(setq font-lock-support-mode 'lazy-lock-mode)
(setq lazy-lock-defer-contextually t)
(setq lazy-lock-defer-time 0)
;make DEL take all previous whitespace with it
;; (c-toggle-hungry-state 1)
;make open-braces after a case: statement indent to 0 (default was '+)
(c-set-offset 'statement-case-open 0)
;make a #define be left-aligned
(setq c-electric-pound-behavior (quote (alignleft)))

(require 'hippie-exp-ext)
(global-set-key (kbd "C-@") 'hippie-expand-dabbrev-limited-chars)
(global-set-key (kbd "M-/") 'hippie-expand-file-name)

;; (autoload 'doxygen-insert-function-comment "doxygen" "insert comment for the function at point" t)
;; (autoload 'doxygen-insert-file-comment "doxygen" "insert comment for file" t)
;; (autoload 'doxygen-insert-member-group-region "doxygen" "insert comment for member group" t)
;; (autoload 'doxygen-insert-compound-comment "doxygen" "insert comment for compound" t)

(require 'highlight-symbol)
(global-set-key (kbd "C-c h") nil)
(global-set-key (kbd "C-c h l") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c h n") 'highlight-symbol-next)
(global-set-key (kbd "C-c h p") 'highlight-symbol-prev)
(global-set-key (kbd "C-c h r") 'highlight-symbol-query-replace)


;; stardict
(require 'sdcv-mode)
(global-set-key (kbd "M-?") 'sdcv-search)

;; ;; Display Emacs Startup Time
;; (add-hook 'after-init-hook (lambda ()
;;                              (growl-notify-notification "Emacs Startup" (format "The init sequence took %s." (emacs-init-time)))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It's not the end. It's just the end of beginning ...
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "It's not the end. It's just the end of beginning ...")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
