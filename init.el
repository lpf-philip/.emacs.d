;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fly in EMACS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This sets up the load path so that we can override it
(package-initialize nil)
(add-to-list 'load-path "~/.emacs.d/elisp/")
(setq load-path (cons "~/.emacs.d" load-path))
(package-initialize t)
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
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
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
	 ;; ;; mouse
	 ;; (mouse-wheel-mode . 1)
	 ;; (mouse-wheel-follow-mouse . t)
	 ;; (mouse-avoidance-mode . 'exile)
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


;; winner-->C-c <left> C-c <right>
(my/package-install 'winner)
(use-package winner
  :config (winner-mode 1))


;; move among windows
(my/package-install 'windmove)
(use-package windmove
  :bind
  (("C-x C-<right>" . windmove-right)
   ("C-x C-<left>" . windmove-left)
   ("C-x C-<up>" . windmove-up)
   ("C-x C-<down>" . windmove-down)))


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


;; modeline
(my/package-install 'smart-mode-line)
(use-package smart-mode-line
  :init
  (progn
    (setq-default
     mode-line-format
     '("%e"
       mode-line-front-space
       mode-line-mule-info
       mode-line-client
       mode-line-modified
       mode-line-remote
       mode-line-frame-identification
       mode-line-buffer-identification
       "   "
       mode-line-position
       (vc-mode vc-mode)
       "  "
       mode-line-modes
       mode-line-misc-info
       mode-line-end-spaces))))

;; (add-to-list 'auto-mode-alist '("routes$" . conf-space-mode))

;; diminish to minimize the mode name to fewer text
(my/package-install 'diminish)
(use-package diminish
  :init
  (progn
    ;; (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
    (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
    (eval-after-load "smartparens" '(diminish 'smartparens-mode))
    (eval-after-load "guide-key" '(diminish 'guide-key-mode))
    (eval-after-load "eldoc" '(diminish 'eldoc-mode))
    (diminish 'visual-line-mode)))

;; minibuffer
(my/package-install 'miniedit)
(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

;; speedbar
;; Start speedbar automatically if we're using a window system like X, etc
;; (when window-system
;;    (speedbar t))

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
    (setq helm-candidate-number-limit 10)
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
    (add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . ido)))
  :bind (("C-c h" . helm-mini)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

;; undo tree
;; This lets you use C-x u (undo-tree-visualize) to visually walk
;; through the changes you've made, undo back to a certain point
;;(or redo), and go down different branches.
(my/package-install 'undo-tree)
(use-package undo-tree
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; It's hard to remember keyboard shortcuts. The guide-key package
;; pops up help after a short delay.
(my/package-install 'guide-key)
(use-package guide-key
  :init
  (setq guide-key/guide-key-sequence
        '("C-x" "C-x r" "C-x 4"
          "C-c" "C-c p"
          "M-s"
          "C-h"))
  (guide-key-mode 1))

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

;; recent mode
(my/package-install 'recentf)
(use-package recentf
  :init
  (progn
    (setq recentf-max-saved-items 200
	  recentf-max-menu-items 15)
    (recentf-mode t)))

;; yasnippet for make fast insert
(my/package-install 'yasnippet)
(use-package yasnippet
  :init
  (progn
    (yas-global-mode 1)
    ))

(my/package-install 'iedit)
(use-package iedit)

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

;; smex
(my/package-install 'smex)
(use-package smex
  :init
  (smex-initialize))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; (require 'server)
;; (or (server-running-p)
;;     (server-start))

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


;; ido settings
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)



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
(add-hook 'c-mode-hook
	  '(lambda ()
	     (gtags-mode t)
	     ))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It's not the end. It's just the end of beginning ...
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "It's not the end. It's just the end of beginning ...")
