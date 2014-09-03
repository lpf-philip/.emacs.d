;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;; fly in emacs ;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package management
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize nil)

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
;; add the path(always http) to fetch packages.
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ("org" . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives source t)) ; t for add to the end of list

(package-initialize t)
(setq package-enable-at-startup nil)

;; informs Emacs about the latest versions of all packages
(unless package-archive-contents
  (package-refresh-contents))

;; ;; All packages should be managed at here.
;; (let ((packages '(
;;                   ;; package manage
;;                   use-package
;;                   ;; window & buffer
;;                   winner
;;                   windmove
;;                   ;; mode line
;;                   smart-mode-line
;;                   diminish
;;                   ;; mini buffer
;;                   miniedit              ;
;;                   ;; key map
;;                   guide-key
;;                   ;; good further
;;                   helm
;;                   ido
;;                   smex                  ;M-x interface with Ido-style fuzzy matching.
;;                   ;; search
;;                   helm-swoop
;;                   grep-a-lot
;;                   wgrep
;;                   iedit                 ;Highlight certain contents.Edit multiple regions in the same way simultaneously.
;;                   recentf
;;                   openwith
;;                   undo-tree
;;                   browse-kill-ring
;;                   smartscan
;;                   smartparens
;;                   rainbow-delimiters
;;                   rainbow-identifiers
;;                   highlight-symbol
;;                   expand-region
;;                   projectile
;;                   helm-projectile
;;                   hippie-exp-ext
;;                   yasnippet
;;                   )))
;;   (dolist (package packages)
;;     (let (
;;           ;; (package-archives (if repository
;; 	  ;;       		(list (assoc repository package-archives))
;; 	  ;;       	      package-archives))
;;           )
;;       (package-install package))))

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



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color-theme
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'color-theme)
;; (color-theme-initialize)

(setq custom-safe-themes (quote         ;color-hash-value
                          ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" ;smart-mode-line
                           "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" ;solarized-dark
                           "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" ;solarized-light
                           "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" ;solarized-dark
                           "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" ;solarized
                           default)))

;; (require 'solarized-theme)
;; (setq solarized-distinct-fringe-background t)
;; (setq solarized-high-contrast-mode-line t)
;; (setq solarized-use-less-bold t)
(setq solarized-use-more-italic t)
(setq solarized-emphasize-indicators nil)
(setq x-underline-at-descent-line t)

(load-theme 'solarized-dark t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:background "SkyBlue4" :foreground "black")))))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(define-key emacs-lisp-mode-map (kbd "M-s e b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "M-s e r") 'eval-region)

(fset 'yes-or-no-p 'y-or-n-p)		;replace yes/no with y/n
(global-set-key (kbd "RET") 'newline-and-indent)

(show-paren-mode t)
(icomplete-mode 1)

(delete-selection-mode t)		;delete the selection with a keypress
;; (set-cursor-color "#FF5A0E")		;cursor color is orange
(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 8)	    ;; but maintain correct appearance
(setq dired-recursive-copies (quote always))

(setq uniquify-buffer-name-style (quote post-forward))
;; (setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(global-set-key (kbd "C-x p") 'pop-to-mark-command)	;goto previous mark position
(setq set-mark-command-repeat-pop t)

(mouse-avoidance-mode 'animate)         ;move mouse away when curser in

;; ------------------------------------------------------------
;; emacs garbage collection system settings
;; ------------------------------------------------------------
;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; ------------------------------------------------------------
;; text size
;; ------------------------------------------------------------
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; ------------------------------------------------------------
;; codeing system
;; ------------------------------------------------------------
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; ------------------------------------------------------------
;; file association
;; ------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))


;; ------------------------------------------------------------
;; Openwith
;; ------------------------------------------------------------
(require 'openwith)
(openwith-mode t)
(setq openwith-associations
      (quote (("\\.\\(?:pdf\\|ps\\)\\'" "okular" (file))
              ("\\.\\(?:mp3\\|wav\\|flac\\)\\'" "gmusicbrowser" (file))
              ("\\.\\(?:mpe?g\\|avi\\|wmv\\|flv\\|mov\\|mp4\\|ogg\\)\\'" "smplayer" (file))
              ;; ("\\.\\(?:jpe?g\\|png\\|bmp\\)\\'" "gwenview" (file))
              ("\\.chm\\'" "kchmviewer" (file))
              ("\\.\\(?:odt\\|doc\\|docx\\)\\'" "libreoffice" ("--writer" file))
              ("\\.\\(?:ods\\|xls\\|xlsx\\)\\'" "libreoffice" ("--calc" file))
              ("\\.\\(?:odp\\|pps\\|ppt\\|pptx\\)\\'" "libreoffice" ("--impress" file))
              ("\\.dia\\'" "dia" (file)))))


;; ------------------------------------------------------------
;; guide-key - keyboard shortcuts. lazy boy
;; ------------------------------------------------------------
(require 'guide-key)
(setq guide-key/guide-key-sequence
      '("C-x" "C-x r" "C-x 4"
        "C-c" "C-c p" "C-c g"
        "M-s" "M-s w"
        "M-g"
        "C-h"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'right)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearence management
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
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
	 )))


;; ------------------------------------------------------------
;; linum-mode
;; ------------------------------------------------------------
(global-linum-mode t)

;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
(setq linum-mode-inhibit-modes-list '(eshell-mode
                                      shell-mode
                                      erc-mode
                                      help-mode
                                      jabber-roster-mode
                                      jabber-chat-mode
                                      twittering-mode
                                      compilation-mode
                                      weibo-timeline-mode
                                      woman-mode
                                      Info-mode
                                      calc-mode
                                      calc-trail-mode
                                      comint-mode
                                      gnus-group-mode
                                      inf-ruby-mode
                                      gud-mode
                                      org-mode
                                      vc-git-log-edit-mode
                                      log-edit-mode
                                      term-mode
                                      w3m-mode
                                      speedbar-mode
                                      gnus-summary-mode
                                      gnus-article-mode
                                      calendar-mode))
(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
  (unless (member major-mode linum-mode-inhibit-modes-list)
    ad-do-it))
(ad-activate 'linum-on)


;; ------------------------------------------------------------
;; maxframe
;; ------------------------------------------------------------
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)


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
  (split-window-vertically)
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


(require 'window-number)
(window-number-mode 1)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------------------
;; sml - beauty mode line
;; ------------------------------------------------------------
(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'dark)
;; (sml/apply-theme 'light)
;; (sml/apply-theme 'respectful)


;; ------------------------------------------------------------
;; diminish - to minimize the mode name to fewer text
;; ------------------------------------------------------------
(require 'diminish)
(diminish 'abbrev-mode "Abv")
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minibuffer
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------------------
;; miniedit - edit minibuffer in full buffer: 'C-M-e'
;; ------------------------------------------------------------
(require 'miniedit)
(miniedit-install)


;; ------------------------------------------------------------
;; Smart M-x is smart
;; ------------------------------------------------------------
(require 'smex)
;; (autoload 'smex "smex" nil t)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------------------
;; iedit - highlight/edit certain contents - by press C-;
;; ------------------------------------------------------------
;; Highlight certain contents - by press C-;
;; Edit one of the occurrences The change is applied to other occurrences simultaneously
;; Finish - by pressing C-; again
(require 'iedit)

;; ------------------------------------------------------------
;; expand-region
;; ------------------------------------------------------------
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(require 'hippie-exp-ext)
(when window-system
  (global-set-key (kbd "C-@") 'hippie-expand-dabbrev-limited-chars))
(global-set-key (kbd "M-/") 'hippie-expand-file-name)


;; ------------------------------------------------------------
;; undo-tree
;; ------------------------------------------------------------
;; This lets you use C-x u (undo-tree-visualize) to visually walk
;; through the changes you've made, undo back to a certain point
;;(or redo), and go down different branches.
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;; ------------------------------------------------------------
;; Browse-kill-ring - see what you've cut so that you can paste it
;; ------------------------------------------------------------
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings) ;; M-y
(setq browse-kill-ring-quit-action 'save-and-restore)

;; ------------------------------------------------------------
;; helm mode
;; ------------------------------------------------------------
(require 'helm)
(require 'helm-config)
(setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
      helm-input-idle-delay 0.01  ; this actually updates things
					; relatively quickly.
      helm-quick-update t
      helm-M-x-requires-pattern nil
      helm-ff-skip-boring-files t)
(helm-mode 1)
;; I don't like the way switch-to-buffer uses history, since
;; that confuses me when it comes to buffers I've already
;; killed. Let's use ido instead.
(add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . ido))
;; (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
(global-set-key (kbd "C-c h") 'helm-mini)


;; ------------------------------------------------------------
;; ido-mode is awesome. Let's make it awesomer.
;; ------------------------------------------------------------
(require 'ido)
(ido-mode (quote both))
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-max-directory-size 100000)
(setq ido-default-buffer-method 'selected-window)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-filename-at-point nil)
(setq ido-use-virtual-buffers t)
;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; Use C-f during file selection to switch to regular find-file
(require 'ido-ubiquitous)

;; ------------------------------------------------------------
;; recent mode
;; ------------------------------------------------------------
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        "/home/[a-z]\+/\\.")
      recentf-max-menu-items 15)
(recentf-mode t)
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

(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)


;; ------------------------------------------------------------
;; helm-swoop--a fast way to search things
;; ------------------------------------------------------------
(require 'helm-swoop)
(global-set-key (kbd "C-S-s") 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; ;; Save buffer when helm-multi-swoop-edit complete
;; (setq helm-multi-swoop-edit-save t)
;; ;; If this value is t, split window inside the current window
;; (setq helm-swoop-split-with-multiple-windows nil)
;; ;; Split direction. 'split-window-vertically or 'split-window-horizontally
;; (setq helm-swoop-split-direction 'split-window-vertically)
;; ;; If nil, you can slightly boost invoke speed in exchange for text color
;; (setq helm-swoop-speed-or-color nil)
;; Helm Swoop Edit
;; While doing helm-swoop, press keybind [C-c C-e] to move to edit buffer.
;; Edit the list and apply by [C-x C-s]. If you'd like to cancel, [C-c C-g]


;; ------------------------------------------------------------
;; smartscan - Searching based on the current word
;; ------------------------------------------------------------
(require 'smartscan)
(global-smartscan-mode 1)


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


;; ------------------------------------------------------------
;; grep
;; ------------------------------------------------------------
;; grep-a-lot--mult grep buffer
;; M-g ]         Go to next search results buffer, restore its current search context
;; M-g [         Ditto, but selects previous buffer.
;; M-g -         Pop to previous search results buffer (kills top search results buffer)
;; M-g _         Clear the search results stack (kills all grep-a-lot buffers!)
;; M-g =         Restore buffer and position where current search started
(require 'grep-a-lot)
(grep-a-lot-setup-keys)
;; (grep-a-lot-advise igrep)

;; make grep buffer writable
(require 'wgrep)
(setq wgrep-enable-key "r")


;; ;; ------------------------------------------------------------
;; ;; auto-complete
;; ;; ------------------------------------------------------------
;; (require 'auto-complete)
;; (add-hook 'c-mode-common-hook '(lambda ()

;;                                  ;; ac-omni-completion-sources is made buffer local so
;;                                  ;; you need to add it to a mode hook to activate on 
;;                                  ;; whatever buffer you want to use it with.  This
;;                                  ;; example uses C mode (as you probably surmised).

;;                                  ;; auto-complete.el expects ac-omni-completion-sources to be
;;                                  ;; a list of cons cells where each cell's car is a regex
;;                                  ;; that describes the syntactical bits you want AutoComplete
;;                                  ;; to be aware of. The cdr of each cell is the source that will
;;                                  ;; supply the completion data.  The following tells autocomplete
;;                                  ;; to begin completion when you type in a . or a ->

;;                                  (add-to-list 'ac-omni-completion-sources
;;                                               (cons "\\." '(ac-source-semantic)))
;;                                  (add-to-list 'ac-omni-completion-sources
;;                                               (cons "->" '(ac-source-semantic)))

;;                                  ;; ac-sources was also made buffer local in new versions of
;;                                  ;; autocomplete.  In my case, I want AutoComplete to use 
;;                                  ;; semantic and yasnippet (order matters, if reversed snippets
;;                                  ;; will appear before semantic tag completions).

;;                                  (setq ac-sources '(ac-source-semantic ac-source-yasnippet))
;;                                  ))


;; ------------------------------------------------------------
;; yasnippet for make fast inser
;; ------------------------------------------------------------
(require 'yasnippet)
(yas-global-mode 1)
(my/package-install 'dropdown-list)
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))


;; ------------------------------------------------------------
;; company
;; ------------------------------------------------------------
(require 'company)
(autoload 'company-mode "company" nil t)
(add-hook 'prog-mode-hook 'global-company-mode)
(global-set-key (kbd "C-c o") 'company-complete)
(global-set-key (kbd "C-o") 'company-complete)
(setq company-require-match nil)
;; (setq company-dabbrev-downcase nil)
(setq company-show-numbers t)           ;first ten candiate
;; (setq company-idle-delay 0.2)
;; (setq company-clang-insert-arguments nil)

;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             (company-complete-common)
;;           (indent-for-tab-command)))))

;; (global-set-key [tab] 'tab-indent-or-complete)

;; (require 'color)
;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))




;; ------------------------------------------------------------
;; auto insert pairs
;; ------------------------------------------------------------
(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)

;; ------------------------------------------------------------
;; rainbow - colorful
;; ------------------------------------------------------------
(require 'rainbow-delimiters)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(global-rainbow-delimiters-mode)
(require 'rainbow-identifiers)
;; (rainbow-identifiers-mode t)


;; (use-package multiple-cursors
;;   :bind
;;    (("C->" . mc/mark-next-like-this)
;;     ("C-<" . mc/mark-previous-like-this)
;;     ("C-*" . mc/mark-all-like-this)))


;; ------------------------------------------------------------
;; tranpose
;; ------------------------------------------------------------
(global-set-key (kbd "M-t") nil) ;; Remove the old keybinding
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t e") 'transpose-sexps)
(global-set-key (kbd "M-t s") 'transpose-sentences)
(global-set-key (kbd "M-t p") 'transpose-paragraphs)


;; ------------------------------------------------------------
;; highlight
;; ------------------------------------------------------------
(require 'highlight-symbol)
(global-set-key (kbd "C-c l l") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c l n") 'highlight-symbol-next)
(global-set-key (kbd "C-c l p") 'highlight-symbol-prev)
(global-set-key (kbd "C-c l r") 'highlight-symbol-query-replace)


;; ------------------------------------------------------------
;; dictionary -- stardict
;; ------------------------------------------------------------
(require 'sdcv-mode)
(global-set-key (kbd "M-?") 'sdcv-search)


;; ------------------------------------------------------------
;; killing text
;; ------------------------------------------------------------
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))


;; ------------------------------------------------------------
;; smarter-move-beginning-of-line
;; ------------------------------------------------------------
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


;; ------------------------------------------------------------
;; multi-eshell
;; ------------------------------------------------------------
(require 'multi-eshell)

;; ------------------------------------------------------------
;; unicad - for languages environment
;; ------------------------------------------------------------
(require 'unicad)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't show whitespace in diff, but show context
(setq vc-diff-switches '("-b" "-B" "-u"))
(which-function-mode 1)

;; ------------------------------------------------------------
;; gtags
;; ------------------------------------------------------------
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'gtags-mode-hook
          '(lambda () ; Local customization (overwrite key mapping)
             ;; (define-key gtags-mode-map "\C-f" 'scroll-up)
             ;; (define-key gtags-mode-map "\C-b" 'scroll-down)
             ))
(add-hook 'gtags-select-mode-hook
          '(lambda ()
             (setq hl-line-face 'underline)
             (hl-line-mode 1)
             ))
(add-hook 'c-mode-hook '(lambda () (gtags-mode t)))
(add-hook 'c++-mode-hook '(lambda () (gtags-mode t)))
(add-hook 'java-mode-hook '(lambda () (gtags-mode t)))
(add-hook 'asm-mode-hook '(lambda () (gtags-mode t)))
(add-hook 'makefile-mode-hook '(lambda () (gtags-mode t)))
(setq gtags-suggested-key-mapping t)    ;gtags key mapping
(setq gtags-auto-update t)


;; ------------------------------------------------------------
;; projectile
;; ------------------------------------------------------------
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'default)
(setq projectile-enable-caching t)
(setq projectile-keymap-prefix (kbd "C-c p"))

(autoload 'doxygen-insert-function-comment "doxygen" "insert comment for the function at point" t)
(autoload 'doxygen-insert-file-comment "doxygen" "insert comment for file" t)
(autoload 'doxygen-insert-member-group-region "doxygen" "insert comment for member group" t)
(autoload 'doxygen-insert-compound-comment "doxygen" "insert comment for compound" t)

;; ------------------------------------------------------------
;; lisp
;; ------------------------------------------------------------
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


;; ------------------------------------------------------------
;; cc-mode
;; ------------------------------------------------------------

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

(global-set-key (kbd "C-x C-o") 'ffap)
(global-set-key (kbd "C-c m") 'helm-imenu)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(setq reftex-plug-into-AUCTeX t)

(mapc (lambda (mode)
      (add-hook 'LaTeX-mode-hook mode))
      (list 'auto-fill-mode
            'LaTeX-math-mode
            'turn-on-reftex
            'linum-mode))


(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; remove all tabs before saving
                  ;; TeX-engine 'xetex       ; use xelatex default
                  TeX-show-compilation t) ; display compilation windows
            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
            ;; (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
            ))


(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-view-program-selection '((output-pdf "Okular")
                                               (output-dvi "Okular")))))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It's not the end. It's just the end of beginning ...
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "It's not the end. It's just the end of beginning ...")
