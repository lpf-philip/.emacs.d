;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;; fly in emacs ;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add var in .bashrc or .profile
;; export ALTERNATE_EDITOR=""
;; export EDITOR=emacsclient
;; for emacs daemon start without frame, set for "emacsclient -c"


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package management
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
(add-to-list 'package-archives '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
;; (package-initialize)

;; informs Emacs about the latest versions of all packages
(unless package-archive-contents
  (package-refresh-contents))

(defun my/byte-recompile ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d/site-lisp" 0))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; frame buffer - buffer or file name
(setq frame-title-format
      '("" invocation-name " xxx - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; disable startup screen
(setq inhibit-startup-screen t)

;; menu toolbar mouse
(setq window-system-default-frame-alist
      '(
        (x ; if frame created on x display
         (menu-bar-lines . 1)
         (menu-bar-mode . 1)
         (tooltip-mode . -1)
         (tool-bar-mode . -1)
         (scroll-bar-mode . -1)
         (blink-cursor-mode . -1)
         (mouse-wheel-mode . 1)
         (mouse-avoidance-mode . 'exile)
         )
        (nil ; if on terminal
         (menu-bar-lines . 0)
         (tool-bar-lines . 0)
         )))

;; scroll
(setq scroll-margin 1
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(if (fboundp 'fringe-mode)
    (fringe-mode 4))			;default 8

;; no audio but flash
(setq visible-bell t)

;; move mouse away when curser in
(mouse-avoidance-mode 'animate)

;; show empty lines & whitespace
(setq-default indicate-empty-lines t)	;indicate the empty lines
(setq-default show-trailing-whitespace t) ;show tailing whitespace

(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; (global-visual-line-mode 1) ; 1 for on, 0 for off.

;; replace yes/no with y/n
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "RET") 'newline-and-indent)

(show-paren-mode t)
(icomplete-mode 1)

(delete-selection-mode t) ;; delete the selection with a keypress
(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 4) ;; but maintain correct appearance
(setq dired-recursive-copies (quote always))

;; ------------------------------------------------------------
;; init.el file
;; ------------------------------------------------------------
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(define-key emacs-lisp-mode-map (kbd "M-s e b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "M-s e r") 'eval-region)

;; ------------------------------------------------------------
;; uniquify-buffer-name-style
;; ------------------------------------------------------------
(setq uniquify-buffer-name-style (quote post-forward))
;; (setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; ------------------------------------------------------------
;; goto previous mark position
;; ------------------------------------------------------------
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

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
;; line & column number
;; ------------------------------------------------------------
(setq line-number-mode t)
(setq column-number-mode t)
(global-linum-mode t)
;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
(setq linum-mode-inhibit-modes-list
      '(eshell-mode shell-mode erc-mode help-mode jabber-roster-mode
                    jabber-chat-mode twittering-mode compilation-mode
                    weibo-timeline-mode woman-mode Info-mode calc-mode
                    calc-trail-mode comint-mode gnus-group-mode inf-ruby-mode
                    gud-mode org-mode vc-git-log-edit-mode log-edit-mode
                    term-mode w3m-mode speedbar-mode gnus-summary-mode
                    gnus-article-mode calendar-mode))
(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
  (unless (member major-mode linum-mode-inhibit-modes-list)
    ad-do-it))
(ad-activate 'linum-on)

;; ------------------------------------------------------------
;; backups
;; ------------------------------------------------------------
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; ------------------------------------------------------------
;; history
;; ------------------------------------------------------------
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length 1024)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; ------------------------------------------------------------
;; refresh file
;; ------------------------------------------------------------
(defun refresh-file ()
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))
(global-set-key (kbd "C-c g") 'refresh-file)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make sure the (custom-set-variables ...) sexp is at the very
;; top of your .emacs file. That is the right place for it.
;; for smart-mode-line(https://github.com/Malabarba/smart-mode-line)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(package-selected-packages
   '(helm-c-moccur magit yasnippet window-number undo-tree solarized-theme smex smartscan smartparens smart-mode-line rainbow-delimiters projectile openwith maxframe highlight-symbol helm-swoop guide-key ggtags diminish browse-kill-ring)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ------------------------------------------------------------
;; theme: solarized-theme
;; ------------------------------------------------------------
;; https://ethanschoonover.com/solarized/
(require 'solarized-theme)
;; (setq solarized-distinct-fringe-background t)
;; (setq solarized-high-contrast-mode-line t)
;; (setq solarized-use-less-bold t)
;; (setq solarized-use-more-italic t)
;; (setq solarized-emphasize-indicators nil)
;; (setq x-underline-at-descent-line t)
(load-theme 'solarized-dark t)

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
;; guide-key - keyboard shortcuts tips.
;; ------------------------------------------------------------
(require 'guide-key)
(setq guide-key/guide-key-sequence
      '("C-x" "C-x r" "C-x 4" "C-x c"
        "C-c" "C-c p" "C-c g"
        "M-s" "M-s w"
        "M-g"
        "C-h"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'right)

;; ------------------------------------------------------------
;; maxframe
;; ------------------------------------------------------------
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; ------------------------------------------------------------
;; move among windows
;; ------------------------------------------------------------
(require 'windmove)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)
(global-set-key (kbd "C-x C-<left>") 'windmove-left)
(global-set-key (kbd "C-x C-<up>") 'windmove-up)
(global-set-key (kbd "C-x C-<down>") 'windmove-down)

;; ------------------------------------------------------------
;; move among windows with numbers. C-x C-j [n]
;; ------------------------------------------------------------
(require 'window-number)
(window-number-mode 1)

;; ------------------------------------------------------------
;; optmize window/buffer split
;; ------------------------------------------------------------
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

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)
;; M-s w[indow]
(global-set-key (kbd "M-s w") nil) ;; Remove the old keybinding
(global-set-key (kbd "M-s w x") 'swap-windows)
(global-set-key (kbd "M-s w s") 'change-split)

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
(diminish 'wrap-region-mode)
(diminish 'yas/minor-mode)
(diminish 'abbrev-mode "Abv")
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "projectile-mode" '(diminish 'projectile-mode))

;; ------------------------------------------------------------
;; Smart M-x is smart
;; ------------------------------------------------------------
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

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
;; auto insert pairs
;; ------------------------------------------------------------
(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)

;; ------------------------------------------------------------
;; rainbow - colorful
;; ------------------------------------------------------------
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; ------------------------------------------------------------
;; highlight
;; ------------------------------------------------------------
(require 'highlight-symbol)
(global-set-key (kbd "C-c l l") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c l n") 'highlight-symbol-next)
(global-set-key (kbd "C-c l p") 'highlight-symbol-prev)
(global-set-key (kbd "C-c l r") 'highlight-symbol-query-replace)

;; ------------------------------------------------------------
;; smartscan - Searching based on the current word
;; ------------------------------------------------------------
(require 'smartscan)
(global-smartscan-mode 1)

;; ------------------------------------------------------------
;; yasnippet for make fast inser
;; ------------------------------------------------------------
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt
                             yas-completing-prompt))

;; ------------------------------------------------------------
;; projectile
;; ------------------------------------------------------------
(require 'projectile)
(projectile-global-mode)

;; ;; ------------------------------------------------------------
;; ;; gtags
;; ;; ------------------------------------------------------------
;; (autoload 'gtags-mode "gtags" "" t)
;; (add-hook 'gtags-mode-hook
;;           '(lambda () ; Local customization (overwrite key mapping)
;;              ;; (define-key gtags-mode-map "\C-f" 'scroll-up)
;;              ;; (define-key gtags-mode-map "\C-b" 'scroll-down)
;;              ))
;; (add-hook 'gtags-select-mode-hook
;;           '(lambda ()
;;              (setq hl-line-face 'underline)
;;              (hl-line-mode 1)
;;              ))
;; (add-hook 'c-mode-hook '(lambda () (gtags-mode t)))
;; (add-hook 'c++-mode-hook '(lambda () (gtags-mode t)))
;; (add-hook 'java-mode-hook '(lambda () (gtags-mode t)))
;; (add-hook 'asm-mode-hook '(lambda () (gtags-mode t)))
;; (add-hook 'makefile-mode-hook '(lambda () (gtags-mode t)))
;; (setq gtags-suggested-key-mapping t)    ;gtags key mapping
;; (setq gtags-auto-update t)

;; ------------------------------------------------------------
;; ggtags
;; ------------------------------------------------------------
(require 'ggtags)
(add-hook 'prog-mode-hook 'ggtags-mode)

;; ------------------------------------------------------------
;; doxygen
;; ------------------------------------------------------------
(autoload 'doxygen-insert-function-comment "doxygen" "insert comment for the function at point" t)
(autoload 'doxygen-insert-file-comment "doxygen" "insert comment for file" t)
(autoload 'doxygen-insert-member-group-region "doxygen" "insert comment for member group" t)
(autoload 'doxygen-insert-compound-comment "doxygen" "insert comment for compound" t)

;; ------------------------------------------------------------
;; helm mode
;; ------------------------------------------------------------
(require 'helm)
(require 'helm-config)
(setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
      helm-input-idle-delay 0.01  ; this actually updates things
      helm-quick-update t ; relatively quickly.
      helm-M-x-requires-pattern nil
      helm-ff-skip-boring-files t)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      ;helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

;; (setq helm-autoresize-max-height 0)
;; (setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)

(helm-mode 1)
;; I don't like the way switch-to-buffer uses history, since
;; that confuses me when it comes to buffers I've already
;; killed. Let's use ido instead.
(add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . ido))
;; (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

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
;; (require 'ido-ubiquitous)

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
(ido-mode -1)
;; ------------------------------------------------------------
;; magit
;; ------------------------------------------------------------
;;  https://github.com/magit/magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; ------------------------------------------------------------
;; dictionary -- stardict
;; ------------------------------------------------------------
(require 'sdcv-mode)
(global-set-key (kbd "M-?") 'sdcv-search)

;; ------------------------------------------------------------
;; unicad - for languages environment
;; ------------------------------------------------------------
(require 'unicad)

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
;; occur search
;; ------------------------------------------------------------
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
;; Don't show whitespace in diff, but show context
(setq vc-diff-switches '("-b" "-B" "-u"))
(setq cc-search-directories '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include"))
(setq c-basic-offset 4)
(setq c-style-variables-are-local-p nil)
;; give me NO newline automatically after electric expressions are entered
(setq c-auto-newline nil)
(c-set-offset 'substatement-open 0)
;;first arg of arglist to functions: tabbed in once
(c-set-offset 'arglist-intro '+)
;;second line of arglist to functions: tabbed in once
;;(default was c-lineup-arglist)
(c-set-offset 'arglist-cont-nonempty '+)
;;switch/case: make each case line indent from switch
(c-set-offset 'case-label '+)
;;make the ENTER key indent next line properly
(local-set-key "\C-m" 'newline-and-indent)
;;syntax-highlight aggressively
;;(setq font-lock-support-mode 'lazy-lock-mode)
(setq lazy-lock-defer-contextually t)
(setq lazy-lock-defer-time 0)
;;make open-braces after a case: statement indent to 0 (default was '+)
(c-set-offset 'statement-case-open 0)
;;make a #define be left-aligned
(setq c-electric-pound-behavior (quote (alignleft)))
(which-function-mode 1)
(global-set-key (kbd "C-x C-o") 'ffap)
(global-set-key (kbd "M-m") 'helm-imenu)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It's not the end. It's just the end of beginning ...
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "It's not the end. It's just the end of beginning ...")
