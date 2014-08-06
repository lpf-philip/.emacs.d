;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; X management
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'usepackage)

;; frame buffer - buffer or file name
(when window-system
  (setq frame-title-format              ;frame title format
      '("" invocation-name " xxx - " (:eval (if (buffer-file-name)
						   (abbreviate-file-name (buffer-file-name))
						 "%b"))))
  (menu-bar-mode -1)                    ;menubar
  (tooltip-mode -1)                     ;tooltip
  (tool-bar-mode -1)                    ;tool bar
  (scroll-bar-mode -1)                  ;scroll bar
  (blink-cursor-mode -1))               ;curser blink mode

;; add var in .bashrc
;; export ALTERNATE_EDITOR=""
;; export EDITOR=emacsclient
;; for emacs daemon start without frame, set for "emacsclient -c"
(setq window-system-default-frame-alist
      '(
        ;; if frame created on x display
        (x
	 (menu-bar-lines . nil)
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

(if (fboundp 'fringe-mode)
    (fringe-mode 4))			;fring width, default 8
(setq inhibit-startup-screen t)         ;disable start page
(setq scroll-margin 3                   ;left 3 lines when page scroll
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(setq visible-bell t)			;no audio but flash
(setq-default indicate-empty-lines t)	;indicate the empty lines
(setq-default show-trailing-whitespace t) ;show tailing whitespace
(setq line-number-mode t)                 ;line number in modeline
(setq column-number-mode t)               ;column number in modeline
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; (global-visual-line-mode 1) ; 1 for on, 0 for off.

;; winner-->C-c <left> C-c <right>
;; to switch between window configurations. This is handy when something
;; has popped up a buffer that you want to look at briefly before
;; returning to whatever you were working on
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

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer) ;vertical split
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer) ;horizontal split


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


(provide 'setup-window)
