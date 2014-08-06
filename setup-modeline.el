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


(provide 'setup-modeline)
