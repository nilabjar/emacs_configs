(use-package evil :init
  (setq evil-disable-insert-state-bindings t) (evil-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package magit)

(use-package which-key :init (which-key-mode))

(use-package company :hook (after-init . global-company-mode))

(use-package flycheck :ensure t :init (global-flycheck-mode))

(use-package vertico :init (vertico-mode))

(use-package savehist :init (savehist-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (
        ("C-x b" . consult-buffer)  ;; orig. switch-to-buffer
  )
)

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-fallback-style "llvm"))

(setq clang-format-fallback-style "llvm")

;;(load "/usr/share/emacs/site-lisp/clang-format-16/clang-format.el")

(recentf-mode)

;;(load-theme 'wombat t)

(global-auto-revert-mode)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

;; Line Number Mode
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                dired-mode-hook
                compilation-mode-hook))
  
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package evil-owl
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))
;; (use-package evil-owl
;;   :config
;;   (setq evil-owl-display-method 'posframe
;;         evil-owl-extra-posframe-args '(:width 50 :height 20)
;;         evil-owl-max-string-length 50)
;;   (evil-owl-mode))

(global-set-key (kbd "S-<down-mouse-3>") #'lsp-mouse-click)


(add-hook 'text-mode-hook 'context-menu-mode)
(add-hook 'prog-mode-hook 'context-menu-mode)
(add-hook 'shell-mode-hook 'context-menu-mode)
(add-hook 'dired-mode-hook 'context-menu-mode)


;; Transform Text
(defvar cc/transform-text-menu (make-sparse-keymap "Transform Text"))

(define-key cc/transform-text-menu [tranform-text-uppercase]
  '(menu-item "Make Upper Case" upcase-region
              :help "Upper case region"))

(define-key-after cc/transform-text-menu [tranform-text-lowercase]
  '(menu-item "Make Lower Case" downcase-region
              :help "Lower case region"))

(define-key-after cc/transform-text-menu [tranform-text-capitalize]
  '(menu-item "Capitalize" capitalize-region
              :help "Capitalize region"))


(defun cc/context-menu-addons (menu click)
  "CC context menu additions"
  (save-excursion
    (mouse-set-point click)
    (define-key-after menu [open-in-finder]
      '(menu-item "Open in Finder" reveal-in-folder-this-buffer
                  :help "Open file (buffer) in Finder"))

    (when (region-active-p)
      (define-key-after menu [osx-dictionary-lookup]
        '(menu-item "Look up" osx-dictionary-search-word-at-point
                    :help "Look up in dictionary"))

      (define-key-after menu [occur-word-at-mouse]
        '(menu-item "Occur" occur-word-at-mouse
                    :help "Occur")))

    (when (and (bound-and-true-p buffer-file-name)
               (vc-registered (buffer-file-name)))
      (define-key-after menu [vc-separator]
        '(menu-item "--single-line"))

      (define-key-after menu [magit-status]
        '(menu-item "Magit Status" magit-status
                    :help "Magit Status"))
      (define-key-after menu [ediff-revision]
        '(menu-item "Ediff revision…" cc/ediff-revision
                    :help "Ediff this file with revision")))

    (when (region-active-p)
      (define-key-after menu [transform-text-separator]
        '(menu-item "--single-line"))
      (define-key-after menu [tranform-text]
        (list 'menu-item "Transform" cc/transform-text-menu)))

    (when (and (derived-mode-p 'org-mode) (region-active-p))
      (define-key-after menu [org-emphasize]
        (list 'menu-item "Org Emphasize" cc/org-emphasize-menu))

      (define-key-after menu [org-export-to-slack]
        '(menu-item "Copy as Slack" org-slack-export-to-clipboard-as-slack
                    :help "Copy as Slack to clipboard"))

      (define-key-after menu [copy-as-rtf]
        '(menu-item "Copy as RTF" dm/copy-as-rtf
                    :help "Copy as RTF to clipboard")))

    (when (region-active-p)
      (define-key-after menu [google-search]
        '(menu-item "Search with Google" google-this-noconfirm
                    :help "Search Google with region"))))
    menu)

;; hook into context menu
(add-hook 'context-menu-functions #'cc/context-menu-addons)
