;;; init.el --- Description -*- lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages
      '(
        ;; insert packages here
	    ))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))


(setq package-install-upgrade-built-in t)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(xterm-mouse-mode)

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t)
  (evil-mode))

(use-package magit)

(use-package which-key
  :init
  (which-key-mode))

(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
)

;; (use-package eglot
;;   :hook ((c++-mode python-mode) . eglot-ensure))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (
        ("C-x b" . consult-buffer)  ;; orig. switch-to-buffer
  )
)

(recentf-mode)

(global-auto-revert-mode t)

;;C++ language settings
(setq c-default-style
      '((java-mode . "java") (other . "gnu")))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Indentation setting for various languages.
(setq c-basic-offset 4)


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

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package counsel-etags
  :ensure t
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package pyvenv
  :ensure t
  :init
  (pyvenv-mode))

;; The following was added so that behave tests run nicely in the
;; compile buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-clients-clangd-args '("--completion-style=detailed"))
  :commands lsp)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package git-link)

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(eval-after-load 'git-link
 '(progn
   (add-to-list 'git-link-remote-alist
     '("bbgithub" git-link-github))
   (add-to-list 'git-link-commit-remote-alist
     '("bbgithub\\.dev\\.bloomberg\\.com" git-link-commit-github))))

;;========== Space leader keys ========================
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(rune/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")

  "p"  '(:ignore p :which-key "project")
  "pc" '(projectile-compile-project :which-key "project compile")
  "pt" '(counsel-etags-find-tag-at-point :which-key "find tags")
  "pf" '(projectile-find-file :which-key "find file in project")

  "f"  '(:ignore f :which-key "file")
  "ft" '(counsel-etags-list-tag-in-current-file :which-key "list tags in file")

  "g"  '(:ignore f :which-key "global")
  "gh" '(highlight-symbol-at-point :which-key "highlight point")
  "gu" '(unhighlight-regexp :which-key "unhighlight phrase")

  "*" '(consult-ripgrep :which-key "search project")
)
;;==================================================

;;========== Opengrok ==============================
(use-package eopengrok)
(require 'eopengrok)
;;==================================================

;;========== diff-hl ==============================
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode t)
  (diff-hl-margin-mode t)
  :hook
  (dired-mode . diff-hl-dired-mode)
)
;;==================================================

;;========== deadgrep ==============================
(use-package deadgrep)
(global-set-key (kbd "<f5>") #'deadgrep)
;;==================================================

;;========== context menu ==============================
(add-hook 'text-mode-hook 'context-menu-mode)
(add-hook 'prog-mode-hook 'context-menu-mode)
(add-hook 'shell-mode-hook 'context-menu-mode)
(add-hook 'dired-mode-hook 'context-menu-mode)


(defun cc/context-menu-addons (menu click)
  "CC context menu additions"
  (save-excursion
    (mouse-set-point click)
    (define-key-after menu [find-tags]
      '(menu-item "Find Tags" counsel-etags-find-tag-at-point
                  :help "Find tags at point"))

    (define-key-after menu [highlight-word]
      '(menu-item "Highlight Word" highlight-symbol-at-point
                  :help "Highlight word"))

    (define-key-after menu [tags-in-file]
      '(menu-item "Tags In File" counsel-etags-list-tag-in-current-file
                  :help "Tags In File"))

    (define-key-after menu [un-highlight-word]
      '(menu-item "Un-Highlight" unhighlight-regexp
                  :help "Un-Highlight"))

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

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "<mouse-3>") nil))

(global-set-key (kbd "S-<down-mouse-3>") #'lsp-mouse-click)

;;==================================================
;; Show the current function name in the header line
;;==================================================

(which-function-mode)

;;==================================================
;; Enable SR-SPEEDBAR
;;==================================================

(use-package sr-speedbar
  :ensure t)

;;==================================================


(setq completion-at-point-functions '(elisp-completion-at-point comint-dynamic-complete-filename t))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" default))
 '(global-display-line-numbers-mode t)
 '(package-selected-packages '(consult use-package eglot company which-key vertico evil))
 '(python-shell-interpreter "python3.11"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
