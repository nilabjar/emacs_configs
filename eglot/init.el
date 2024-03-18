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
  (after-init . global-company-mode))

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

(load-theme 'wombat t)

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

(setq lsp-pyright-multi-root nil)

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


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
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
  :hook
  (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode t)
  (diff-hl-margin-mode t)
)
;;==================================================

;;========== deadgrep ==============================
(use-package deadgrep)
(global-set-key (kbd "<f5>") #'deadgrep)
;;==================================================

(setq completion-at-point-functions '(elisp-completion-at-point comint-dynamic-complete-filename t))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages '(consult use-package eglot company which-key vertico evil))
 '(python-shell-interpreter "python3.11"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
