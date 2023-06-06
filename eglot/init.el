;;; init.el --- Description -*- lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages
      '(
	    company
	    which-key
        magit
        evil
	    ))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package eglot
  :hook ((c++-mode python-mode) . eglot-ensure))

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(recentf-mode)

(load-theme 'wombat t)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)

(which-key-mode)


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

;; Enable Evil
(setq evil-disable-insert-state-bindings t)
;;(setq evil-default-state 'emacs)
(require 'evil)
(evil-mode)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package eglot company which-key magit vertico evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )