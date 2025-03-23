(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; BASIC SETUP --------------------------------

(setq use-package-always-defer t)

(use-package recentf
  :init
  (setq
    recentf-save-file "~/.cache/emacs/recentf"
    recentf-max-saved-items 10000
    recentf-max-menu-items 5000
    )
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  )

(use-package fido-vertical-mode
  :init
  (fido-vertical-mode 1))

(load-theme 'doom-challenger-deep t)
;; --------------------------------------------

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-challenger-deep t))

(use-package display-line-numbers
  :ensure t
  :init
  (display-line-numbers-mode 1)
  :config
  (setq display-line-numbers-relative t))


(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))


(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'speedbar-mode 'emacs)
  :hook (prog-mode text-mode)
  :commands (evil-emacs-state))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode)
  :hook (prog-mode text-mode))

(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-mode)
  (setq evil-snipe-scope 'whole-buffer)
  :hook (prog-mode text-mode))

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  (shell-mode . (lambda() (company-mode 0)))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
