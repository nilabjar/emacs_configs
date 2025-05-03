(require 'package)

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(xterm-mouse-mode)
(setq mouse-wheel-scroll-amount '(3))
;; (setq use-package-always-defer t)
(load-theme 'wombat)

(use-package hydra :ensure t)
(use-package flycheck :ensure t)
(use-package yasnippet :ensure t)

(use-package ripgrep :ensure t)

(use-package projectile :ensure t)

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

(use-package sr-speedbar
  :ensure t
  :config
  (setq sr-speedbar-auto-refresh nil)
  (setq sr-speedbar-right-side nil))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;; (add-hook 'after-init-hook
;; 	  (lambda ()
;; 	    (load-theme 'wombat)))

(use-package doom-themes :ensure t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package magit
  :ensure t

  :config
  (setq magit-disabled-section-inserters t)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  )


(use-package deadgrep :ensure t)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'deadgrep-mode 'emacs)
  (evil-set-initial-state 'speedbar-mode 'emacs)
  (evil-set-initial-state 'apropos-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  :hook (prog-mode text-mode))

(use-package evil-commentary
  :ensure t
  :hook (prog-mode text-mode))

(use-package evil-snipe
  :ensure t
  :config
  (setq evil-snipe-scope 'whole-buffer)
  :hook (prog-mode text-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))


;; (load-file "~/my_bigstore/alternate_home/emacs_configs/test_lsp/lsp_setup.el")

;; (add-hook 'python-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-keymap-prefix "C-c l")
  ;; Disable trying format while typing
  (setq lsp-enable-on-type-formatting nil)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)))

;; optionally
;; (use-package lsp-ui :ensure t :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :ensure t)


(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(setq gc-cons-threshold (* 100 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(use-package diff-hl
  :ensure t
  :init
  :config
  (global-diff-hl-mode)
  (diff-hl-dir-mode)
  (diff-hl-margin-mode)
  :hook (prog-mode text-mode))


(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay nil)
  :hook
  (after-init . global-company-mode)
  ;; (shell-mode . (lambda() (company-mode 0)))
  )


(use-package pyvenv
  :ensure t
  :config (pyvenv-mode 1)
  :hook (python-mode . pyvenv-mode))

(use-package python-black
  :ensure t
  :hook (python-mode . python-black-on-save-mode))

(use-package dap-mode
  :ensure t)

(require 'dap-python)
;; if you installed debugpy, you need to set this
;; https://github.com/emacs-lsp/dap-mode/issues/306
(setq dap-python-debugger 'debugpy)

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))
;; optionally if you want to use debugger
;; (use-package dap-mode
;;   :ensure t
;;   :init
;;   (setq dap-python-debugger 'debugpy)
;;   :commands dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details

;; (use-package avy :ensure t
;;   :config
;;   :commands (avy-goto-word-1)
;;   )

;; (load-file "~/my_bigstore/alternate_home/emacs_configs/test_lsp/helm.el")

(use-package helm
  :defer t
  :commands (helm-mini)
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-d" . helm-browse-project)
         ("M-y" . helm-show-kill-ring))
  :config  (helm-mode 1)
  (setq helm-M-x-fuzzy-match 1)
  (setq helm-buffers-fuzzy-matching 1)
  (setq helm-recentf-fuzzy-match 1)
  ;; Set buffer list column size to max length
  (setq helm-buffer-max-length nil)
  )

(use-package helm-xref
  :ensure t
  :after (helm))

(use-package git-link
  :ensure t
  :config
  '(progn
     (add-to-list 'git-link-remote-alist
                  '("bbgithub" git-link-github))
     (add-to-list 'git-link-commit-remote-alist
                  '("bbgithub\\.dev\\.bloomberg\\.com" git-link-commit-github)))
  )

;; ;
;;                                         ; (helm-mode)
;; ;; (require 'helm-xref)
;; ;; (define-key global-map [remap find-file] #'helm-find-files)
;; ;; (define-key global-map [remap execute-extended-command] #'helm-M-x)
;; ;; (define-key global-map [remap switch-to-buffer] #'helm-mini)


;; ====================================
;; Development Setup
;; ====================================

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

;; Indentation setting for various languages.
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

;; read shell history from bash_history
(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(defun my-shell-mode-hook ()
(setq comint-input-ring-file-name "~/.bash_history")  ;; or bash_history
(comint-read-input-ring t))

;; enable auto revert
(setq global-auto-revert-mode t)


;; ====================================
;; Change FONT size interactively
;; ====================================
(defun set-font-size ()
    "Set the font size."
  (interactive)
  (set-face-attribute
   'default nil :height
   (string-to-number
    (read-string "Font size: " (number-to-string (face-attribute 'default :height nil))))))

(use-package general :ensure t
  :demand t
  :config
  (general-define-key
   :states '(normal visual insert)
   :prefix "SPC"
   :non-normal-prefix "C-j"

   "p"  '(:ignore p :which-key "project")
   "pc" '(projectile-compile-project :which-key "project compile")
   "pt" '(counsel-etags-find-tag-at-point :which-key "find tags")
   "pf" '(projectile-find-file :which-key "find file in project")
   "p/" '(deadgrep :which-key "Deadgrep")

   "f"  '(:ignore f :which-key "file")
   "ft" '(counsel-etags-list-tag-in-current-file :which-key "list tags in file")
   "fe" '(sr-speedbar-toggle :which-key "speedbar-toggle")
   "fs" '(ff-get-other-file  :which-key "switch header/source")
   "fo" '(helm-find-files  :which-key "open file")
   "fr" '(recentf-open :which-key "recent files")

   "b"  '(:ignore f :which-key "buffer")
   "bo" '(evil-buffer :which-key "other buffer")
   "bb" '(helm-mini :which-key "other buffer")
   "bd" '(kill-buffer :which-key "kill buffer")
   "bf" '(delete-other-windows :which-key "Fullscreen buffer")
   "bc" '(company-complete :which-key "Invoke completion")

   "h" '(:ignore fh :which-key "hunks")
   "hn" '(diff-hl-next-hunk  :which-key "next hunk")

   "*" '(rg-menu  :which-key "project-wide grep")

   "g"  '(:ignore f :which-key "global")
   "gh" '(highlight-symbol-at-point :which-key "highlight point")
   "gu" '(unhighlight-regexp :which-key "unhighlight phrase")
   "gf" '(highlight-regexp '"[a-z].*(" :which-key "Highlight Functions")
   ))

;; ==========================================================================
;;                Ripgrep
;; ==========================================================================

;; (grep-apply-setting
;;    'grep-find-command
;;    '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)
;;  )

;; (setq grep-command "rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)")
;; (setq grep-command '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))

(setq grep-program "rg")

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

;; ==========================================================================

;; load breg lens
;; (load-file "~/my_bigstore/alternate_home/emacs_configs/lsp-emacs/breglens.el")



; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(custom-safe-themes
;    '("13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
;  '(nil nil t)
;  '(package-selected-packages
;    '(evil-surround counsel-etags rg ripgrep sr-speedbar git-link dape esup general lsp-pyright lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode evil magit deadgrep doom-themes pyvenv python-black)))
; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(package-selected-packages '(ripgrep yasnippet flycheck hydra)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
