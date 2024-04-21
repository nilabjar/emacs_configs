;; (require 'package)

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages
      '(
;;        lsp-mode
;;        yasnippet
;;        lsp-treemacs
;;        helm-lsp
;;        projectile
;;        hydra
;;        flycheck
;;        avy
;;        helm-xref
;;        dap-mode
;;        deadgrep
;;        pyvenv
;;        python-black
	)
)


(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(setq use-package-compute-statistics t)

(xterm-mouse-mode)
(setq mouse-wheel-scroll-amount '(3))
(setq use-package-always-defer t)
(load-theme 'wombat)

(use-package hydra :ensure t)
(use-package flycheck :ensure t)
(use-package yasnippet :ensure t)

(use-package projectile
  :ensure t
  :commands (projectile-mode))

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

(use-package doom-themes
  :defer t
  :config
  :commands (load-theme))

(use-package evil
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

(use-package magit
  :ensure t
  :commands (magit-status))

(use-package diff-hl
  :ensure t
  :init
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode)
  (diff-hl-margin-mode)
  :commands (diff-hl-mode)
  :hook (prog-mode text-mode))

(use-package deadgrep
  :ensure t
  :commands (deadgrep))

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(use-package company
  :hook
  (after-init . global-company-mode)
  (shell-mode . (lambda() (company-mode 0)))
  )

(use-package lsp-mode
  :config
  (setq lsp-keymap-prefix "C-c l")
  ;; Disable trying format while typing
  (setq lsp-enable-on-type-formatting nil)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
;; (use-package lsp-ui :ensure t :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package pyvenv
  :ensure t
  :config (pyvenv-mode 1)
  :hook (python-mode . pyvenv-mode))

(use-package python-black
  :ensure t
  :hook (python-mode . python-black-on-save-mode))


;; optionally if you want to use debugger
(use-package dap-mode :commands dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))


;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details

;; (use-package avy :ensure t
;;   :config
;;   :commands (avy-goto-word-1)
;;   )

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

;; (eval-after-load 'git-link
;;  '(progn
;;    (add-to-list 'git-link-remote-alist
;;      '("bbgithub" git-link-github))
;;    (add-to-list 'git-link-commit-remote-alist
;;      '("bbgithub\\.dev\\.bloomberg\\.com" git-link-commit-github))))



;; ;
;;                                         ; (helm-mode)
;; ;; (require 'helm-xref)
;; ;; (define-key global-map [remap find-file] #'helm-find-files)
;; ;; (define-key global-map [remap execute-extended-command] #'helm-M-x)
;; ;; (define-key global-map [remap switch-to-buffer] #'helm-mini)

(setq gc-cons-threshold (* 100 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   (require 'dap-cpptools)
;;   (require 'dap-python)
;;   (setq dap-auto-configure-features '(sessions locals controls tooltip))
;;   ;; if you installed debugpy, you need to set this
;;   ;; https://github.com/emacs-lsp/dap-mode/issues/306
;;   (setq dap-python-debugger 'debugpy)
;;   (yas-global-mode))

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
   :non-normal-prefix "C-f"

   "p"  '(:ignore p :which-key "project")
   "pc" '(projectile-compile-project :which-key "project compile")
   "pt" '(helm-etags-select :which-key "find tags")
   "pf" '(projectile-find-file :which-key "find file in project")

   "f"  '(:ignore f :which-key "file")
   "ft" '(list-tags :which-key "list tags in file")
   "fe" '(sr-speedbar-toggle :which-key "speedbar-toggle")
   "fs" '(ff-get-other-file  :which-key "switch header/source")
   "fo" '(helm-find-files  :which-key "open file")
   "fe" '(sr-speedbar-toggle :which-key "speedbar")

   "b"  '(:ignore f :which-key "buffer")
   "bo" '(evil-buffer :which-key "other buffer")
   "bb" '(helm-mini :which-key "other buffer")
   "bd" '(kill-buffer :which-key "kill buffer")
   "bf" '(delete-other-windows :which-key "Fullscreen buffer")

   "h" '(:ignore fh :which-key "hunks")
   "hn" '(diff-hl-next-hunk  :which-key "next hunk")

   "*" '(projectile-grep  :which-key "project-wide grep")

   "g"  '(:ignore f :which-key "global")
   "gh" '(highlight-symbol-at-point :which-key "highlight point")
   "gu" '(unhighlight-regexp :which-key "unhighlight phrase")
   ))

(use-package dape
  ;; :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; :hook
  ;; Save breakpoints on quit
  ;; (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;; (after-init . dape-breakpoint-load)

  ;; :config
  ;; Turn on global bindings for setting breakpoints with mouse
  ;; (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  ;; (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; ;; Projectile users
  ;; (setq dape-cwd-function 'projectile-project-root)
  )

;; Enable repeat mode for more ergonomic `dape' use
;; (use-package repeat
;;   :config
;;   (repeat-mode))

;;========== Space leader keys ========================
;(use-package general
;  :config
;  (general-evil-setup t)
;
;  (general-create-definer rune/leader-keys
;    :keymaps '(normal insert visual emacs)
;    :prefix "SPC"
;    :global-prefix "M-SPC"))
;
;(rune/leader-keys
;  "t"  '(:ignore t :which-key "toggles")
;  "tt" '(counsel-load-theme :which-key "choose theme")
;
;  "p"  '(:ignore p :which-key "project")
;  "pc" '(projectile-compile-project :which-key "project compile")
;  "pt" '(counsel-etags-find-tag-at-point :which-key "find tags")
;  "pf" '(projectile-find-file :which-key "find file in project")
;
;  "f"  '(:ignore f :which-key "file")
;  "ft" '(counsel-etags-list-tag-in-current-file :which-key "list tags in file")
;  "fe" '(sr-speedbar-toggle :which-key "speedbar-toggle")
;  "fo" '(ff-get-other-file  :which-key "switch header/source")
;
;  "g"  '(:ignore f :which-key "global")
;  "gh" '(highlight-symbol-at-point :which-key "highlight point")
;  "gu" '(unhighlight-regexp :which-key "unhighlight phrase")
;
;  "*" '(consult-ripgrep :which-key "search project")
;
;  "j"  '(:ignore f :which-key "jump")
;  "jj" '(ace-jump-char-mode :which-key "Jump to Char")
;  "jw" '(ace-jump-word-mode :which-key "Jump to Word")
;  "jl" '(ace-jump-line-mode :which-key "Jump to Line")
;
;  "b"  '(:ignore f :which-key "buffer")
;  "bb" '(consult-buffer :which-key "Change buffer")
;)
;;==================================================



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(nil nil t)
 '(package-selected-packages
   '(sr-speedbar git-link dape esup general lsp-pyright lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode evil magit deadgrep doom-themes pyvenv python-black)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
