(require 'package)

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(xterm-mouse-mode)
(setq mouse-wheel-scroll-amount '(3))
;; (setq use-package-always-defer t)
;; (load-theme 'wombat)

;; for fast emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode -1)
(blink-cursor-mode -1)
(setq redisplay-dont-pause t) ;; Optional, improves responsiveness

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
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (aw-dispatch-always t)
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "deep sky blue"
                      :weight 'bold
                      :height 400
                      :family "Fira Code")
  (set-face-attribute 'aw-background-face nil
                      :foreground "gray40"))

  ;; Optional: dim the other windows slightly
  ;; (set-face-attribute 'aw-background-face nil
  ;;                     :foreground "gray40"
  ;;                     :height 200))

;; (add-hook 'after-init-hook
;; 	  (lambda ()
;; 	    (load-theme 'wombat)))

(use-package doom-themes :ensure t)

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

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

;; (use-package evil-snipe
;;   :ensure t
;;   :config
;;   (setq evil-snipe-scope 'whole-buffer)
;;   :hook (prog-mode text-mode))

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
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      auto-revert-interval 1
      auto-revert-use-notify t)

(add-hook 'after-init-hook #'global-auto-revert-mode)

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

  ;; Normal mode bindings for quick avy jump
  (general-define-key
   :states 'normal
   :keymaps 'override
   "s" #'avy-goto-char-timer
   "S" #'avy-goto-line)

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

   "b"  '(:ignore b :which-key "buffer")
   "bo" '(evil-buffer :which-key "other buffer")
   "bb" '(helm-mini :which-key "other buffer")
   "bd" '(kill-buffer :which-key "kill buffer")
   "bf" '(delete-other-windows :which-key "Fullscreen buffer")
   "bc" '(company-complete :which-key "Invoke completion")
   "br" '(lsp-format-region :which-key "Lsp format region")

   "h" '(:ignore fh :which-key "hunks")
   "hn" '(diff-hl-next-hunk  :which-key "next hunk")

   "*" '(rg-menu  :which-key "project-wide grep")

   "g"  '(:ignore g :which-key "global")
   "gh" '(highlight-symbol-at-point :which-key "highlight point")
   "gu" '(unhighlight-regexp :which-key "unhighlight phrase")
   "gf" '(highlight-regexp '"[a-z].*(" :which-key "Highlight Functions")

   "j"  '(:ignore j :which-key "jump")
   "jc" '(avy-goto-char-timer :which-key "go to char")
   "jC" '(avy-goto-char-2     :which-key "go to 2-char")
   "jl" '(avy-goto-line       :which-key "go to line")
   "jw" '(avy-goto-word-1     :which-key "go to word")
   "js" '(avy-goto-subword-0  :which-key "go to subword")

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

;; ==========================================================================
;;                avy
;; ==========================================================================

(use-package avy
  :ensure t
  :config
  ;; Optional tweaks
  (setq avy-background t) ;; dim background while jumping
  (setq avy-style 'at-full) ;; visual style for overlays

  ;; Bind `s` in evil normal state to `avy-goto-char-timer`
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "s") #'avy-goto-char-timer)
    (define-key evil-normal-state-map (kbd "S") #'avy-goto-line)))

;; ==========================================================================

;; ==========================================================================
;;                Setup compilation buffer to show ansi colors
;; ==========================================================================

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)



;; ==========================================================================
;;                Fixing compilation path when using berg
;; ==========================================================================

(defvar my-bad-path-prefixes
  '("/workarea/src" "/workarea/temp" "/workarea/dev" "/tmp/tsoxomspub-1.305.0")
  "List of bad path prefixes to replace with the correct project root.")

(defun my-rewrite-paths-to-project-root ()
  "Rewrite incorrect paths in compile buffer to use actual Projectile project root."
  (let ((inhibit-read-only t)
        (project-root (directory-file-name (or (projectile-project-root) "")))) ;; remove trailing /
    (when (and project-root (not (string= project-root "")))
      (save-excursion
        (goto-char compilation-filter-start)
        (dolist (bad-prefix my-bad-path-prefixes)
          (let ((regexp (concat (regexp-quote bad-prefix) "/\\(.*?\\)\\(:[0-9]+\\)")))
            (while (re-search-forward regexp (point-max) t)
              (replace-match (format "%s/\\1\\2" project-root) nil nil))))))))

(add-hook 'compilation-filter-hook #'my-rewrite-paths-to-project-root)

;; ==========================================================================
;;                Reverting all buffers
;; ==========================================================================

(defun revert-all-no-confirm ()
  "Revert all file buffers, without confirmation.
Buffers visiting files that no longer exist are ignored.
Files that are not readable (including do not exist) are ignored.
Other errors while reverting a buffer are reported only as messages."
  (interactive)
  (let (file)
    (dolist (buf  (buffer-list))
      (setq file  (buffer-file-name buf))
      (when (and file  (file-readable-p file))
        (with-current-buffer buf
          (with-demoted-errors "Error: %S" (revert-buffer t t)))))))

;; ==========================================================================
;;                UI Beatification
;; ==========================================================================

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))

(set-face-attribute 'default nil
                    :font "Fira Code Retina"
                    :height 120)



;; ==========================================================================
;; ==========================================================================
;; ==========================================================================
;; ==========================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1" "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(package-selected-packages
   '(vscode-dark-plus-theme rg general git-link helm-xref dap-mode python-black pyvenv company which-key diff-hl lsp-pyright helm-lsp lsp-mode evil-surround evil-commentary evil deadgrep magit doom-themes ace-window sr-speedbar counsel-etags projectile ripgrep yasnippet flycheck hydra)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
