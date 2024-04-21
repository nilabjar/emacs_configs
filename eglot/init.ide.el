
;; (use-package eglot
;;   :hook ((c++-mode java-mode python-mode) . eglot-ensure))


(use-package lsp-pyright
 :ensure t
 :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

;;C++ language settings
(setq c-default-style
     '((java-mode . "java") (other . "gnu")))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;Indentation setting for various languages.
(setq c-basic-offset 4)

(use-package lsp-mode
 :init
 ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
 (setq lsp-keymap-prefix "C-c l")
 :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
        (python-mode . lsp)
        (c++-mode . lsp)
        (java-mode . lsp)
        ;; if you want which-key integration
        (lsp-mode . lsp-enable-which-key-integration))
 :commands lsp)

;;optionally
(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
