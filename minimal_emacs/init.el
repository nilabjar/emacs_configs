(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages
      '(
	company
	elpy
	evil
	)
      )

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))


;; ====================================
;; Development Setup
;; ====================================

;; setup company
(add-hook 'after-init-hook 'global-company-mode)

;; Enable elpy
(elpy-enable)
;;(setq elpy-rpc-virtualenv-path 'current)

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; General setup (Many used from emfy-emacs for you)

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
