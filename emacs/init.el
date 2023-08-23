(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; Options
(setq inhibit-startup-message t)        ; Hide startup
(menu-bar-mode -1)                      ; Hide File menu
(global-display-line-numbers-mode)      ; Line numbers
(global-hl-line-mode t)                 ; Hightlight current line
(setq ring-bell-function 'ignore)	; silent bell when you make a mistake
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq make-backup-files nil)            ; stop creating ~ files
(set-face-attribute 'default nil :font "JetBrains Mono")

(require 'use-package)

(use-package general) ;; Keymap
(use-package evil :config (evil-mode 1)) ;; Vim keymapping
(use-package which-key :config (which-key-mode)) ;; Keymapp helper

(use-package helm) ;; ??
(use-package treemacs) ;; File tree
(use-package tree-sitter) ;; Colors
(use-package tree-sitter-langs) ;; Colors

;; LSP
(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; Clojure
(use-package cider)

(general-define-key
    :states '(normal visual insert emacs treemacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    "t" 'treemacs
    "/" '(counsel-rg :which-key "ripgrep")
    "SPC" '(counsel-M-x :which-key "M-x"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-treemacs helm-lsp lsp-ui lsp-mode which-key vertico stimmung-themes helm general evil-collection counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
