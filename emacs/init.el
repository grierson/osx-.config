;;; packages -- Summary
;;; Commentary:

;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 210
                    :weight 'normal
                    :width 'normal)


(require 'project)
(use-package treemacs) ; Filetree
(use-package magit) ; Git

;; Minibuf
(require 'package)
(use-package vertico :init (vertico-mode)) ;; Completion
(use-package savehist :init (savehist-mode)) ;; Save history
(use-package marginalia :init (marginalia-mode)) ;; Extra info for search items
(use-package orderless ;; search allow spaces
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(use-package consult ;; Live preview search
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Autocomplete
(use-package corfu :init (global-corfu-mode))
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; Lsp
(use-package lsp-mode :hook ((clojure-mode . lsp)
                             (lsp-mode . lsp-enable-which-key-integration)))
(use-package lsp-ui :hook (lsp-mode . lsp-ui-mode))
(use-package lsp-treemacs :after lsp)
(use-package flycheck :init (global-flycheck-mode))
(use-package clojure-mode)
(use-package cider)
(add-hook 'clojure-mode-hook #'lsp-deferred)

(use-package paredit
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  :config
  (show-paren-mode t))

;; Vi
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Keymapping

(use-package which-key :config (which-key-mode)) ; Key binding hints
(use-package general) ; Key binding hints

(general-create-definer g/leader :prefix "SPC")

(g/leader
  :keymaps 'normal
  :infix "s"
  "" '(:ignore t :wk "Search")
  "f" '(project-find-file :wk "Git File")
  "F" '(consult-find :wk "File")
  "g" '(consult-git-grep :wk "Git Grep")
  "G" '(consult-ripgrep :wk "Grep"))


(global-hl-line-mode t)
(setq display-line-numbers 'relative)

(provide 'init)
;;; init.el ends here
