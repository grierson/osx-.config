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

(use-package which-key :config (which-key-mode)) ; Key binding hints
(use-package treemacs) ; Filetree

;; Finder ??
(use-package counsel :after ivy :config (counsel-mode)) ; ?
(use-package ivy :config (ivy-mode)) ; ?
(use-package all-the-icons-ivy :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;; Autocomplete
(use-package company :config (global-company-mode t))
;; Lsp
(use-package lsp-mode :hook ((clojure-mode . lsp)
                             (lsp-mode . lsp-enable-which-key-integration)))
(use-package lsp-ui :hook (lsp-mode . lsp-ui-mode))
(use-package lsp-treemacs :after lsp)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; Linting
(use-package flycheck :init (global-flycheck-mode))

;; Clojure LSP
(use-package clojure-mode)
(use-package cider)
(add-hook 'clojure-mode-hook #'lsp-deferred)

;; Vi
(use-package evil :config (evil-mode t))
(use-package evil-commentary :config (evil-commentary-mode))
(provide 'init)
;;; init.el ends here


