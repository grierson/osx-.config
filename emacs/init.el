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

(use-package which-key) ; Key binding hints

;; Autocomplete + LSP
(use-package corfu
    :custom
    (setq corfu-cycle t
	corfu-auto t
	corfu-auto-delay 0.2
	corfu-quit-at-boundary t))

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package lsp-mode
  :config (lsp-enable-which-key-integration t)
  :custom (lsp-completion-provider :none)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(use-package lsp-ui :hook (lsp-mode . lsp-ui-mode))
(use-package lsp-treemacs :after lsp)
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


