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


(use-package which-key :config (which-key-mode)) ; Key binding hints
(use-package treemacs) ; Filetree
(use-package magit) ; Git

(use-package vertico :init (vertico-mode)) ;; Minibuf Completion
(use-package savehist :init (savehist-mode)) ;; Remember Minibuf search
(use-package marginalia :init (marginalia-mode)) ;; Extra info for Minibuf search results

(use-package orderless ;; Minibuf easier search
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult ;; Live preview search
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Autocomplete
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  :hook ((prog-mode . corfu-mode))
  :init (global-corfu-mode))

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

;; Vi
(use-package evil :config (evil-mode t))
(use-package evil-commentary :config (evil-commentary-mode))

(menu-bar-mode t)

(provide 'init)
;;; init.el ends here
