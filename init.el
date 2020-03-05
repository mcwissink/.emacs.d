;;; init.el --- Mark Wissink

;;; Commentary:
;;; a block of text so stops complaining

;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
(package-initialize)

;; Fix for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package tramp)

(use-package org
  :custom
  (org-startup-truncated nil)
  :mode
  ("\\.org$" . org-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package company
  :config
  (global-company-mode 1))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package evil
  :config
  (evil-mode 1))

(use-package flycheck-inline
  :config
  (with-eval-after-load 'flycheck (global-flycheck-inline-mode)))

(use-package flyspell
  :hook
  ((org-mode text-mode) . flyspell-mode))

(use-package smex)

(use-package ivy
  :config
  (ivy-mode 1))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file)))

(use-package lsp-mode
  :hook
  ((prog-major-mode . lsp-prog-major-mode-enable)
    (lsp-after-open-hook . lsp-enable-imenu)
    (prog-mode . lsp))
  :config
  (setq lsp-prefer-flymake nil
    lsp-inhibit-message nil
    lsp-highlight-symbol nil
    lsp-enable-snippet nil))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil
    lsp-ui-peek-enable nil
    lsp-ui-sideline-enable nil
    lsp-ui-imenu-enable nil
    lsp-ui-flycheck-enable t)
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package projectile
  :config
  (projectile-mode 1))

;; TypeScript configuration
(use-package tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode +1))

(use-package web-mode
  :config
  (with-eval-after-load 'tide
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-hook 'web-mode-hook
      (lambda ()
        (when (string-equal "tsx" (file-name-extension buffer-file-name))
          (setup-tide-mode))))
    ;; enable typescript-tslint checker
    (flycheck-add-mode 'typescript-tslint 'web-mode)))

;; C++ configuration
(defun setup-c++-mode ()
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inclass '++)
  (c-set-offset 'access-label '-)
  (setq flycheck-clang-include-path
    (list
      (expand-file-name "include"
        (projectile-project-root)))))

(add-hook 'c++-mode-hook 'setup-c++-mode)

(when (string-equal system-type "windows-nt")
  (let* (
          (mypaths
            '(
               "C:/msys64/usr/bin"
               "C:/msys64/mingw64/bin"
               )))

    (setenv "PATH" (mapconcat 'identity (cons (getenv "PATH") mypaths) ";"))
    (setq exec-path (append exec-path mypaths))))

(use-package apropospriate-theme
  :ensure t
  :config
  (load-theme 'apropospriate-dark t))

(set-face-attribute 'default nil :height 90)
(prefer-coding-system 'utf-8)
(setq initial-scratch-message nil)
;; Disable the awful bell in windows
(setq ring-bell-function 'ignore)

;; Remove unnecessary things
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(provide 'init)
;;; init.el ends here
