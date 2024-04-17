;;; init.el --- Mark Wissink

;;; Commentary:

;;; Code:

;;; random fixes
;; Fix for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (setq ghub-use-workaround-for-emacs-bug 'force)

;; remove bindings
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "s-t"))
(global-unset-key (kbd "s-p"))

(defun ensure-file (path path-callback)
  (unless (file-exists-p path)
    (with-temp-buffer (write-file path)))
  (funcall path-callback path))

;;; bootstrap custom file
(ensure-file
 "~/.emacs.d/custom.el"
 (lambda (path)
   (setq custom-file path)
   (load custom-file)))

;;; bootstrap device file
;; useful for any device specific configurations
(ensure-file "~/.emacs.d/device.el" 'load)

;;; bootstrap use-package
(require 'package)
(package-initialize)

(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package use-package-ensure-system-package)

;;; coding
;; lsp-mode is required until eglot supports multiple language servers for a single mode
(use-package lsp-mode
  :bind
  ("C-c C-a" . lsp-execute-code-action)
  ("C-c C-r" . lsp-rename)
  :commands
  lsp
  :hook
  ((tsx-ts-mode . lsp-deferred)
   (typescript-ts-mode . lsp-deferred))
  :custom
  (lsp-auto-guess-root t)
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; (use-package lsp-treemacs
;;   :commands
;;   lsp-treemacs-errors-list)

;; major modes
;; (use-package web-mode
;;   :hook
;;   (before-save . lsp-organize-imports)
;;   (before-save . lsp-format-buffer)
;;   :custom
;;   (web-mode-enable-auto-quoting nil)
;;   (web-mode-enable-auto-indentation nil))

;;; editor
(use-package counsel-jq)

(use-package vterm
  :config
  (unbind-key "C-c C-t" vterm-mode-map)
  (unbind-key "C-c C-f" vterm-mode-map)
  (unbind-key "C-c C-g" vterm-mode-map))

(use-package multi-vterm
  :bind
  ("C-c C-t" . multi-vterm))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

(use-package evil
  :custom
  (evil-toggle-key "C-`")
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-mode 1)
  ;; custom evil bindings
  (evil-define-key 'normal 'global (kbd "gi") 'lsp-find-implementation)
  (evil-define-key 'normal 'global (kbd "gd") 'lsp-find-definition)
  (evil-define-key 'normal 'global (kbd "gr") 'lsp-find-references)
  (evil-define-key 'emacs 'vterm-mode-map (kbd "C-<tab>") 'multi-vterm-next))

(use-package ivy
  :bind
  ("C-s" . swiper)
  :config
  (ivy-mode 1))

(use-package ivy-xref
  :ensure t
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel
  :demand
  :bind
  ("C-c C-g" . counsel-rg)
  :config
  (counsel-mode))

(use-package company
  :config
  (global-company-mode))

(use-package projectile
  :bind
  ("C-c C-f" . projectile-find-file)
  :config
  (projectile-mode))

(use-package magit)

(use-package visual-regexp
  :bind
  ("C-c r" . vr/replace))

;;; theme
(use-package doom-themes
  :config
  (load-theme 'doom-homage-white))

;; remove scratch message
(setq initial-scratch-message nil)
;; disable the awful bell sound
(setq ring-bell-function 'ignore)
;; reoad files when modified by external program
(global-auto-revert-mode 1)
;; disable backups
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; remove unnecessary emacs ui
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; reuse directory buffer
(setq dired-kill-when-opening-new-dired-buffer t)

;; simplify y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; fix large file performance problems
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)


(setq treesit-extra-load-path '("~/.emacs.d/languages"))
(setq-default typescript-ts-mode-indent-offset 4)
(setq-default indent-tabs-mode nil)

;;; CL
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")


;;; custom functions
(defun copy-filename-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(provide 'init)
;;; init.el ends here
