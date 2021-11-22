;;; init.el --- Mark Wissink

;;; Commentary:

;;; Code:

;;; random fixes
;; Fix for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (setq ghub-use-workaround-for-emacs-bug 'force)

(defun ensure-file (path path-callback)
  "Give a handle to a file path"
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
;; lsp
(use-package lsp-mode
  :commands
  lsp
  :hook
  (
    (typescript-mode . lsp)
    (web-mode . lsp)
    (js-mode . lsp))
  :config
  (defun ts-before-save ()
    (lsp-format-buffer)
    (lsp-organize-imports))
  (add-hook 'web-mode-hook
    (lambda () (add-hook 'before-save-hook #'ts-before-save nil 'local)))
  (add-hook 'typescript-mode-hook
    (lambda () (add-hook 'before-save-hook #'ts-before-save nil 'local)))
  :custom
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :commands
  lsp-ui-mode)

(use-package lsp-ivy
  :commands
  lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands
  lsp-treemacs-errors-list)

;; major modes
(use-package handlebars-mode
  :custom
  (handlebars-basic-offset 4)
  :mode
  "\\.tpl\\'")

(use-package typescript-mode)

(use-package web-mode
  :custom
  (web-mode-enable-auto-quoting nil)
  ;;(web-mode-code-indent-offset 2)
  :mode
  "\\.tsx\\'")

;;; editor
(use-package counsel-jq)

(use-package vterm)

(use-package multi-vterm
  :bind
  (
    ("C-c t" . multi-vterm)))

(use-package editorconfig
  :config
  (add-hook 'editorconfig-after-apply-functions
	    (lambda (props) (setq web-mode-block-padding 0)))
  (editorconfig-mode 1))

(use-package undo-tree
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
  (evil-define-key 'normal 'global (kbd "zr") 'query-replace)
  (evil-define-key 'normal 'global (kbd "zsr") 'lsp-rename)
  (evil-define-key 'normal 'global (kbd "zc") 'lsp-execute-code-action)
  (evil-define-key 'normal 'global (kbd "zf") 'swiper)
  (evil-define-key 'normal 'global (kbd "zg") 'counsel-rg)
  (evil-define-key 'normal 'global (kbd "zp") 'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "zsp") 'counsel-fzf)
  (evil-define-key 'normal 'global (kbd "zj") 'flycheck-next-error)
  (evil-define-key 'normal 'global (kbd "zk") 'flycheck-previous-error)
  (evil-define-key 'normal 'global (kbd "ze") 'flycheck-explain-error-at-point)
  (evil-define-key 'emacs 'vterm-mode-map (kbd "C-<tab>") 'multi-vterm-next)
  (evil-define-key 'emacs 'vterm-mode-map (kbd "C-<S-<tab>>") 'multi-vterm-prev))

(use-package ivy
  :bind
  (
    ("C-s" . swiper))
  :config
  (ivy-mode 1))

(use-package counsel
  :demand
  :bind
  (
    ("C-c C-p" . counsel-fzf)
    ("C-c a" . counsel-rg))
  :config
  (counsel-mode 1))

(use-package company
  :config
  (global-company-mode 1))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package projectile
  :bind
  (
    ("C-c p" . projectile-find-file))
  :config
  (projectile-mode 1))

(use-package magit)

;; (use-package org)

;; (use-package org-roam
;;   :after org
;;   :custom
;;   (org-roam-directory "~/Documents/org/roam"))

;; (use-package deft
;;   :after org
;;   :custom
;;   (deft-recursive t)
;;   (deft-use-filter-string-for-filename t)
;;   (deft-default-extension "org")
;;   (deft-directory org-roam-directory))

;;; theme

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-light-hard))


;; (use-package twilight-bright-theme)
;; (use-package doom-themes
;;   :after twilight-bright-theme
;;   :config
;;   (load-theme 'twilight-bright t)
;;   (load-theme 'doom-solarized-light t)
;;   (set-face-attribute 'fringe nil :background "#FDF6E3" :foreground "#FDF6E3"))

;; (custom-set-faces
;;   '(mode-line ((t (:background "#424242" :box (:line-width 4 :color "#424242")))))
;;   '(mode-line-inactive ((t (:background "#424242" :box (:line-width 4 :color "#424242"))))))

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
(put 'dired-find-alternate-file 'disabled nil)

;; simplify y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; remove annoying bindings
(unbind-key "s-t")
(unbind-key "s-p")

;; fix large file performance problems
(setq read-process-output-max (* 1024 1024))


;;; CL
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")


;;; custom functions
(defun copy-filename-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun note ()
  "Create a note file entry."
  (interactive)
  (ensure-file
   (format-time-string "~/Documents/notes/%Y-%m-%d.org")
   'find-file))

(defun future-note ()
  "Create a future note file entry."
  (interactive)
  (ensure-file
   (format "~/Documents/notes/%s.org" (org-read-date))
   'find-file))

(provide 'init)
;;; init.el ends here
