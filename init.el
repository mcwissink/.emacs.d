;;; init.el --- Mark Wissink

;;; Commentary:
;;; a block of text so stops complaining

;;; Code:
;;; Figure out how to load file without creating it first
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; (setq ghub-use-workaround-for-emacs-bug 'force)

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
  ("\\.org$" . org-mode)
  :preface
  (define-skeleton org-skeleton
    "org header"
    "Title: "
    "#+TITLE:" str "\n"
    "#+AUTHOR: Mark Wissink\n")
  :config
  (global-set-key (kbd "C-c k") 'org-capture)
  (setq org-directory "~/Dropbox/org/")
  (setq org-default-notes-file (concat org-directory "/todo.org"))
  (setq org-agenda-files '("~/Dropbox/org/"))
  (setq org-src-tab-acts-natively t)
  (setq org-capture-templates
    '(("d" "Task Details" entry (file+headline "" "Task Details")
        "* TODO %?\n\n")
       ("s" "Stand-Up" entry (file+headline "" "Stand-Up")
         "* TODO %t %?\n\n")
       ("t" "Task" entry (file+headline "" "Tasks")
         "* TODO [/] %^{Task}\n  %t\n  %a\n  - [ ] test\n  - [ ] deploy\n** Notes\n")))
  :init
  (require 'ox-publish)
  (setq org-publish-project-alist
    '(
       ("org-content"
         :base-directory "~/Programming/mcwissink.github.io/org/"
         :base-extension "org"
         :publishing-directory "~/Programming/mcwissink.github.io/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :author "Mark Wissink"
         )
       ("org-static"
         :base-directory "~/Programming/mcwissink.github.io/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Programming/mcwissink.github.io/"
         :recursive t
         :publishing-function org-publish-attachment
       )
       ("org-site" :components ("org-content" "org-static"))
    )))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package company
  :custom
  (company-tooltip-idle-delay 0.4)
  (company-dabbrev-d:config
  (setq lsp-completion-provider :capf))
  :config
  (global-company-mode 1))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package evil
  :config
  ;; (define-key evil-normal-state-map "\C-w" 'evil-delete)
  (evil-set-initial-state 'vterm-mode 'emacs)
  ;; (define-key evil-insert-state-map "\C-w" 'evil-delete)
  (evil-mode 1))

(use-package flycheck-inline
  :config
  (with-eval-after-load 'flycheck (global-flycheck-inline-mode)))

(use-package flyspell
  :hook
  ((org-mode text-mode) . flyspell-mode))

(use-package ivy
  :config
  (ivy-mode 1))

(use-package counsel
  :config
  (counsel-mode 1)
  :bind
  (
    ("C-x a" . counsel-rg)
    ))

(use-package deadgrep
  :bind (("C-c a" . #'deadgrep)))

(use-package swiper
  :bind
  (
    ("C-s" . swiper)
    ))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode 1)
  :bind
  (
    ("C-x p" . projectile-find-file)
    ))

(use-package lsp-mode
  :hook ((js-mode . lsp))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  :commands lsp)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package magit)

;; TypeScript configuration
(use-package tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode +1))

;; SCA development
(use-package web-beautify)

(use-package handlebars-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . handlebars-mode)))

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

(set-face-attribute 'default nil :height 110)
(prefer-coding-system 'utf-8)
(setq initial-scratch-message nil)
;; Disable the awful bell in windows
(setq ring-bell-function 'ignore)
(global-auto-revert-mode 1)
;; Disable backups because I don't like having to modify gitignores all the time
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

;; Remove unnecessary things
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(display-battery-mode 1)
(display-time-mode 1)
(put 'dired-find-alternate-file 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(unbind-key "s-t")
(unbind-key "s-p")
(setq read-process-output-max (* 1024 1024)) ; 1mb

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'js-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-eslint-fix-all)))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(provide 'init)
;;; init.el ends here
