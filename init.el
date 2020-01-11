;;; init.el --- Mark Wissink

;;; Commentary:
;;; a block of text so emacs-lisp stops complaining

;;; Code:

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
  :mode
  (("\\.org$" . org-mode)))

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

(use-package smex)

(use-package ivy
  :config
  (ivy-mode 1))

(use-package counsel
  :bind*
  (("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file)))

(use-package lsp-mode
  :hook
  ((prog-major-mode . lsp-prog-major-mode-enable)
    (lsp-after-open-hook . lsp-enable-imenu)
    (prog-mode . lsp))
  :config
  (setq
    lsp-prefer-flymake nil
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

(use-package company-lsp)

;; (use-package ccls
;;   :config
;;   (setq flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;   (setq ccls-executable "/user/bin/ccls")
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;           (lambda () (require 'ccls) (lsp) (message "hello ccls"))))

(use-package projectile
  :config
  (projectile-mode 1))

(use-package js2-mode
  :mode
  ("\\.js\\'" . js2-jsx-mode)
  :interpreter
  ("node" . js2-jsx-mode))

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

C++ configuration
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

;; Theme
(if (display-graphic-p)
  (progn
    (use-package cherry-blossom-theme)
    (load-theme 'cherry-blossom t))
  (progn
    (use-package monokai-theme)
    (load-theme 'monokai t)))

(set-cursor-color "#aaaaaa")
(set-face-attribute 'fringe nil :background "#000000")
(set-face-foreground 'vertical-border (face-background 'fringe))

;; Set company theme since cherry blossom doesn't
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
    ;; `(mode-line-inactive ((t (:background "#000000"))))
    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Other aesthetic customization
(set-face-attribute 'default nil :height 90)
(prefer-coding-system 'utf-8)
;;(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
;; Disable the awful bell in windows
(setq ring-bell-function 'ignore)

;; I don't like auto saving
;; (setq auto-save-default nil)
;; Remove unnecessary things
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(ansi-color-faces-vector
     [default default default italic underline success warning error])
  '(ansi-color-names-vector
     ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
  '(custom-safe-themes
     (quote
       ("a77ced882e25028e994d168a612c763a4feb8c4ab67c5ff48688654d0264370c" "04589c18c2087cd6f12c01807eed0bdaa63983787025c209b89c779c61c3a4c4" default)))
 '(delete-selection-mode nil)
 '(fringe-mode 6 nil (fringe))
 '(linum-format (quote dynamic))
  '(package-selected-packages
     (quote
       (ccls monokai-theme sudoku tramp speed-type soothe-theme org-bullets a js2-mode auctex w3m web-mode tide projectile ace-window grandshell-theme alect-themes alect-theme lua-mode boon-qwerty counsel smex ivy lsp-ui flycheck-inline flycheck ido-vertical-mode company-lsp company lsp-mode cherry-blossom-theme editorconfig use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#199919991999"))))
 '(company-scrollbar-fg ((t (:background "#0ccc0ccc0ccc"))))
 '(company-tooltip ((t (:inherit default :background "#222222"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(mode-line-inactive ((t (:background "#000000")))))
