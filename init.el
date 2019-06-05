(package-initialize)

(require 'package)
(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package company
  :config
  (global-company-mode 1))

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package evil
  :init
  (require 'evil)
  (evil-mode 1))

;(use-package flycheck-inline
 ; :init
  ;(with-eval-after-load 'flycheck (global-flycheck-inline-mode)))

(use-package ivy
  :init
  (ivy-mode 1))

(use-package smex)

(use-package counsel :ensure t
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
   ("C-c f"   . counsel-git)       ; search for files in git repo
   ("C-c s"   . counsel-git-grep)  ; search for regexp in git repo
   ("C-c /"   . counsel-ag)        ; search for regexp in git repo using ag
   ("C-c l"   . counsel-locate))   ; search for files or else using locate
  )

;; lsp-mode
(use-package lsp-mode
  :hook
  ((prog-major-mode . lsp-prog-major-mode-enable)
    (lsp-after-open-hook . lsp-enable-imenu)
    (prog-mode . lsp))
  :init
  (setq
    lsp-inhibit-message nil
    lsp-highlight-symbol nil
    lsp-enable-snippet nil))

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package company-lsp)
  
;; Dark theme :)
(use-package cherry-blossom-theme)
(load-theme 'cherry-blossom t)

;; C++ configuration
(defun my-c-setup ()
   (c-set-offset 'innamespace 0))
(add-hook 'c++-mode-hook 'my-c-setup)

;; Other customization
(set-face-attribute 'default nil :height 90)
(prefer-coding-system 'utf-8)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq initial-scratch-message nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(package-selected-packages
     (quote
       (lua-mode boon-qwerty counsel smex ivy lsp-ui flycheck-inline flycheck ido-vertical-mode company-lsp company lsp-mode cherry-blossom-theme editorconfig use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
