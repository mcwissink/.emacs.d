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
  :config
  (global-flycheck-mode))

(use-package evil
  :config
  (evil-mode 1))

(use-package flycheck-inline
  :config
  (with-eval-after-load 'flycheck (global-flycheck-inline-mode)))

(use-package ivy
  :config
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
  :config
  (setq
    lsp-inhibit-message nil
    lsp-highlight-symbol nil
    lsp-enable-snippet nil))

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package company-lsp)
  
;; (use-package ace-window
;;   :config
;;   (global-set-key (kbd "C-x o") 'ace-window)
;;   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package projectile
  :config
  (projectile-mode +1))

(use-package cquery
  :config
  (setq cquery-executable "/usr/bin/cquery")
  (setq cquery-extra-init-params '(:extraClangArguments ("-I/home/mark/Documents/Programming/tsal/include -I/usr/local/include/rtaudio"))))

(use-package tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
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






;; Theme
(use-package cherry-blossom-theme)
(if (display-graphic-p)
    (load-theme 'cherry-blossom t)
  (load-theme 'manoj-dark))

(set-cursor-color "#aaaaaa")

;; Set company theme since cherry blossom doesn't
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
    `(company-tooltip ((t (:inherit default :background "#222222"))))
    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; C++ configuration
(defun my-c-setup ()
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inclass '++)
  (c-set-offset 'access-label '-))
(add-hook 'c++-mode-hook 'my-c-setup)

;; Other customization
(set-face-attribute 'default nil :height 90)
(prefer-coding-system 'utf-8)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
;; I don't like auto saving
(setq auto-save-default nil)
;; Remove unnecessary things
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (w3m web-mode tide projectile ace-window grandshell-theme alect-themes alect-theme lua-mode boon-qwerty counsel smex ivy lsp-ui flycheck-inline flycheck ido-vertical-mode company-lsp company lsp-mode cherry-blossom-theme editorconfig use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#199919991999"))))
 '(company-scrollbar-fg ((t (:background "#0ccc0ccc0ccc"))))
 '(company-tooltip ((t (:inherit default :background "#222222"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
