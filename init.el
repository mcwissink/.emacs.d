;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html
;; Do this before package stuff so that the package thing does not
;; prompt which encoding I want to use to read packages with.
(prefer-coding-system 'utf-8)

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

(use-package hydra)

(use-package
  editorconfig
  :config (editorconfig-mode 1))

;; Dark theme :)
(use-package cherry-blossom-theme)
(load-theme 'cherry-blossom t)

;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Other customization
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq initial-scratch-message nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (cherry-blossom-theme editorconfig use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
