;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html
;; Do this before package stuff so that the package thing does not
;; prompt which encoding I want to use to read packages with.
(prefer-coding-system 'utf-8)

(package-initialize)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")

(use-package
  editorconfig
  :config (editorconfig-mode 1))

;; Dark theme :)
(load-theme 'manoj-dark)

;; Key bindings
(global-set-key (kbd "C-z") 'shell)

;; Other customization
(tool-bar-mode -1)
(setq initial-scratch-message nil)
