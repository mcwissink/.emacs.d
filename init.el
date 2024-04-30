(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; remove bindings
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

(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))

;;; coding
;; lsp-mode is required until eglot supports multiple language servers for a single mode
;; https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless
(use-package lsp-mode
  :bind
  ("C-c C-a" . lsp-execute-code-action)
  ("C-c C-r" . lsp-rename)
  :commands
  lsp
  :init
  (defun my/lsp-completion-mode ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  ((tsx-ts-mode . lsp-deferred)
   (typescript-ts-mode . lsp-deferred)
   (lsp-completion-mode . my/lsp-completion-mode))
  :custom
  (lsp-auto-guess-root t)
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package vterm
  :config
  (push (list "print-to-temp-buffer"
              (lambda (object)
                (with-current-buffer (generate-new-buffer "*temp*")
                  (insert-file-contents object)
                  (switch-to-buffer-other-window (current-buffer)))))
        vterm-eval-cmds)
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
  (evil-define-key 'normal 'global (kbd "K") 'lsp-ui-doc-glance)
  (evil-define-key 'normal 'global (kbd "E") 'consult-flymake)
  (evil-define-key 'emacs 'vterm-mode-map (kbd "C-<tab>") 'multi-vterm-next))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :bind
  ("C-." . embark-act))

(use-package embark-consult)

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-alpha))

(use-package consult
  :bind
  ("C-c C-g" . consult-ripgrep)
  ("C-x b" . consult-buffer)
  ("C-s" . consult-line)
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))

(use-package wgrep)

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :config
  (global-corfu-mode))

(use-package company)

(use-package cape
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package projectile
  :bind
  ("C-c C-f" . projectile-find-file)
  :config
  (projectile-mode))

(use-package magit)

(use-package visual-regexp
  :bind
  ("C-c r" . vr/replace))

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

(defun copy-filename-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(provide 'init)
;;; init.el ends here
