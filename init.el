(use-package emacs
  :demand t
  :init
  ;; bootstrap init files
  (defun ensure-file (path path-callback)
    (unless (file-exists-p path)
      (with-temp-buffer (write-file path)))
    (funcall path-callback path))
  (ensure-file "~/.emacs.d/device.el" 'load)
  (ensure-file "~/.emacs.d/custom.el"
               (lambda (path)
                 (setq custom-file path)
                 (load custom-file)))

  (global-auto-revert-mode 1)
  (global-display-line-numbers-mode 1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (size-indication-mode 1)
  (column-number-mode 1)
  (global-unset-key (kbd "s-t"))
  (global-unset-key (kbd "s-p"))
  ;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (defalias 'yes-or-no-p 'y-or-n-p)
  :config
  (global-hl-line-mode)
  (defun copy-filename-to-clipboard ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
      (when filename
        (kill-new filename)
        (message "Copied buffer file name '%s' to the clipboard." filename))))

  ;; https://www.emacswiki.org/emacs/ExecuteExternalCommand
  (defun shell-command-on-buffer ()
    "Asks for a command and executes it in inferior shell with current buffer
as input."
    (interactive)
    (shell-command-on-region
     (point-min) (point-max)
     (read-shell-command "Shell command on buffer: ") t t))
  :bind
  ("C-!" . shell-command-on-buffer)
  :custom
  (standard-indent 4)
  (package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
  (treesit-extra-load-path '("~/.emacs.d/languages"))
  (read-process-output-max (* 1024 1024))
  (gc-cons-threshold 100000000)
  (dired-kill-when-opening-new-dired-buffer t)
  (initial-scratch-message nil)
  (ring-bell-function 'ignore)
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil))

(use-package vterm
  :ensure t
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :config
  (push (list "print-to-temp-buffer"
              (lambda (object)
                (with-current-buffer (generate-new-buffer "*temp*")
                  (insert-file-contents object)
                  (switch-to-buffer-other-window (current-buffer)))))
        vterm-eval-cmds))

(use-package multi-vterm
  :ensure t
  :after vterm)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-toggle-key "C-`")
  :config
  (evil-set-leader 'normal (kbd "<SPC>"))
  ;; custom evil bindings
  (evil-define-key '(normal insert emacs) 'global (kbd "C-<tab>") 'multi-vterm-next)
  (evil-define-key 'normal 'global (kbd "/") 'consult-line)
  (evil-define-key 'normal 'global (kbd "<leader>f") 'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>a") 'eglot-code-actions)
  (evil-define-key 'normal 'global (kbd "<leader>d") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>k") 'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>t") 'multi-vterm)
  (evil-define-key 'normal 'global (kbd "<leader>b") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>g") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>o") 'other-window)
  (evil-define-key 'normal 'global (kbd "<leader>r") 'vr/replace)
  (evil-define-key 'normal 'global (kbd "<leader>s") 'save-some-buffers)
  (evil-define-key 'normal 'global (kbd "<leader>x") 'execute-extended-command)
  (evil-define-key 'normal 'global (kbd "<leader>0") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>1") 'delete-other-windows)
  (evil-define-key 'normal 'global (kbd "<leader>2") 'split-window-below)
  (evil-define-key 'normal 'global (kbd "<leader>3") 'split-window-right)
  (evil-define-key 'normal 'global (kbd "E") 'consult-flymake)
  (evil-set-undo-system 'undo-tree)
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :config
  (setq evil-collection-key-blacklist '("<SPC>"))
  (evil-collection-init))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  ("C-." . embark-act))

(use-package embark-consult
  :ensure t)

(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  :custom
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if vertico-mode
                #'consult-completion-in-region
              #'completion--in-region)
            args)))
  (vertico-sort-function 'vertico-sort-alpha))

(use-package consult
  :ensure t
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))

(use-package wgrep
  :ensure t)

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :config
  (global-corfu-mode))

(use-package company
  :ensure t)

(use-package cape
  :ensure t
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
  :ensure t
  :config
  (projectile-mode))

(use-package magit
  :ensure t)

(use-package visual-regexp
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'leuven-dark))

(use-package typescript-ts-mode
  :ensure t
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package prettier-js
  :ensure t
  :hook
  (tsx-ts-mode . prettier-js-mode)
  (typescript-ts-mode . prettier-js-mode))

(use-package eglot
  :hook
  (prog-mode . eglot-ensure))

(provide 'init)
;;; init.el ends here

