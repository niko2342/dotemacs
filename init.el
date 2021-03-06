;;; init.el --- Emacs configuration

;; Copyright (C) 2020 Nico Wagner <nwagner84@protonmail.com>
;; Author: Nico Wagner <nwagner84@protonmail.com>
;; Keywords: Emacs, configuration

;;; Commentary:

;; This package contains my private Emacs configuration.

;;; Code:
(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Disable menu-bar-mode.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Disable toolbar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Disable scrollbar.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Always load newest byte code.
(setq load-prefer-newer t)

;; Disable ring bell.
(setq ring-bell-function 'ignore)

;; Improve scrolling.
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Enable y/n anwsers.
(fset 'yes-or-no-p 'y-or-n-p)

;; Wrap lines at 79 characters.
(setq-default fill-column 79)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Speed up Emacs startup time by reducing the frequency of garbage
;; collection; run garbage collection each 50MB of data. Default
;; values are restored after initialization.
(let ((gc-cons-percentage-old gc-cons-percentage)
      (gc-cons-threshold-old gc-cons-threshold))

  (setq gc-cons-threshold (* 50 1024 1024)
	gc-cons-percentage 0.1)

  (add-hook 'after-init-hook
	    `(lambda ()
	       (setq gc-cons-percentage ,gc-cons-percentage-old
		     gc-cons-threshold ,gc-cons-threshold-old)
	       (garbage-collect)) t))

(require 'use-package)
(setq use-package-verbose t)

(use-package browse-url
  :custom
  (browse-url-browser-function 'browse-url-firefox))

(use-package autorevert
  :config
  (global-auto-revert-mode t))

(use-package auto-virtualenv
  :ensure t
  :hook (python-mode . auto-virtualenv-set-virtualenv))

(use-package avy
  :ensure t
  :bind (("M-g w" . avy-goto-word-1)
         ("M-g f" . avy-goto-line)
         ("C-." . avy-goto-word-or-subword-1)
         ("C-," . avy-goto-char)))

(use-package company
  :ensure t
  :defer 5
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-tooltip-flip-when-above)
  (company-tooltip-limit 10))

(use-package company-ghc
  :ensure t
  :defer t
  :after company)

(use-package company-ghci
  :ensure t
  :defer t
  :after company
  :config
  (push 'company-ghci company-backends))

(use-package company-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook (lambda()
				(add-to-list 'company-backends 'company-jedi))))

(use-package compile
  :custom
  (compilation-scroll-output 'first-error))

(use-package counsel
  :ensure t
  :bind (([remap execute-extended-command] . counsel-M-x)
	 ([remap find-file] . counsel-find-file)
	 ([remap yank-pop] . counsel-yank-pop)
	 ([remap describe-function] . counsel-describe-function)
	 ([remap describe-variable] . counsel-describe-variable)
         ("C-c i" . counsel-semantic-or-imenu)))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  (counsel-projectile-mode 1))

(use-package custom
  :config
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file))

(use-package delsel
  :config
  ;; Delete selections with a keypress.
  (delete-selection-mode t))

(use-package diminish
  :ensure t)

(use-package dired
  :custom
  (dired-listing-switches "-lXGh --group-directories-first"))

(use-package eldoc
  :diminish)

(use-package files
  :custom
  (require-final-newline t "add newline at end of file")
  ;; store all backup and autosave files in the tmp dir
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (backup-directory-alist `((".*" . ,temporary-file-directory))))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package format-all
  :ensure t
  :disabled t
  :bind ("C-c C-f" . format-all-buffer)
  :hook (haskell-mode . format-all-mode))

(use-package frame
  :config
  (if (string-equal (system-name) "L-WS-40009-VM")
      (set-frame-font "FuraCode Nerd Font 10")
    (set-frame-font "FuraCode Nerd Font 14"))
  ;; Disable blinking cursor.
  (blink-cursor-mode -1))

(use-package haskell-mode
  :ensure t
  :bind (:map haskell-mode-map
              ("C-c C-c" . haskell-compile)
              ("M-g i" . haskell-navigate-imports))
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends))))
  :custom
  (haskell-compile-cabal-build-command "stack build")
  (haskell-interactive-popup-errors nil)
  (haskell-stylish-on-save t)
  (haskell-tags-on-save t))


(use-package hindent
  :ensure t
  :disabled t
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package hippie-exp
  ;; "Hippie" expansion provides a variety of completions and expansions.
  :bind (("M-/" . hippie-expand))
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package ivy
  :ensure t
  :diminish
  :bind (([remap switch-to-buffer] . ivy-switch-buffer)
	 ("C-c C-r" . ivy-resume))
  :custom
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (when (fboundp 'magit-display-buffer-fullframe-status-v1)
    (setq magit-display-buffer-function
          #'magit-display-buffer-fullframe-status-v1))
  (setq transient-default-level 5))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown"))

(use-package mule
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(use-package mustache-mode
  :ensure t
  :mode ("\\.mustache\\'" . mustache-mode))

(use-package newcomment
  :bind (([remap comment-dwim] . comment-line)))

(use-package projectile
  :ensure t
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/projects/" "~/work/")
	projectile-sort-order 'recentf
	projectile-completion-system 'ivy)
  (projectile-mode 1))

(use-package python
  :custom
  (python-shell-interpreter-args "-m IPython --simple-prompt -i")
  (python-shell-interpreter "python3")
  :config
  (add-hook 'python-mode-hook
            (lambda()
              (setq imenu-create-index-function
                    #'python-imenu-create-flat-index))))

(use-package python-black
  :ensure t
  :demand t
  :diminish python-black-on-save-mode
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package py-isort
  :ensure t
  :defer
  :hook (before-save . py-isort-before-save))

(use-package racer
  :ensure t
  :after rust-mode
  :diminish
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package recentf
  :custom
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 15)
  :config
  (recentf-mode +1))

(use-package rust-mode
  :ensure t
  :custom
  (rust-format-on-save t))

(use-package saveplace
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  :config
  (save-place-mode t))

(use-package savehist
  ;; Save minibuffer history.
  :custom
  (savehist-file (expand-file-name "savehist" user-emacs-directory))
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  :config
  (savehist-mode +1))

(use-package shackle
  :ensure t
  :custom
  (shackle-rules
   '((compilation-mode :popup t :select t :align 'right :size 0.5)
     ("*compilation*" :popup t :select t :align 'right :size 0.5)
     (t :select t)))
  :config
  (shackle-mode t))

(use-package shfmt
  :ensure t
  :hook (sh-mode . shfmt-on-save-mode))

(use-package simple
  :config
  ;; Display line number in mode line.
  (line-number-mode t)
  ;; Display column number in mode line.
  (column-number-mode t)
  ;; Display buffer size in mode line.
  (size-indication-mode t))

(use-package "startup"
  :custom
  (inhibit-startup-screen t "disable startup screen"))

(use-package subword
  :hook ((prog-mode . subword-mode)))

(use-package undo-tree
  :ensure t
  :diminish
  :init
  (global-undo-tree-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" . yaml-mode))

(use-package yasnippet
  :ensure t
  :diminish
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(provide 'init)
;;; init.el ends here
