;;; init.el --- Emacs configuration

;; Copyright (C) 2020 Nico Wagner <nwagner84@protonmail.com>
;; Author: Nico Wagner <nwagner84@protonmail.com>
;; Keywords: Emacs, configuration

;;; Commentary:

;; This package contains my private Emacs configuration.

;;; Code:
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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

;; Wrap lines at 80 characters.
(setq-default fill-column 80)

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

(use-package autorevert
  :config
  (global-auto-revert-mode t))

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

(use-package counsel
  :ensure t
  :bind (([remap execute-extended-command] . counsel-M-x)
	 ([remap find-file] . counsel-find-file)
	 ([remap yank-pop] . counsel-yank-pop)
	 ([remap describe-function] . counsel-describe-function)
	 ([remap describe-variable] . counsel-describe-variable)))

(use-package counsel-projectile
  :ensure t
  :after projectile)

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

(use-package files
  :custom
  (require-final-newline t "add newline at end of file")
  ;; store all backup and autosave files in the tmp dir
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (backup-directory-alist `((".*" . ,temporary-file-directory))))

(use-package frame
  :config
  (set-frame-font "FuraCode Nerd Font 14")
  ;; Disable blinking cursor.
  (blink-cursor-mode -1))

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

(use-package mule
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(use-package projectile
  :ensure t
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/projects/" "~/work/")
	projectile-sort-order 'recentf
	projectile-completion-system 'ivy)
  (projectile-mode 1))

(use-package recentf
  :custom
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 15)
  :config
  (recentf-mode +1))

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

(provide 'init)
;; init.el ends here
