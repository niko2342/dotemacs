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

;; Disable menu-bar-mode
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Disable toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package frame
  :config
  (set-frame-font "FuraCode Nerd Font 13"))

(use-package "startup"
  :custom
  (inhibit-startup-screen t "disable startup screen"))

(provide 'init)
;; init.el ends here
