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

;; Always load newest byte code.
(setq load-prefer-newer t)

;; Disable ring bell.
(setq ring-bell-function 'ignore)

;; Improve scrolling.
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

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

(use-package frame
  :config
  (set-frame-font "FuraCode Nerd Font 13")
  ;; Disable blinking cursor.
  (blink-cursor-mode -1))

(use-package "startup"
  :custom
  (inhibit-startup-screen t "disable startup screen"))

(provide 'init)
;; init.el ends here
