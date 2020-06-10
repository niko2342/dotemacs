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

;; setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(provide 'init)
;; init.el ends here
