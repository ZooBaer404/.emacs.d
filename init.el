;; UI

(setq inhibit-startup-message t) ;; No welcome screen
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breating room
(menu-bar-mode -1)   ; Disable the menu bar
(setq visual-bell t)  ; Set up the visual bell
(set-face-attribute 'default nil :font "Jetbrains Mono SemiBold" :height 95)

;; Theme

(load-theme 'leuven-dark)

;; Packages

;;; Packages-init

;;;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; help

(use-package command-log-mode)

;; editor

(use-package swiper)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 13)))

;; Completion




;; Languages

;;; general

;;; C


;; Org


;; Keybindings

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-s") 'swiper)
