
; ------------------------------
;; Package Manager Bootstrap
;; ------------------------------

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

;; -----------------------
;; help
;; -----------------------

;;; enhanced M-x, buffer-switching, find file
(use-package consult)

;;; better *help*
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-command]  . helpful-command)
  ([remap describe-key]      . helpful-key))

(use-package command-log-mode)
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;; ------------------------------
;; UI Settings
;; ------------------------------

(setq inhibit-startup-message t) ;; No welcome screen
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breating room
(menu-bar-mode -1)   ; Disable the menu bar
(setq visual-bell t)  ; Set up the visual bell
(set-face-attribute 'default nil :font "CaskaydiaCove NF" :height 95)


;;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Parenthses

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Theme

(use-package doom-themes
  :ensure t
  :config
  ;; defaults
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-material-dark t)

  ;; flashing mode on error
  (doom-themes-visual-bell-config)
  ;; neotree theme
  (doom-themes-neotree-config)
  ;; org-mode
  (doom-themes-org-config))

;; editor

(use-package swiper)
(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 13)))

(use-package counsel)

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package surround
  :ensure t
  :bind-keymap ("C-c s" . surround-keymap))



;; -------------------------------
;; Shell
;; -------------------------------

(use-package vterm
    :ensure t)


;; -------------------------------
;; Mode Settings
;; -------------------------------

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

;; ------------------------------
;; Completion
;; ------------------------------

(use-package ivy
  :init
  (ivy-mode))

;;; completion annotations
(use-package marginalia
  :after ivy
  :init
  (marginalia-mode))

;;; sorting and filtering
(use-package orderless
  :custom
  (completion-style '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))


;;; show keybinding hints & minibuffer help
(use-package embark
  :bind
  (("C-." . embark-act)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; ---------------------------
;; Project
;; ---------------------------

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Music")
    (setq projectile-project-search-path `("~/Music")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;;; Magit

(use-package magit
  :commands (magit-status magit-get-current-branch))

;; (use-package force) ; Use Github from Emacs!!!

;;;

;; -------------------------------
;; Languages
;; -------------------------------

;;; general

;;;; Defaults

;;;; Functions

(defun set-indent (mode tab-width electric-pair &optional use-tabs)
  "Helper to set indentation for MODE."
  (add-hook mode
            (lambda ()
              (setq tab-width tab-width
		    electric-pair-mode electric-pair
                    indent-tabs-mode (if use-tabs t nil)))))

;;;; Pair

(use-package smartparens
  :hook ((text-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1))

;;;; Basic LSP

(use-package flycheck
  :hook (prog-mode . flycheck-mode))


(setq eldoc-echo-area-use-multiline-p t)

(add-hook 'prog-mode-hook #'eldoc-mode)


;;;; Detect root of the project

(defun zoobaer/project-root-detect (dir)
  "Detect project root for various build systems and languages."
  (let ((root (or
               (locate-dominating-file dir "go.mod")
               (locate-dominating-file dir "zig.mod")
               (locate-dominating-file dir "Makefile")
               (locate-dominating-file dir "pom.xml")
               (locate-dominating-file dir "build.gradle.kts")
               (locate-dominating-file dir "build.gradle")
               (locate-dominating-file dir "Cargo.toml")
               (locate-dominating-file dir "compile_commands.json"))))
    (when root
      (cons 'vc root)))) ;; Use 'vc' to reuse Emacs’s built-in project backend

(add-hook 'project-find-functions #'zoobaer/project-root-detect)



;;; Assembly

(use-package asm-mode
  :mode ("\\.asm\\'" "\\.s\\'" "\\.S\\'"))

(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-width 8 ;; or whatever asm wants
                  indent-tabs-mode t)))


;;; C

(use-package c-mode
  :ensure nil
  :hook (c-mode-common . (lambda ()
                           (local-set-key (kbd "M-.") 'xref-find-definitions)
                           (local-set-key (kbd "M-,") 'xref-pop-marker-stack)))
  :mode ("\\.c\\'" "\\.h\\'"))

(setq c-default-style "k&r"
      c-basic-offset 4)


(use-package c++-mode
  :ensure nil
  :mode ("\\.cpp\\'" "\\.hpp\\'" "\\.cc\\'" "\\.hh\\'"))

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

;;; Go

(use-package go-mode
  :mode ("\\.go\\'" "\\.mod'"))

(set-indent 'go-mode-hook 4 t t) ;; Go prefers tabs


;;; Python

(use-package python
  :mode "\\.py\\'")

(set-indent 'python-mode-hook 4 t nil)


;;;; Function Signature and auto-import
(use-package jedi
  :hook (python-mode . jedi:setup))


;;; Java

(use-package java-mode
  :ensure nil
  :mode "\\.java\\'")

(set-indent 'java-mode-hook 4 t t)

;;; Lisp

(dolist (mode '(emacs-lisp-mode lisp-mode lisp-interaction-mode clojure-mode scheme-mode))
  (add-hook mode
            (lambda ()
              (setq tab-width 2
                    lisp-indent-offset 2
                    indent-tabs-mode nil))))


;;;; Disable electric-indent mode for Lisp
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook
		slime-repl-mode-hook
                clojure-mode-hook))
  (add-hook hook (lambda ()
		   (electric-pair-mode -1)
                   (smartparens-strict-mode +1)
		   (rainbow-delimiters-mode +1)
		   (hungry-delete-mode -1))))

(dolist (hook '(lisp-interaction-mode-hook)))

;;;; Common Lisp

(use-package slime

  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")

  :config
  (slime-setup '(slime-fancy slime-repl slime-banner))
  (setq slime-complete-symbol*-fancy t))

(with-eval-after-load "slime"
  ;; a list of alternative Common Lisp implementations that can be
  ;; used with SLIME. Note that their presence render
  ;; inferior-lisp-program useless. This variable holds a list of
  ;; programs and if you invoke SLIME with a negative prefix
  ;; argument, M-- M-x slime, you can select a program from that list.
  (setq slime-lisp-implementations
        '((ccl ("ccl"))
          (clisp ("clisp" "-q"))
          (cmucl ("cmucl" "-quiet"))
          (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

  ;; select the default value from slime-lisp-implementations
  (if (and (eq system-type 'darwin)
           (executable-find "ccl"))
      ;; default to Clozure CL on macOS
      (setq slime-default-lisp 'ccl)
    ;; default to SBCL on Linux and Windows
    (setq slime-default-lisp 'sbcl))

  ;; Add fancy slime contribs
  (setq slime-contribs '(slime-fancy slime-cl-indent))

  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t
        slime-autodoc-use-multiline-p t)

  ;; rainbow-delimeters messes up colors in slime-repl, and doesn't seem to work
  ;; anyway, so we won't use prelude-lisp-coding-defaults.
  (add-hook 'slime-repl-mode-hook (lambda ()
                                    (smartparens-strict-mode +1)
                                    (whitespace-mode -1)
				    (rainbow-delimiters-mode +1)))

  (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))




(defun my/common-lisp-setup ()
  (slime-mode +1))

(add-hook 'lisp-mode-hook #'my/common-lisp-setup)



;;;; Clojure

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'")

;;;;

;;; Lua

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(set-indent 'lua-mode-hook 4 t t)

;;; Zig

(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'")

(set-indent 'zig-mode-hook 4 t t)

;;; JSON

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(set-indent 'json-mode-hook 2 t nil)

;;; Markdown

;;;; ToDo: Use org-mode for markdown mode!!
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(set-indent 'markdown-mode-hook 2 t nil)


;;; Shell

(use-package sh-script
  :ensure nil
  :mode ("\\.sh\\'" . sh-mode))

(set-indent 'sh-mode-hook 2 t nil)

;;; Powershell

(use-package powershell
  :ensure t
  :mode ("\\.ps1\\'" . powershell-mode))

(set-indent 'powershell-mode-hook 4 t nil)

;;; 

;; -------------------------
;; Org
;; -------------------------

(defun zoobaer/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . zoobaer/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c x" . org-capture)))

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode)))

(defun zoobaer/org-font-setup()
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1))))

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(dolist (mode '(text-mode org-mode))
  (add-hook mode (lambda () (electric-indent-local-mode -1))))

(add-hook 'message-mode-hook #'turn-on-orgtbl)
(add-hook 'prog-mode #'turn-on-orgtbl)


;; -------------------------
;; Keybindings
;; -------------------------

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "fffef514346b2a43900e1c7ea2bc7d84cbdd4aa66c1b51946aade4b8d343b55a"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
