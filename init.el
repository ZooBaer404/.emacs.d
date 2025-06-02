;; init.el --- all of the configurations	-*- lexical-binding: t -*-
;; ------------------------------
;; Package Manager Bootstrap
;; ------------------------------

;;; Packages-init

;;;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			                    ("org"   . "https://orgmode.org/elpa/")
			                    ("elpa"  . "https://elpa.gnu.org/packages/")
													("nongnu" . "https://elpa.nongnu.org/nongnu/")))
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


;;; better *help*
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-c C-d" . helpful-at-point)
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

;;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breating room
(menu-bar-mode -1)   ; Disable the menu bar
(setq visual-bell t)  ; Set up the visual bell
(set-face-attribute 'default nil :font "CaskaydiaCove NF" :height 100)
(global-hl-line-mode t)

(savehist-mode 1) ; Preserve minibuffer history across sessions
(recentf-mode 1)  ; Access recent files quickly
(global-auto-revert-mode 1) ; Automatically reload files changed on disk


(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode)
  :bind (:map default-text-scale-mode-map
         ("s-="   . default-text-scale-increase)
         ("s--"   . default-text-scale-decrease)
         ("s-0"   . default-text-scale-reset)
         ("C-s-=" . default-text-scale-increase)
         ("C-s--" . default-text-scale-decrease)
         ("C-s-0" . default-text-scale-reset)))

;;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode)
  :config (dolist (mode '(dashboard-mode emacs-news-mode))
            (add-to-list 'page-break-lines-modes mode)))

;;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :diminish)

;;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Parenthses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Mouse & Smooth Scroll

;;;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(10 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 10
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

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

;; -------------------------------
;; Shell & Terminal Mode
;; -------------------------------

(use-package eat
  :pin nongnu
  :custom
  (eat-kill-buffer-on-exit t)
  :config
  (delete [?\C-u] eat-semi-char-non-bound-keys) ; make C-u work in Eat terminals like in normal terminals
  (delete [?\C-g] eat-semi-char-non-bound-keys) ; ditto for C-g
  (eat-update-semi-char-mode-map)
  ;; XXX: Awkward workaround for the need to call eat-reload after changing Eat's keymaps,
  ;; but reloading from :config section causes infinite recursion because :config wraps with-eval-after-load.
  (defvar eat--prevent-use-package-config-recursion nil)
  (unless eat--prevent-use-package-config-recursion
    (setq eat--prevent-use-package-config-recursion t)
    (eat-reload))
  (makunbound 'eat--prevent-use-package-config-recursion))

(use-package eterm-256color
  :hook ((term-mode . eterm-256color-mode)
          (eshell-mode . eterm-256color-mode)))

(use-package term
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000))

(defun zoobaer/configure-eshell ()
  ;; save command history when commands are entered
  (add-hook  'eshell-pre-command-hook 'eshell-save-some-history)

  ;; truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; useful keybindings
  (define-key eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (define-key eshell-mode-map (kbd "<home>") 'eshell-bol)

  (setq
    eshell-history-size 10000
    eshell-buffer-maximum-lines 10000
    eshell-hist-ignoredups t
    eshell-scroll-to-buttom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :ensure t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell
  :hook (eshell-first-time-mode . zoobaer/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "btop" "zsh" "vim" "screen" "tmux" "top" "less" "more" "lynx" "ncftp" "mutt" "pine" "tin" "trn" "elm" "vi" "emacs -nw")))
  (eshell-git-prompt-use-theme 'powerline))


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

(use-package counsel
  :init
  (counsel-mode))

;;; completion annotations
(use-package marginalia
  :after ivy
  :init
  (marginalia-mode))

;;; show keybinding hints & minibuffer help
(use-package embark
  :bind
  (("C-." . embark-act)))

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


;;; find the root of the project
(defun projectile-project-find-function (dir)
  (let* ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'projectile-project-find-function))


;; --------------------------------
;; Navigation
;; --------------------------------

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-c C-j" . direc-jump)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "loupe")
                                 ("mkv" . "vlc")
                                 ("mp4" . "vlc"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map "H" dired-hide-dotfiles-mode))

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


;; (defun set-indent (mode tab-width use-tabs) 
;;   (let ((hook (intern (format "%s-hook" mode))))
;;     (add-hook hook
;;       (lambda ()
;;         (setq-local tab-width tab-width)
;;         (setq-local indent-tabs-mode use-tabs)))))

;;;; Pair

(use-package smartparens
  :hook ((text-mode . smartparens-mode)
	        (prog-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1))


;;;; Indent


;;;; Basic LSP

(use-package eglot
  :hook (((go-mode. eglot-ensure)
	         (cc-mode . eglot-ensure)
           (python-mode . eglot-ensure)
	         (java-mode . eglot-ensure)
	         (lua-mode . eglot-ensure)
	         (zig-mode . eglot-ensure)
	         (json-mode . eglot-ensure)
	         (markdown-mode . eglot-ensure)
	         (sh-mode . eglot-ensure)
	         (powershell-mode . eglot-ensure)
	         (clojure-mode . eglot-ensure)

					 (python-mode . eglot-mode)
					 (c-mode . eglot-mode)
					 (go-mode . eglot-mode)))
  :bind (:map eglot-mode-map
	        ("M-."   . xref-find-definitions)
	        ("M-?"   . xref-find-references)
	        ("C-c f" . eglot-format-buffer)
	        ("C-c r" . eglot-rename)
	        ("C-c a" . eglot-code-actions)
	        ("C-c h" . eglot-hover)
	        ("C-c C-x f" . eglot-find-references)
	        ("C-c s" . eglot-workspace-symbol)
	        ("C-c n" . flymake-goto-next-error)
	        ("C-c p" . flymake-goto-prev-error))
  :config
  (setq eldoc-documentation-strategy
	  #'eldoc-documentation-compose-eagerly) ; signature help

  (setq eldoc-echo-area-use-multiline-p nil) ; single line eldoc
  (setq eldoc-idle-delay 0.1)

  ;; don't allow servers to format the buffer
  (setq eglot-ignored-server-capabilities '(:documentFormattingProvider :documentRangeFormattingProvider))
  ;; inlay hints
  (when (boundp 'eglot-extend-to-xref)
    (setq eglot-extend-to-xref t))

  ;;inlay hints at startup
  (add-hook 'eglot-managed-mode-hook
	  (lambda ()
	    (when (eglot-server-capable :inlayHintProvider)
		    (eglot-inlay-hints-mode 1))))

  (setq completion-auto-help nil)
  (setq completion-show-inline-help nil))

(with-eval-after-load 'eglot
  (setq eglot-server-programs
    '((c-mode . ("clangd"))
       (c++-mode . ("clangd"))
       (go-mode . ("gopls"))
       (python-mode . ("pyright-langserver" "--stdio"))
       (java-mode . ("jdtls"))
       (lua-mode . ("lua-language-server"))
       (zig-mode . ("zls"))
       (json-mode . ("vscode-json-languageserver" "--stdio"))
       (markdown-mode . ("marksman"))
       (sh-mode . ("bash-language-server" "start"))
       (powershell-mode . ("powershell-editor-services" "--stdio"))
       (clojure-mode . ("clojure-lsp")))))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; ;;;; Detect root of the project

;; (defun zoobaer/project-root-detect (dir)
;;   "Detect project root for various build systems and languages."
;;   (let ((root (or
;;                (locate-dominating-file dir "go.mod")
;;                (locate-dominating-file dir "zig.mod")
;;                (locate-dominating-file dir "makefile")
;;                (locate-dominating-file dir "pom.xml")
;;                (locate-dominating-file dir "build.gradle.kts")
;;                (locate-dominating-file dir "build.gradle")
;;                (locate-dominating-file dir "Cargo.toml")
;;                (locate-dominating-file dir "compile_commands.json"))))
;;     (when root
;;       (cons 'vc root)))) ;; Use 'vc' to reuse Emacs’s built-in project backend

;; (add-hook 'project-find-functions #'zoobaer/project-root-detect)


;; Debugging


;;; Assembly

(use-package asm-mode
  :mode ("\\.asm\\'" "\\.s\\'" "\\.S\\'"))

(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-width 8 ;; or whatever asm wants
                  indent-tabs-mode t)))


;;; C

(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "k&r"
	c-basic-offset 4))


(use-package c++-mode
  :ensure nil
  :mode ("\\.cpp\\'" "\\.hpp\\'" "\\.cc\\'" "\\.hh\\'"))

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

;;; Go

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" "\\.mod\\'")
  :hook ((before-save . gofmt-before-save )))

;; (set-indent 'go-mode 4 t) ;; Go prefers tabs

(setq gofmt-command "goimports")

;;; Python

(use-package python
  :ensure nil
	:custom
	(python-shell-interpreter "python3"))

(use-package elpy
  :ensure t
	:config
  (require 'elpy-config)
  :hook
	(python-mode . elpy-mode))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))


;; (set-indent 'python-mode 4 0)


;;;; Function Signature and auto-import
(use-package jedi
  :hook (python-mode . jedi:setup))


;;; Java

(use-package java-mode
  :ensure nil
  :mode "\\.java\\'")

;; (set-indent 'java-mode 4 t)

;;; Lisp

(defun lisp-common-setup ()
  (electric-pair-mode -1)
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (hungry-delete-mode -1)
  (whitespace-mode -1)
  (setq tab-width 2
    lisp-indent-offset 2
    indent-tabs-mode 0))

;;;; Disable electric-indent mode for Lisp
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook
		slime-repl-mode-hook
                clojure-mode-hook))
  (add-hook hook #'lisp-common-setup))

;;;; Common Lisp

(use-package slime

  :ensure t
  :init
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

  ;; load slime helper from the quicklisp library manager for Common Lisp
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  
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

  (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))


(defun my/common-lisp-setup ()
  (slime-mode +1))

(add-hook 'lisp-mode-hook #'my/common-lisp-setup)

;;;; Clojure

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'")

(use-package cider
  :ensure t
  :hook (clojure-mode . cider-mode))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;;;;

;;; Lua

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

;; (set-indent 'lua-mode 4 0)

;;; Zig

(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'")

;; (set-indent 'zig-mode 4 0)

;;; JSON

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; (set-indent 'json-mode 2 t)

;;; Markdown

;;;; ToDo: Use org-mode for markdown mode!!
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

;; (set-indent 'markdown-mode 2 0)


;;; Shell

(use-package sh-script
  :ensure nil
  :mode ("\\.sh\\'" . sh-mode))

;; (set-indent 'sh-mode 2 0)

;;; Powershell

(use-package powershell
  :ensure t
  :mode ("\\.ps1\\'" . powershell-mode))

;; (set-indent 'powershell-mode 4 0)

;; Julia

(use-package julia-mode
	:mode ("\\.jl\\'" . julia-mode)
	:config
	(define-key julia-mode-map (kbd "TAB") 'julia-latexsub-or-indent))

(use-package julia-repl
	;; :hook (julia-mode . julia-repl-mode)
	:init
	(setenv "JULIA_NUM_THREADS" "8")
	:config
	(julia-repl-set-terminal-backend 'vterm)
	(define-key julia-repl-mode-map (kbd "C-c C-v") 'julia-repl-send-cell)
  (define-key julia-repl-mode-map (kbd "C-c C-l") 'julia-repl-send-line))

(use-package julia-snail
  :ensure t
  :hook
  (julia-mode . julia-snail-mode))

(add-to-list 'display-buffer-alist
             '("\\*julia" (display-buffer-reuse-window display-buffer-same-window)))

;; key 	command and description
;; C-c C-z 	julia-snail
;; start a REPL; flip between REPL and source
;; C-c C-a 	julia-snail-package-activate
;; activate the project using Project.toml
;; C-c C-d 	julia-snail-doc-lookup
;; display the docstring of the identifier at point
;; C-c C-l 	julia-snail-send-line
;; evaluate current line in the current module (or in Main with prefix arg;
;; or copy directly to REPL with two prefix args)
;; C-c C-r 	julia-snail-send-region
;; evaluate active region in the current module (or in Main with prefix arg;
;; or copy directly to REPL with two prefix args)
;; C-c C-e 	julia-snail-send-dwim
;; if region active, evaluate it in current module;
;; else if on top-level block, evaluate it in current module;
;; else evaluate current line
;; C-c C-c 	julia-snail-send-top-level-form
;; evaluate end-terminated block around the point in the current module
;; C-M-x 	julia-snail-send-top-level-form
;; ditto
;; C-c C-k 	julia-snail-send-buffer-file
;; include() the current buffer’s file
;; C-c C-R 	julia-snail-update-module-cache
;; update module-nested include cache (mainly for Revise)


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
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  (setq org-agenda-files
	'("~/org/tasks.org"))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))))

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
		'("014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
			 "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
			 "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
			 "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
			 "fffef514346b2a43900e1c7ea2bc7d84cbdd4aa66c1b51946aade4b8d343b55a"
			 "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
			 default))
 '(package-selected-packages
		'(aggressive-indent all-the-icons-dired clj-refactor command-log-mode
			 counsel-projectile default-text-scale dired-hide-dotfiles
			 dired-open doom-modeline doom-themes eat eglot elpy emacs-jl
			 embark-consult embrace eshell-git-prompt
			 eshell-syntax-highlighting eterm-256color flycheck go-mode
			 helpful hungry-delete iscroll jedi json-mode julia-repl
			 julia-snail lsp-java lsp-ui lua-mode magit marginalia
			 mixed-pitch orderless org-bullets page-break-lines powershell
			 python-mode pyvenv rainbow-delimiters slime smartparens
			 surround ultra-scroll vterm which-key zig-mode))
 '(package-vc-selected-packages
		'((ultra-scroll :vc-backend Git :url
				"https://github.com/jdtsmith/ultra-scroll"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
