(let ((file-name-handler-alist nil)))

(add-hook 'emacs-startup-hook
  (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq gc-cons-threshold most-positive-fixnum)

(setq debug-on-error t)

(setq gc-cons-threshold most-positive-fixnum)

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
(custom-set-variables '(use-package-verbose nil))

(use-package paradox
  :ensure t
  :custom
  (paradox-github-token t)
  :config
  (paradox-enable))

(custom-set-variables '(load-prefer-newer t))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))

(custom-set-variables '(confirm-kill-processes nil))

(defun zz/set-proxy ()
  (interactive)
  (customize-set-variable 'url-proxy-services
    '(("http"  . "proxy.corproot.net:8079")
       ("https" . "proxy.corproot.net:8079"))))
(defun zz/unset-proxy ()
  (interactive)
  (customize-set-variable 'url-proxy-services nil))

(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(require 'cl)

(use-package async
  :ensure t)

(add-hook 'before-save-hook 'time-stamp)

(custom-set-variables '(kill-whole-line t))

(custom-set-variables '(mouse-yank-at-point t))

(setq completion-ignore-case t)
(custom-set-variables
	'(read-buffer-completion-ignore-case t)
	'(read-file-name-completion-ignore-case t))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(custom-set-variables '(show-trailing-whitespace nil))

;; (show-paren-mode)

(custom-set-variables '(indent-tabs-mode nil))

(custom-set-variables
	'(backup-directory-alist
		 `(("." . ,(concat user-emacs-directory "backups")))))

(when (fboundp 'winner-mode) (winner-mode))

(use-package unfill
  :bind
  ("M-q" . unfill-toggle)
  ("A-q" . unfill-paragraph))

(use-package saveplace
  :config
  (save-place-mode))

(use-package imenu-anywhere
  :bind
  ("M-i" . helm-imenu-anywhere))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables '(ad-redefinition-action (quote accept)))

(cond ((eq system-type 'darwin)
				
				)
  ((eq system-type 'windows-nt)
    
    )
  ((eq system-type 'gnu/linux)
    
    ))

(use-package diminish
  :ensure t)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(defun zz/goto-match-paren (arg)
  "Go to the matching paren/bracket, otherwise (or if ARG is not
    nil) insert %.  vi style of % jumping to matching brace."
  (interactive "p")
  (if (not (memq last-command '(set-mark
                                 cua-set-mark
                                 zz/goto-match-paren
                                 down-list
                                 up-list
                                 end-of-defun
                                 beginning-of-defun
                                 backward-sexp
                                 forward-sexp
                                 backward-up-list
                                 forward-paragraph
                                 backward-paragraph
                                 end-of-buffer
                                 beginning-of-buffer
                                 backward-word
                                 forward-word
                                 mwheel-scroll
                                 backward-word
                                 forward-word
                                 mouse-start-secondary
                                 mouse-yank-secondary
                                 mouse-secondary-save-then-kill
                                 move-end-of-line
                                 move-beginning-of-line
                                 backward-char
                                 forward-char
                                 scroll-up
                                 scroll-down
                                 scroll-left
                                 scroll-right
                                 mouse-set-point
                                 next-buffer
                                 previous-buffer
                                 previous-line
                                 next-line
                                 back-to-indentation
                                 )))
    (self-insert-command (or arg 1))
    (cond ((looking-at "\\s\(") (sp-forward-sexp) (backward-char 1))
      ((looking-at "\\s\)") (forward-char 1) (sp-backward-sexp))
      (t (self-insert-command (or arg 1))))))

(bind-key "%" 'zz/goto-match-paren)

(use-package drag-stuff
  :ensure t
  :config
  (global-set-key (kbd "M-p") 'drag-stuff-up)
  (global-set-key (kbd "M-n") 'drag-stuff-down))

(use-package hydra)

(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

(setq use-file-dialog nil
  use-dialog-box nil
  inhibit-startup-screen t
  inhibit-startup-echo-area-message user-login-name
  inhibit-default-init t)
(tool-bar-mode -1)   ; Disable the toolbar
(set-fringe-mode 10) ; Give some breating room
(menu-bar-mode -1)   ; Disable the menu bar
(setq visual-bell t)  ; Set up the visual bell
(savehist-mode 1) ; Preserve minibuffer history across sessions
(recentf-mode 1)  ; Access recent files quickly


  ;;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
  frame-resize-pixelwise t)

;; Display dividers between windows
(setq window-divider-default-places t
  window-divider-default-bottom-width 1
  window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode)
  :config (dolist (mode '(dashboard-mode emacs-news-mode))
            (add-to-list 'page-break-lines-modes mode)))

;;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :ensure t
  :diminish)

(add-to-list 'default-frame-alist
             '(font . "CaskaydiaCove NF 10"))

(set-face-attribute 'default nil :font "CaskaydiaCove NF" :height 95)
(set-face-attribute 'fixed-pitch nil :font "CaskaydiaCove NF" :height 95)
(set-face-attribute 'variable-pitch nil :font "CaskaydiaCove NF" :height 95)

(pixel-scroll-precision-mode 1)

(use-package doom-themes
  :ensure t
  :config
  ;; defaults
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)

  ;; flashing mode on error
  (doom-themes-visual-bell-config)
  ;; neotree theme
  (doom-themes-neotree-config)
  ;; org-mode
  (doom-themes-org-config))

(load-theme 'doom-material-dark)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; If non-nil, cause imenu to see `doom-modeline' declarations.
;; This is done by adjusting `lisp-imenu-generic-expression' to
;; include support for finding `doom-modeline-def-*' forms.
;; Must be set before loading doom-modeline.
(setq doom-modeline-support-imenu t)

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 13)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 4)

;; Whether to use hud instead of default bar. It's only respected in GUI.
(setq doom-modeline-hud nil)

;; The limit of the window width.
;; If `window-width' is smaller than the limit, some information won't be
;; displayed. It can be an integer or a float number. `nil' means no limit."
(setq doom-modeline-window-width-limit 85)

;; Override attributes of the face used for padding.
;; If the space character is very thin in the modeline, for example if a
;; variable pitch font is used there, then segments may appear unusually close.
;; To use the space character from the `fixed-pitch' font family instead, set
;; this variable to `(list :family (face-attribute 'fixed-pitch :family))'.
(setq doom-modeline-spc-face-overrides nil)

;; How to detect the project root.
;; nil means to use `default-directory'.
;; The project management packages have some issues on detecting project root.
;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; to hanle sub-projects.
;; You can specify one if you encounter the issue.
(setq doom-modeline-project-detection 'auto)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   auto => emacs/l/comint.el (in a project) or comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   file-name-with-project => FOSS|comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are experiencing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'auto)

;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon t)

;; Whether display the icon for `major-mode'. It respects option `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display the colorful icon for `major-mode'.
;; It respects `nerd-icons-color-icons'.
;; (setq doom-modeline-major-mode-color-icon t)

;; Whether display the icon for the buffer state. It respects option `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display the modification icon for the buffer.
;; It respects option `doom-modeline-icon' and option `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether display the lsp icon. It respects option `doom-modeline-icon'.
(setq doom-modeline-lsp-icon t)

;; Whether display the time icon. It respects option `doom-modeline-icon'.
(setq doom-modeline-time-icon t)

;; Whether display the live icons of time.
;; It respects option `doom-modeline-icon' and option `doom-modeline-time-icon'.
(setq doom-modeline-time-live-icon t)

;; Whether to use an analogue clock svg as the live time icon.
;; It respects options `doom-modeline-icon', `doom-modeline-time-icon', and `doom-modeline-time-live-icon'.
(setq doom-modeline-time-analogue-clock t)

;; The scaling factor used when drawing the analogue clock.
(setq doom-modeline-time-clock-size 0.7)

;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
(setq doom-modeline-unicode-fallback nil)

;; Whether display the buffer name.
(setq doom-modeline-buffer-name t)

;; Whether highlight the modified buffer name.
(setq doom-modeline-highlight-modified-buffer-name t)

;; When non-nil, mode line displays column numbers zero-based.
;; See `column-number-indicator-zero-based'.
(setq doom-modeline-column-zero-based t)

;; Specification of \"percentage offset\" of window through buffer.
;; See `mode-line-percent-position'.
(setq doom-modeline-percent-position '(-3 "%p"))

;; Format used to display line numbers in the mode line.
;; See `mode-line-position-line-format'.
(setq doom-modeline-position-line-format '("L%l"))

;; Format used to display column numbers in the mode line.
;; See `mode-line-position-column-format'.
(setq doom-modeline-position-column-format '("C%c"))

;; Format used to display combined line/column numbers in the mode line. See `mode-line-position-column-line-format'.
(setq doom-modeline-position-column-line-format '("%l:%c"))

;; Whether display the minor modes in the mode-line.
(setq doom-modeline-minor-modes nil)

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)

;; Major modes in which to display word count continuously.
;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;; remove the modes from `doom-modeline-continuous-word-count-modes'.
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

;; Whether display the buffer encoding.
(setq doom-modeline-buffer-encoding t)

;; Whether display the indentation information.
(setq doom-modeline-indent-info nil)

;; Whether display the total line number。
(setq doom-modeline-total-line-number nil)

;; Whether display the icon of vcs segment. It respects option `doom-modeline-icon'."
(setq doom-modeline-vcs-icon t)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 15)

;; The function to display the branch name.
(setq doom-modeline-vcs-display-function #'doom-modeline-vcs-name)

;; Alist mapping VCS states to their corresponding faces.
;; See `vc-state' for possible values of the state.
;; For states not explicitly listed, the `doom-modeline-vcs-default' face is used.
(setq doom-modeline-vcs-state-faces-alist
      '((needs-update . (doom-modeline-warning bold))
        (removed . (doom-modeline-urgent bold))
        (conflict . (doom-modeline-urgent bold))
        (unregistered . (doom-modeline-urgent bold))))

;; Whether display the icon of check segment. It respects option `doom-modeline-icon'.
(setq doom-modeline-check-icon t)

;; If non-nil, only display one number for check information if applicable.
(setq doom-modeline-check-simple-format nil)

;; The maximum number displayed for notifications.
(setq doom-modeline-number-limit 99)

;; Whether display the project name. Non-nil to display in the mode-line.
(setq doom-modeline-project-name t)

;; Whether display the workspace name. Non-nil to display in the mode-line.
(setq doom-modeline-workspace-name t)

;; Whether display the perspective name. Non-nil to display in the mode-line.
(setq doom-modeline-persp-name t)

;; If non nil the default perspective name is displayed in the mode-line.
(setq doom-modeline-display-default-persp-name nil)

;; If non nil the perspective name is displayed alongside a folder icon.
(setq doom-modeline-persp-icon t)

;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(setq doom-modeline-lsp t)

;; Whether display the GitHub notifications. It requires `ghub' package.
(setq doom-modeline-github nil)

;; The interval of checking GitHub.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display the modal state.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal t)

;; Whether display the modal state icon.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal-icon t)

;; Whether display the modern icons for modals.
(setq doom-modeline-modal-modern-icon t)

;; When non-nil, always show the register name when recording an evil macro.
(setq doom-modeline-always-show-macro-register nil)

;; ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
;; (setq doom-modeline-mu4e nil)
;; ;; also enable the start of mu4e-alert
;; (mu4e-alert-enable-mode-line-display)

;; ;; Whet her display the gnus notifications.
(setq doom-modeline-gnus t)

;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
(setq doom-modeline-gnus-timer 2)

;; Wheter groups should be excludede when gnus automatically being updated.
(setq doom-modeline-gnus-excluded-groups '("dummy.group"))

;; Whether display the IRC notifications. It requires `circe' or `erc' package.
(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)

;; Whether display the battery status. It respects `display-battery-mode'.
(setq doom-modeline-battery t)

;; Whether display the time. It respects `display-time-mode'.
(setq doom-modeline-time t)

;; Whether display the misc segment on all mode lines.
;; If nil, display only if the mode line is active.
(setq doom-modeline-display-misc-in-all-mode-lines t)

;; The function to handle `buffer-file-name'.
(setq doom-modeline-buffer-file-name-function #'identity)

;; The function to handle `buffer-file-truename'.
(setq doom-modeline-buffer-file-truename-function #'identity)

;; Whether display the environment version.
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; What to display as the version while a new one is being loaded
(setq doom-modeline-env-load-string "...")

;; By default, almost all segments are displayed only in the active window. To
;; display such segments in all windows, specify e.g.
(setq doom-modeline-always-visible-segments '(mu4e irc))

;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)

(use-package desktop
  :custom
  (desktop-restore-eager   1 "Restore the first buffer right away")
  (desktop-lazy-idle-delay 1 "Restore the other buffers 1 second later")
  (desktop-lazy-verbose  nil "Be silent about lazily opening buffers")
  :bind
  ("C-M-s-k" . desktop-clear)
  :config
  (desktop-save-mode))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-strip-common-suffix t))

(use-package hl-line
  :disabled
  :config
  (defun zz/get-visual-line-range ()
    (let (b e)
      (save-excursion
        (beginning-of-visual-line)
        (setq b (point))
        (end-of-visual-line)
        (setq e (+ 1 (point)))
        )
      (cons b e)))
  (setq hl-line-range-function #'zz/get-visual-line-range)
  (global-hl-line-mode))

(use-package col-highlight
  :disabled
  :config
  (col-highlight-toggle-when-idle)
  (col-highlight-set-interval 2))

(use-package crosshairs
  :disabled
  :config
  (crosshairs-mode))

(use-package recentf
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :init
  (recentf-mode))

(use-package ibuffer
  :disabled
  :bind
  ("C-x C-b" . ibuffer))

(use-package smex
  :disabled
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package midnight
  :config
  (setq midnight-period 7200)
  (midnight-mode 1))

(use-package writeroom-mode
  :ensure t)

(use-package neotree
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-smart-open t)
  (projectile-switch-project-action 'neotree-projectile-action)
  :config
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  :bind
  ([f8] . neotree-project-dir))

(use-package wc-mode
  :hook
  (org-journal-mode . wc-mode))

(use-package all-the-icons)

(use-package ido
  :disabled
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-use-virtual-buffers t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1))

(use-package ido-completing-read+
  :disabled
  :config
  (ido-ubiquitous-mode 1))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind
  (("C-x C-f"       . helm-find-files)
   ("C-x C-b"       . helm-buffers-list)
   ("C-x b"         . helm-multi-files)
   ("M-x"           . helm-M-x)
   :map helm-find-files-map
   ("C-<backspace>" . helm-find-files-up-one-level)
   ("C-f"           . helm-execute-persistent-action)
   ([tab]           . helm-ff-RET))
  :init
  (defun daedreth/helm-hide-minibuffer ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  :custom
  (helm-autoresize-max-height 0)
  (helm-autoresize-min-height 40)
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-semantic-fuzzy-match t)
  (helm-imenu-fuzzy-match t)
  (helm-split-window-in-side-p nil)
  (helm-move-to-line-cycle-in-source nil)
  (helm-ff-search-library-in-sexp t)
  (helm-scroll-amount 8)
  (helm-echo-input-in-header-line nil)
  :config
  ;;(require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  :hook
  (helm-mode .
             (lambda ()
               (setq completion-styles
                     (cond ((assq 'helm-flex completion-styles-alist)
                            '(helm-flex)) ;; emacs-26
                           ((assq 'flex completion-styles-alist)
                            '(flex))))))  ;; emacs-27+
  (helm-minibuffer-set-up . daedreth/helm-hide-minibuffer))

(use-package helm-flx
  :ensure t
  :custom
  (helm-flx-for-helm-find-files t)
  (helm-flx-for-helm-locate t)
  :config
  (helm-flx-mode +1))

(use-package swiper-helm
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package helm-lsp)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-c C-j" . direc-jump)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions '(("png" . "loupe")
                                 ("mkv" . "vlc")
                                 ("mp4" . "vlc"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map "H" dired-hide-dotfiles-mode))

(use-package subword
  :hook
  (prog-mode . subword-mode))

(use-package aggressive-indent
  :ensure t
  :disabled
  :diminish aggressive-indent-mode
  :hook
  (prog-mode . aggressive-indent-mode)
  (python-mode . (lambda () (aggressive-indent-mode -1))))

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))

(use-package iedit
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("C-;" . iedit-mode))

(use-package eldoc
  :diminish
  :hook
  (prog-mode       . turn-on-eldoc-mode)
  (cider-repl-mode . turn-on-eldoc-mode)
	(slime-mode      . turn-on-eldoc-mode))

(use-package flyspell
  :diminish)

(use-package realgud
  :ensure t)

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

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

(use-package lsp-mode
   :commands (lsp lsp-deferred)
   :bind
   (:map lsp-mode-map
              (("\C-\M-b" . lsp-find-implementation)
               ("M-RET" . lsp-execute-code-action)))
   :init
   (setq lsp-keymap-prefix (kbd "C-c l"))
   :config
   (lsp-enable-which-key-integration t))

 (use-package lsp-ui)
 (use-package lsp-treemacs)

(use-package yasnippet :config (yas-global-mode))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

(use-package asm-mode
  :mode ("\\.asm\\'" "\\.s\\'" "\\.S\\'"))

(add-hook 'asm-mode-hook
  (lambda ()
            (setq tab-width 8 ;; or whatever asm wants
                  indent-tabs-mode t)))

(use-package cc-mode
  :ensure t
  :config
  (setq c-default-style "k&r"
	c-basic-offset 4))

(use-package c++-mode
  :ensure nil
  :mode ("\\.cpp\\'" "\\.hpp\\'" "\\.cc\\'" "\\.hh\\'"))

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" "\\.mod\\'")
  :hook ((before-save . gofmt-before-save)))

;; (set-indent 'go-mode 4 t) ;; Go prefers tabs

(setq gofmt-command "goimports")
(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(setq-default eglot-workspace-configuration
  '((:gopls .
      ((staticcheck . t)
       (matcher . "CaseSensitive")))))

(add-hook 'before-save-hook
  (lambda ()
      (call-interactively 'eglot-code-action-organize-imports))
  nil t)

(use-package python
  :ensure nil
	:custom
	(python-shell-interpreter "python3"))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

;; (set-indent 'python-mode 4 0)

;;;; Function Signature and auto-import
(use-package jedi
  :hook (python-mode . jedi:setup))

(use-package java-mode
  :ensure nil
  :mode "\\.java\\'")

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp))

(use-package dap-java
  :ensure nil
  :after (lsp-java)
  :config
  (global-set-key (kbd "<f7>") 'dap-step-in)
  (global-set-key (kbd "<f8>") 'dap-next)
  (global-set-key (kbd "<f9>") 'dap-continue))

(setq lsp-java-server-install-dir "~/jdt-language-server-1.47.0-202505151856/")


(defun java-configuration ()
  (auto-fill-mode)
  (flycheck-mode)
  (git-gutter-mode)
  (subword-mode)
  (yas-minor-mode)
  (set-fringe-style '(8 . 0))
  (define-key c-mode-base-map (kbd "C-M-j") 'tkj-insert-serial-version-uuid)
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arugments on next line as indented body
  (c-set-offset 'arglist-intro '++))

(add-hook 'java-mode-hook 'java-configuration)

(setq lsp-java-content-provider-preferred "fernflower")

;; (require 'lsp-java-boot)

;; ;; to enable the lenses
;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

;; (set-indent 'lua-mode 4 0)

(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'")

;; (set-indent 'zig-mode 4 0)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; (set-indent 'json-mode 2 t)

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :hook
	(markdown-mode . visual-line-mode)
	(markdown-mode . variable-pitch-mode))

;; (set-indent 'markdown-mode 2 0)

(use-package sh-script
  :ensure nil
  :mode ("\\.sh\\'" . sh-mode))

;; (set-indent 'sh-mode 2 0)

(use-package powershell
  :ensure t
  :mode ("\\.ps1\\'" . powershell-mode))

;; (set-indent 'powershell-mode 4 0)

(use-package julia-mode
	:mode ("\\.jl\\'" . julia-mode)
	:config
	(define-key julia-mode-map (kbd "TAB") 'julia-latexsub-or-indent))

(use-package julia-snail
  :ensure t
  :hook
  (julia-mode . julia-snail-mode))

(add-to-list 'display-buffer-alist
             '("\\*julia" (display-buffer-reuse-window display-buffer-same-window)))

(defun lisp-common-setup ()
  (electric-pair-mode -1)
  (whitespace-mode -1)
  (setq tab-width 2
        lisp-indent-offset 2
        indent-tabs-mode 0))

;;;; Disable electric-indent mode for Lisp
(dolist (hook '( org-mode
		 clojure-mode
		 emacs-lisp-mode
		 lisp-mode
		 cider-repl-mode
		 racket-mode
		 racket-repl-mode
		 slime-mode
                 lisp-interaction-mode
                 scheme-mode
		 slime-repl-mode))
  (add-hook hook #'lisp-common-setup))

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

(use-package racket-mode
  :ensure t
)

(use-package clojure-mode
  :mode "\\.clj.*$"
  :mode "riemann.config"
  :mode "\\.boot"
  :config
  (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode)))

;; (use-package clojure-mode-extra-font-locking
;;   :ensure t)

;; (use-package cider
;;     :ensure t
;;     :custom
;;     ;; nice pretty printing
;;     (cider-repl-use-pretty-printing nil)
;;     ;; nicer font lock in REPL
;;     (cider-repl-use-clojure-font-lock t)
;;     ;; result prefix for the REPL
;;     (cider-repl-result-prefix "; => ")
;;     ;; never ending REPL history
;;     (cider-repl-wrap-history t)
;;     ;; looong history
;;     (cider-repl-history-size 5000)
;;     ;; persistent history
;;     (cider-repl-history-file "~/.emacs.d/cider-history")
;;     ;; error buffer not popping up
;;     (cider-show-error-buffer nil)
;;     ;; go right to the REPL buffer when it's finished connecting
;;     (cider-repl-pop-to-buffer-on-connect t))

(use-package clj-refactor
  :ensure t
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  :hook
  (clojure-mode . my-clojure-mode-hook))

(use-package emr
  :ensure t
  :config
  (bind-key "A-RET" 'emr-show-refactor-menu prog-mode-map))

(use-package rainbow-delimiters
  :ensure t
  :hook
  ((prog-mode cider-repl-mode slime-mode org-mode) . rainbow-delimiters-mode))

(defun zz/sp-enclose-next-sexp (num)
  (interactive "p")
  (insert-parentheses (or num 1)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  :custom
  (sp-base-key-bindings 'paredit)
  :hook
  (prog-mode . smartparens-mode)
  ((org-mode
    clojure-mode
    emacs-lisp-mode
    lisp-mode
    cider-repl-mode
    racket-mode
    racket-repl-mode
    slime-mode) . smartparens-strict-mode)
  (smartparens-mode  . sp-use-paredit-bindings)
  (smartparens-mode  . (lambda ()
                         (local-set-key (kbd "M-(")
                                        'zz/sp-enclose-next-sexp))))

(use-package lispy
  :disabled
  :ensure t
  :config
  (defun enable-lispy-mode () (lispy-mode 1))
  :hook
  ((clojure-mode
    emacs-lisp-mode
    common-lisp-mode
    scheme-mode
    lisp-mode) . enable-lispy-mode))

(use-package flycheck
:ensure t
:config
(add-hook 'after-init-hook #'global-flycheck-mode))

(use-package package-lint)

(use-package cperl-mode
  :mode ("\\.p[lm]\\'" . cperl-mode)
  :interpreter "perl"
  :config
  (setq cperl-hairy t))

(use-package fish-mode
  :mode "\\.fish\\'"
  :interpreter "fish")

(use-package yaml-mode
		:mode ("\\.y[a]ml\\'" . yaml-mode))

(use-package nix-mode
		:mode ("\\.nix\\'" . nix-mode))

(use-package dockerfile-mode
  :ensure t)

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'")

(use-package cfengine
  :commands cfengine3-mode
  :mode ("\\.cf\\'" . cfengine3-mode))

(use-package package-build
  :ensure t)
(use-package package-lint
  :ensure t)

(use-package elvish-mode
  :ensure t)

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

(use-package eshell-git-prompt
  :ensure t)

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
  ;;(eshell-git-prompt-use-theme 'powerline)
  )

(require 'rx)
(use-package xr
  :ensure t)

(use-package helm-pass
  :ensure t)

(use-package magit
  :diminish auto-revert-mode
  :bind
  (("C-c C-g" . magit-status)
   :map magit-status-mode-map
   ("q"       . magit-quit-session))
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    "Make magit-status run alone in a frame."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restore the previous window configuration and kill the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package git-gutter)
(global-git-gutter-mode)

(use-package ag
  :ensure t)

(defun my-randomize-region (beg end)
  "Randomize lines in region from BEG to END."
  (interactive "*r")
  (let ((lines (split-string
                (delete-and-extract-region beg end) "\n")))
    (when (string-equal "" (car (last lines 1)))
      (setq lines (butlast lines 1)))
    (apply 'insert
           (mapcar 'cdr
                   (sort (mapcar
                          (lambda (x)
                            (cons (random) (concat x "\n")))
                          lines)
                         (lambda (a b) (< (car a) (car b))))))))

(use-package autoinsert
  :ensure nil
  :custom
  (auto-insert-directory (concat user-emacs-directory "auto-insert/"))
  :hook
  (find-file . auto-insert))

(use-package gist
  :ensure t
  :custom
  (gist-view-gist t "Automatically open new gists in browser"))

(use-package esup
  :ensure t)

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(use-package restart-emacs
  :ensure t)

(use-package multiple-cursors
  :bind
  ("C-c m c"   . mc/edit-lines)
  ("C-c m <"   . mc/mark-next-like-this)
  ("C-c m >"   . mc/mark-previous-like-this)
  ("C-c m C-<" . mc/mark-all-like-this))

(use-package lorem-ipsum
  :ensure t)

(use-package keybase
  :disabled
  :ensure nil
  :load-path ("lisp/keybase-chat")
  :config (require 'keybase))

(use-package erc
  :disabled
  :custom
  ;;(erc-autojoin-channels-alist '(("freenode.net" "#elvish" "#hammerspoon"
  ;;                                "#org-mode")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-nick "zzamboni")
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules))

(use-package adoc-mode
  :mode "\\.asciidoc\\'"
  :hook
  (adoc-mode . visual-line-mode)
  (adoc-mode . variable-pitch-mode))

;; (use-package typopunct
;;   :ensure t
;;   :hook (text-mode . typopunct-mode)
;;   :config
;;   (setq typopunct-quote-style 'english)
;;   (setq typopunct-contexts '((typopunct-guess-context))))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

(use-package org
  ;;    :pin manual
  :load-path ("lisp/org-mode/lisp" "lisp/org-mode/lisp/contrib/lisp")
  :bind
  (:map org-mode-map
   ("C-c l" . org-store-link)
   ("A-h" . org-mark-element)
   ("C-a" . org-beginning-of-line)
   ("C-e" . org-end-of-line)
   ("C-k" . org-kill-line))
  :custom
  (org-directory "~/org")
  (org-log-done t)
  (org-startup-indented t)
  (org-log-into-drawer t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-use-speed-commands
   (lambda ()
     (and (looking-at org-outline-regexp)
       (looking-back "^\**"))))
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-hide-emphasis-markers t)
  (prettify-symbols-unprettify-at-point 'right-edge)
  (org-fontify-done-headline t)
  (org-tags-column 0)
  (org-todo-keyword-faces
   '(("AREA"         . "DarkOrchid1")
     ("[AREA]"       . "DarkOrchid1")
     ("PROJECT"      . "DarkOrchid1")
     ("[PROJECT]"    . "DarkOrchid1")
     ("INBOX"        . "cyan")
     ("[INBOX]"      . "cyan")
     ("PROPOSAL"     . "orange")
     ("[PROPOSAL]"   . "orange")
     ("DRAFT"        . "yellow3")
     ("[DRAFT]"      . "yellow3")
     ("INPROGRESS"   . "yellow4")
  		("[INPROGRESS]" . "yellow4")
     ("MEETING"      . "purple")
     ("[MEETING]"    . "purple")
     ("CANCELED"     . "blue")
     ("[CANCELED]"   . "blue")))
  :custom-face
  ;;    (fixed-pitch ((t (:family "Inconsolata Nerd Font"))))
  (fixed-pitch ((t (:family "CaskaydiaCove NF" :height 100))))
  (variable-pitch ((t (:family "CaskaydiaCove NF" :height 100))))
  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-done ((t (:foreground "PaleGreen"
  			   :strike-through t))))
  :hook
  (org-mode . (lambda () (add-hook 'after-save-hook 'org-babel-tangle :append :local)))
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . (lambda ()
                "Beautify Org Checkbox Symbol"
               ;; (push '("[ ]" . "☐" ) prettify-symbols-alist)
               ;; (push '("[X]" . "☑" ) prettify-symbols-alist)
               ;; (push '("[-]" . "⊡" ) prettify-symbols-alist)
                (prettify-symbols-mode)))
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     ;; (cfengine3 . t)
     (ruby      . t)
     (latex     . t)
     (plantuml  . t)
     (python    . t)
     (shell     . t)
     ;;(elvish    . t)
     (calc      . t)
     (dot       . t)
     (ditaa     . t)
     (org       . t)
     (lua       . t)
     ;;(powershell . t)
     ))
  (font-lock-add-keywords
  	'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")
  
  (font-lock-add-keywords
   'org-mode
  	`(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)
  
  (let* ((variable-tuple
           (cond ((x-list-fonts   "ETBembo")         '(:font   "ETBembo"))
                 ((x-list-fonts   "Source Sans Pro") '(:font   "Source Sans Pro"))
                 ((x-list-fonts   "Lucida Grande")   '(:font   "Lucida Grande"))
                 ((x-list-fonts   "Verdana")         '(:font   "Verdana"))
                 ((x-family-fonts "Sans Serif")      '(:family "Sans Serif"))
                 (nil (warn "Cannot find a Sans Serif Font."))))
          (base-font-color (face-foreground 'default nil 'default))
          (headline `(:inherit default :weight bold
                      :foreground ,base-font-color))))
  ;;(eval-after-load 'face-remap '(diminish 'buffer-face-mode))
  ;;(eval-after-load 'simple '(diminish 'visual-line-mode)))

(use-package org-indent
  :ensure nil
  :diminish
  :custom
  (org-indent-indentation-per-level 4))

(defun zz/add-file-keybinding (key file &optional desc)
  (lexical-let ((key key)
                (file file)
                (desc desc))
    (global-set-key (kbd key) (lambda () (interactive) (find-file file)))
    (which-key-add-key-based-replacements key (or desc file))))

(custom-set-variables '(org-agenda-files
                        '("~/gtd" "~/Work/work.org.gpg" "~/org/ideas.org" "~/org/projects.org" "~/org/diary.org")))
(zz/add-file-keybinding "C-c f w" "~/Work/work.org.gpg" "work.org")
(zz/add-file-keybinding "C-c f i" "~/org/ideas.org" "ideas.org")
(zz/add-file-keybinding "C-c f p" "~/org/projects.org" "projects.org")
(zz/add-file-keybinding "C-c f d" "~/org/diary.org" "diary.org")

(use-package org-capture
  :ensure nil
  :after org
  :bind
  ("C-c c" . org-capture)
  :config
  (add-to-list 'org-capture-templates
               '("i" "GTD item"
                 entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                 "* %?\n%U\n\n  %i"
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("l" "GTD item with link to where you are in emacs now"
                 entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                 "* %?\n%U\n\n  %i\n  %a"
  								:kill-buffer t))
  
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
    See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: "))
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
                   ":EXPORT_FILE_NAME: index"
                   ":END:"
  									"%?\n") ; Place the cursor here finally
                 "\n")))
  (add-to-list 'org-capture-templates
               '("z"       ;`org-capture' binding + z
                 "zzamboni.org post"
                 entry
                 (file+olp "~/Personal/websites/zzamboni.org/content-org/zzamboni.org" "Ideas")
                 (function org-hugo-new-subtree-post-capture-template)))
  )

(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

(use-package org-agenda
  :ensure nil
  :after org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-agenda-include-diary t)
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                              ;; Indent todo items by level to show nesting
                              (todo . " %i %-12:c%l")
                               (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-start-on-weekday nil))

(use-package holidays
  :ensure nil
  :init
  :config
  (setq calendar-holidays holiday-islamic-holidays)))

;; (require 'org-habit)
;; (use-package org-super-agenda
;;   :ensure t
;;   :custom
;;   (org-super-agenda-groups '((:auto-dir-name t)))
;;   :config
;;   (org-super-agenda-mode))

(use-package org-archive
  :ensure nil
  :custom
  (org-archive-location "archive.org::datetree/"))

(use-package org-edna
  :ensure t)


(use-package org-gtd
  :after org
  :config
  (require 'org-gtd)
  ;; these are the interactive functions you're likely to want to use as you go about GTD.
  (global-set-key (kbd "C-c d c") 'org-gtd-capture) ;; add item to inbox
  (global-set-key (kbd "C-c d p") 'org-gtd-process-inbox) ;; process entire inbox
  (global-set-key (kbd "C-c d a") 'org-agenda-list) ;; see what's on your plate today
  (global-set-key (kbd "C-c d n") 'org-gtd-show-all-next) ;; see all NEXT items
  (global-set-key (kbd "C-c d s") 'org-gtd-show-stuck-projects) ;; see projects that don't have a NEXT item

  ;; package: https://www.nongnu.org/org-edna-el/
  ;; org-edna is used to make sure that when a project task gets DONE,
  ;; the next TODO is automatically changed to NEXT.
  (setq org-edna-use-inheritance t)
  (org-edna-load)

  ;; package: https://github.com/Malabarba/org-agenda-property
  ;; this is so you can see who an item was delegated to in the agenda
  (setq org-agenda-property-list '("DELEGATED_TO"))
  ;; I think this makes the agenda easier to read
  (setq org-agenda-property-position 'next-line))

;; not upgrading since I don't know what it is.
(setq org-gtd-update-ack "2.1.0")

(use-package emacsql
  :ensure t)

;; (use-package emacsql-sqlite
;;   :ensure t
;;   :after emacsql)

(use-package org-roam
  :after (org emacsql emacsql-sqlite)
  :load-path "lisp/org-roam"
  :diminish
  :hook
  ((org-mode . org-roam-mode)
   (after-init . org-roam--build-cache-async))
  :custom
  (org-roam-directory "~/org")
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n i" . org-roam-insert)
  ("C-c n g" . org-roam-show-graph))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-directory org-directory)
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase)))
  (deft-org-mode-title-prefix t)
  (deft-extensions '("org" "txt" "text" "md" "markdown" "org.gpg"))
  (deft-default-extension "org"))

(use-package org-download
  :ensure t
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  :bind
  ("C-M-y" .
   (lambda (&optional noask)
     (interactive "P")
     (let ((file
             (if (not noask)
                (read-string (format "Filename [%s]: " org-download-screenshot-basename)
                             nil nil org-download-screenshot-basename)
              nil)))
       (org-download-clipboard file))))
  :config
  (require 'org-download))

(use-package ox-reveal
  :load-path ("lisp/org-reveal")
  :after org
  :custom
  (org-reveal-note-key-char nil)
  (org-reveal-root "file:///Users/taazadi1/.emacs.d/lisp/reveal.js"))
(use-package htmlize
  :after ox-reveal)

(use-package ox-html
  :ensure nil
  :after org
  :custom
  (org-html-checkbox-type 'unicode))

(use-package ox-md
  :ensure nil
  :after org)

(use-package ox-jira
  :after org)

(use-package org-jira
  :after org
  :custom
  (jiralib-url "https://jira.swisscom.com"))

(use-package ox-confluence
  :ensure nil
  :after org)

(use-package ox-asciidoc
  :after org)

(use-package ox-texinfo
  :load-path "lisp/org-mode/lisp"
  :ensure nil
  :after org)

(use-package ox-latex
  :load-path "lisp/org-mode/lisp"
  :ensure nil
  :demand
  :after org
  :custom
  (org-latex-compiler "xelatex")
  ;; (org-latex-pdf-process
  ;;  '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;    "%latex -interaction nonstopmode -output-directory %o %f"
  ;;    "%latex -interaction nonstopmode -output-directory %o %f"))
  :config
  ;; (setq org-latex-listings 'minted)
  ;; (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  ;; (add-to-list 'org-latex-minted-langs '(lua "lua"))
  ;; (add-to-list 'org-latex-minted-langs '(shell "shell"))
  (add-to-list 'org-latex-classes
               '("book-no-parts" "\\documentclass[11pt,letterpaper]{book}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
									("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  ;; Necessary for LuaLaTeX to work - see
  ;; https://tex.stackexchange.com/a/374391/10680
  (setenv "LANG" "en_US.UTF-8"))

(use-package ox-clip
  :bind
  ("A-C-M-k" . ox-clip-formatted-copy))

;;(use-package ox-awesomecv
  ;;:load-path "~/.emacs.d/lisp/org-cv"
  ;;:init
  ;;(require 'ox-awesomecv))
;;(use-package ox-hugocv
;;  :disabled
;;  :load-path "~/.emacs.d/lisp/org-cv"
;;  :init
;;  (require 'ox-hugocv))

(use-package ox-org
  :ensure nil
  :after org)

(use-package ox-hugo
  :after org
  ;; Testing hooks to automatically set the filename on an ox-hugo
  ;; blog entry when it gets marked as DONE
  ;; :hook
  ;; (org-mode . (lambda ()
  ;;               (add-hook 'org-after-todo-state-change-hook
  ;;                         (lambda ()
  ;;                           (org-set-property
  ;;                            "testprop"
  ;;                            (concat "org-state: " org-state
  ;;                                    " prev-state: " (org-get-todo-state))))
  ;;                         'run-at-end 'only-in-org-mode)))
  :custom
  (org-hugo-use-code-for-kbd t))

(use-package epa-file
  :ensure nil ;; included with Emacs
  :config
  (setq epa-file-encrypt-to '("zubairstudytech@gmail.com"))
  :custom
  (epa-file-select-keys 'silent))

(use-package org-crypt
  :ensure nil  ;; included with org-mode
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  :custom
  (org-crypt-key "zubairstudytech@gmail.com"))

(use-package org-journal
  :after org
  :custom
  (org-journal-dir (concat (file-name-as-directory org-directory) "journal"))
  (org-journal-file-format "%Y/%m/%Y%m%d")
  (org-journal-date-format "%A, %Y-%m-%d")
  (org-journal-encrypt-journal t)
  (org-journal-enable-encryption nil)
  (org-journal-enable-agenda-integration t)
  :bind
  ("C-c j" . org-journal-new-entry))

;;(use-package ob-cfengine3
;;  :after org)

;;(use-package ob-elvish
;;  :after org)

(require 'subr-x)
(setq homebrew-plantuml-jar-path
  (expand-file-name
       (string-trim
        (shell-command-to-string "brew list plantuml | grep jar"))))

(use-package plantuml-mode
  :ensure t
  :custom
  (plantuml-jar-path homebrew-plantuml-jar-path))

(use-package ob-plantuml
  :ensure nil
  :after org
  :custom
  (org-plantuml-jar-path homebrew-plantuml-jar-path))

(defalias 'console-mode 'shell-script-mode)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("lua" . "src lua"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))

(defun zz/org-babel-tangle-async (file)
  "Invoke `org-babel-tangle-file' asynchronously."
  (message "Tangling %s..." (buffer-file-name))
  (async-start
   (let ((args (list file)))
     `(lambda ()
        (require 'org)
        ;;(load "~/.emacs.d/init.el")
        (let ((start-time (current-time)))
          (apply #'org-babel-tangle-file ',args)
          (format "%.2f" (float-time (time-since start-time))))))
   (let ((message-string (format "Tangling %S completed after " file)))
     `(lambda (tangle-time)
        (message (concat ,message-string
                   (format "%s seconds" tangle-time)))))))

(defun zz/org-babel-tangle-current-buffer-async ()
  "Tangle current buffer asynchronously."
  (zz/org-babel-tangle-async (buffer-file-name)))

(use-package org-bullets
  :ensure t
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

;; (use-package nerd-icons
;;   :ensure t)

(defun zz/write ()
  (interactive)
  ;; Line spacing
  (setq line-spacing 0.1)
  ;; Top padding
  (setq header-line-format " ")
  ;; Hide modeline
  (hide-mode-line-mode)
  ;;(setq mode-line-format nil)
  ;; Side padding
  (setq left-margin-width 2)
  (setq right-margin-width 2)
  (set-window-buffer nil (current-buffer)))

(use-package hide-mode-line
  :ensure t)

(with-eval-after-load 'org
  (defvar-local rasmus/org-at-src-begin -1
    "Variable that holds whether last position was a ")

  (defvar rasmus/ob-header-symbol ?☰
    "Symbol used for babel headers")

  (defun rasmus/org-prettify-src--update ()
    (let ((case-fold-search t)
           (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
           found)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (goto-char (match-end 0))
          (let ((args (org-trim
												(buffer-substring-no-properties (point)
                          (line-end-position)))))
            (when (org-string-nw-p args)
              (let ((new-cell (cons args rasmus/ob-header-symbol)))
                (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
                (cl-pushnew new-cell found :test #'equal)))))
        (setq prettify-symbols-alist
          (cl-set-difference prettify-symbols-alist
            (cl-set-difference
              (cl-remove-if-not
                (lambda (elm)
                  (eq (cdr elm) rasmus/ob-header-symbol))
                prettify-symbols-alist)
              found :test #'equal)))
        ;; Clean up old font-lock-keywords.
        (font-lock-remove-keywords nil prettify-symbols--keywords)
        (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
        (font-lock-add-keywords nil prettify-symbols--keywords)
        (while (re-search-forward re nil t)
          (font-lock-flush (line-beginning-position) (line-end-position))))))

  (defun rasmus/org-prettify-src ()
    "Hide src options via `prettify-symbols-mode'.

        `prettify-symbols-mode' is used because it has uncollpasing. It's
        may not be efficient."
    (let* ((case-fold-search t)
						(at-src-block (save-excursion
														(beginning-of-line)
														(looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
      ;; Test if we moved out of a block.
      (when (or (and rasmus/org-at-src-begin
                  (not at-src-block))
              ;; File was just opened.
              (eq rasmus/org-at-src-begin -1))
        (rasmus/org-prettify-src--update))
      ;; Remove composition if at line; doesn't work properly.
      ;; (when at-src-block
      ;;   (with-silent-modifications
      ;;     (remove-text-properties (match-end 0)
      ;;                             (1+ (line-end-position))
      ;;                             '(composition))))
      (setq rasmus/org-at-src-begin at-src-block)))

  ;; This function helps to produce a single glyph out of a
  ;; string. The glyph can then be used in prettify-symbols-alist.
  ;; This function was provided by Ihor in the org-mode mailing list.
  (defun yant/str-to-glyph (str)
    "Transform string into glyph, displayed correctly."
    (let ((composition nil))
      (dolist (char (string-to-list str)
                (nreverse (cdr composition)))
        (push char composition)
        (push '(Br . Bl) composition))))

  (defun rasmus/org-prettify-symbols ()
    (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
      (cl-reduce 'append
        (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
          `(("#+begin_src" . ?⎡) ;; ⎡ ➤ 🖝 ➟ ➤ ✎
             ;; multi-character strings can be used with something like this:
             ;; ("#+begin_src" . ,(yant/str-to-glyph "```"))
             ("#+end_src"   . ?⎣) ;; ⎣ ✐
             ("#+header:" . ,rasmus/ob-header-symbol)
             ("#+begin_quote" . ?«)
             ("#+end_quote" . ?»)))))
    (turn-on-prettify-symbols-mode)
    (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))
  (add-hook 'org-mode-hook #'rasmus/org-prettify-symbols))

(use-package toc-org
  :after org
  :hook
  (org-mode . toc-org-enable))

(use-package org-mac-link
  :ensure nil
  :load-path "lisp/org-mode/contrib/lisp"
  :after org
  :custom
  (org-mac-grab-Acrobat-app-p nil "Disable grabbing from Adobe Acrobat")
  (org-mac-grab-devonthink-app-p nil "Disable grabbinb from DevonThink")
  :bind
  (:map org-mode-map
   ("C-c g" . org-mac-grab-link)))

(defun zz/org-reformat-buffer ()
  (interactive)
  (when (y-or-n-p "Really format current buffer? ")
    (let ((document (org-element-interpret-data (org-element-parse-buffer))))
      (erase-buffer)
      (insert document)
      (goto-char (point-min)))))

(defun afs/org-remove-link ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((remove (list (match-beginning 0) (match-end 0)))
            (description (if (match-end 3)
                             (org-match-string-no-properties 3)
                           (org-match-string-no-properties 1))))
        (apply 'delete-region remove)
        (insert description))))
(bind-key "C-c C-M-u" 'afs/org-remove-link)

(defun zz/org-if-str (str &optional desc)
  (when (org-string-nw-p str)
    (or (org-string-nw-p desc) str)))

(defun zz/org-macro-hsapi-code (module &optional func desc)
  (org-link-make-string
   (concat "https://www.hammerspoon.org/docs/"
           (concat module (zz/org-if-str func (concat "#" func))))
   (or (org-string-nw-p desc)
     (format "=%s="
               (concat module
                       (zz/org-if-str func (concat "." func)))))))

(defun zz/org-macro-keys-code-outer (str)
  (mapconcat (lambda (s)
               (concat "~" s "~"))
             (split-string str)
             (concat (string ?\u200B) "+" (string ?\u200B))))
(defun zz/org-macro-keys-code-inner (str)
  (concat "~" (mapconcat (lambda (s)
                           (concat s))
                         (split-string str)
                         (concat (string ?\u200B) "-" (string ?\u200B)))
          "~"))
(defun zz/org-macro-keys-code (str)
  (zz/org-macro-keys-code-inner str))

(defun zz/org-macro-luadoc-code (func &optional section desc)
  (org-link-make-string
   (concat "https://www.lua.org/manual/5.3/manual.html#"
     (zz/org-if-str func section))
   (zz/org-if-str func desc)))

(defun zz/org-macro-luafun-code (func &optional desc)
  (org-link-make-string
		(concat "https://www.lua.org/manual/5.3/manual.html#"
           (concat "pdf-" func))
   (zz/org-if-str (concat "=" func "()=") desc)))

(defun org-latex-publish-to-latex-and-open (plist file pub-dir)
  (org-open-file (org-latex-publish-to-pdf plist file pub-dir)))

(use-package ox-leanpub
  :after org
  :config
  (require 'ox-leanpub-markdown)
  (org-leanpub-book-setup-menu-markdown))

(defun org-get-keyword (key)
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (k)
      (when (string= key (org-element-property :key k))
        (org-element-property :value k)))
    nil t))

(use-package org-sidebar
  :ensure t)

;; Call this function with "M-x org-multi-file-md-export"
(defun org-multi-file-md-export ()
  "Export current buffer to multiple Markdown files."
  (interactive)
  ;; Loop over all entries in the file
  (org-map-entries
   (lambda ()
     (let* ((level (nth 1 (org-heading-components)))
            (title (or (nth 4 (org-heading-components)) ""))
            ;; Export filename is the EXPORT_FILE_NAME property, or the
            ;; lower-cased section title if it's not set.
            (filename
							(or (org-entry-get (point) "EXPORT_FILE_NAME")
                 (concat (replace-regexp-in-string " " "-" (downcase title)) ".md"))))
       (when (= level 1) ;; export only first level entries
         ;; Mark the subtree so that the title also gets exported
         (org-mark-subtree)
         ;; Call the export function. This is one of the base org
         ;; functions, the 'md defines the backend to use for the
         ;; conversion. For exporting to other formats, simply use the
         ;; correct backend name, and also change the file extension
         ;; above.
         (org-export-to-file 'md filename nil t nil))))
   ;; skip headlines tagged with "noexport" (this is an argument to
   ;; org-map-entries above)
   "-noexport")
  nil nil)

(setq gc-cons-threshold (* 2 1000 1000))
