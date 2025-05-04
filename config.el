(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; (use-package use-package-ensure-system-package :ensure t)

(use-package delight)

(use-package auth-source
  :ensure nil
  :custom
  (auth-sources '("~/.config/gnupg/shared/authinfo.gpg"
                  "~/.authinfo.gpg"
                  "~/.authinfo"
                  "~/.netrc")))

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 display-time-default-load-average nil            ; Don't display load average
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Prefer spaces over tabs
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 kill-ring-max 128                                ; Maximum length of kill ring
 load-prefer-newer t                              ; Prefer the newest version of a file
 mark-ring-max 128                                ; Maximum length of mark ring
 read-process-output-max (* 1024 1024)            ; Increase the amount of data reads from the process
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 tab-width 4                                      ; Set width for tabs
 use-package-always-ensure t                      ; Avoid the :ensure keyword for each package
 user-full-name "ZooBaer404"                      ; Set the full name of the current user
 user-mail-address "zubairstudytech@gmail.com"    ; Set the email address of the current user
 vc-follow-symlinks t                             ; Always follow the symlinks
 view-read-only t)                                ; Always open read-only buffers in view-mode
(column-number-mode 1)                            ; Show the column number
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-hl-line-mode)                             ; Hightlight current line
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show the parent


(defvar xdg-bin (getenv "XDG_BIN_HOME")
"The XDG bin base directory.")

(defvar xdg-cache (getenv "XDG_CACHE_HOME")
"The XDG cache base directory.")

(defvar xdg-config (getenv "XDG_CONFIG_HOME")
"The XDG config base directory.")

(defvar xdg-data (getenv "XDG_DATA_HOME")
"The XDG data base directory.")

(defvar xdg-lib (getenv "XDG_LIB_HOME")
  "The XDG lib base directory.")



;; configurations
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq warning-minimum-level :emergency)



(set-face-attribute 'default nil :font "CaskaydiaCove NF")
(set-fontset-font t 'latin "CaskaydiaCove NF")

(use-package vterm
  :ensure t)


;; (use-package dap-mode
;;   :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t))


;; (setq lsp-completion-provider :none)
;; (setq lsp-enable-snippet 'f)
;; (setq lsp-completion-enable 'f)


(use-package doom-themes
  :config
  (load-theme 'doom-material-dark t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode)
  :custom
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-mu4e t))

(use-package solaire-mode
  :defer 0.1
  :custom (solaire-mode-remap-fringe t)
  :config (solaire-global-mode))

(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode 1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

(use-package async
  :after org
  :preface
  (defvar config-file (expand-file-name "config.el" user-emacs-directory)
    "The configuration file.")

  (defvar config-last-change (nth 5 (file-attributes config-file))
    "The last modification time of the configuration file.")

  (defvar show-async-tangle-results nil
    "Keep *emacs* async buffers around for later inspection.")

  (defun my/config-tangle ()
    "Tangle the org file asynchronously."
    (when (my/config-updated)
      (setq config-last-change
            (nth 5 (file-attributes config-file)))
      (my/async-babel-tangle config-file)))

  (defun my/config-updated ()
    "Check if the configuration file has been updated since the last time."
    (time-less-p config-last-change
                 (nth 5 (file-attributes config-file))))

  (defun my/async-babel-tangle (org-file)
    "Tangle the org file asynchronously."
    (let ((init-tangle-start-time (current-time))
          (file (buffer-file-name))
          (async-quiet-switch "-q"))
      (async-start
       `(lambda ()
          (require 'org)

          (org-babel-tangle-file ,org-file))
       (unless show-async-tangle-results
         `(lambda (result)
            (if result
                (message "[✓] %s successfully tangled (%.2fs)"
                         ,org-file
                         (float-time (time-subtract (current-time)
                                                    ',init-tangle-start-time)))
              (message "[✗] %s as tangle failed." ,org-file))))))))

(use-package files
  :ensure nil
  :preface
  (defvar afilename-cmd
    `((,(format "%s/X11/Xresources" xdg-config) . ,(format "xrdb -merge %s/X11/Xresources" xdg-config))
      (,(format "%s/xbindkeysrc" (getenv "HOME")) . "xbindkeys -p"))
    "File association list with their respective command.")

  (defun my/cmd-after-saved-file ()
    "Execute a command after saved a specific file."
    (let* ((match (assoc (buffer-file-name) afilename-cmd)))
      (when match
        (shell-command (cdr match)))))
  :hook (after-save . my/cmd-after-saved-file)
  :init
  ;; Create the "~/.cache/emacs/auto-save" folder if it does not exist.
  (let ((auto-save-folder (expand-file-name
                           (file-name-as-directory
                            (expand-file-name (format "%s/emacs/auto-save/" xdg-cache))))))
    (unless (file-exists-p (locate-user-emacs-file auto-save-folder))
      (make-directory (locate-user-emacs-file auto-save-folder))))
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name (format "%s/emacs/auto-save/" xdg-cache) t))))
  (backup-directory-alist
   `(("." . ,(expand-file-name (format "%s/emacs/backups/" xdg-data)))))
  (delete-old-versions t)
  (vc-make-backup-files t)
  (version-control t))

(use-package ibuffer
  :ensure nil
  :preface
  (defvar protected-buffers '("*scratch*" "*Messages*")
    "Buffer that cannot be killed.")

  (defun my/protected-buffers ()
    "Protect some buffers from being killed."
    (dolist (buffer protected-buffers)
      (with-current-buffer buffer
        (emacs-lock-mode 'kill))))
  :bind ("C-x C-b" . ibuffer)
  :init (my/protected-buffers))

(use-package imenu
  :ensure nil
  :preface
  (defun my/smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character and
   the beginning of the line.

   If ARG is not nil or 1, move forward ARG - 1 lines first. If
   point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))
  :bind (("C-a" . my/smarter-move-beginning-of-line)
         ("C-r" . imenu)))

(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config (move-text-default-bindings))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (:map dired-mode-map
              ("h" . dired-up-directory)
              ("j" . dired-next-line)
              ("k" . dired-previous-line)
              ("l" . dired-single-buffer))
  :delight "Dired"
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh --group-directories-first")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always))

(use-package dired-open
  :after (dired dired-jump)
  :custom (dired-open-extensions '(("mp4" . "mpv"))))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)))

(use-package dired-narrow
  :ensure nil
  :bind (("C-c C-n" . dired-narrow)
         ("C-c C-f" . dired-narrow-fuzzy)))

(use-package window
  :ensure nil
  :bind (("C-x 2" . vsplit-last-buffer)
         ("C-x 3" . hsplit-last-buffer)
         ;; Don't ask before killing a buffer.
         ([remap kill-buffer] . kill-this-buffer))
  :preface
  (defun hsplit-last-buffer ()
    "Focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun vsplit-last-buffer ()
    "Focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1)))

(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x w" . switch-window-then-swap-buffer)))

(use-package winner
  :ensure nil
  :config (winner-mode))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init (vertico-mode)
  :bind (:map vertico-map
              ("C-<backspace>" . vertico-directory-up))
  :custom (vertico-cycle t)
  :custom-face (vertico-current ((t (:background "#1d1f21")))))

(use-package marginalia
  :after vertico
  :init (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package orderless
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-styles '(orderless)))

(use-package consult
  :after projectile
  :bind  (;; Related to the control commands.
          ("<help> a" . consult-apropos)
          ("C-x b" . consult-buffer)
          ("C-x M-:" . consult-complex-command)
          ("C-c k" . consult-kmacro)
          ;; Related to the navigation.
          ("M-g a" . consult-org-agenda)
          ("M-g e" . consult-error)
          ("M-g g" . consult-goto-line)
          ("M-g h" . consult-org-heading)
          ("M-g i" . consult-imenu)
          ("M-g k" . consult-global-mark)
          ("M-g l" . consult-line)
          ("M-g m" . consult-mark)
          ("M-g o" . consult-outline)
          ("M-g I" . consult-project-imenu)
          ;; Related to the search and selection.
          ("M-s G" . consult-git-grep)
          ("M-s g" . consult-grep)
          ("M-s k" . consult-keep-lines)
          ("M-s l" . consult-locate)
          ("M-s m" . consult-multi-occur)
          ("M-s r" . consult-ripgrep)
          ("M-s u" . consult-focus-lines)
          ("M-s f" . consult-find))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (consult-narrow-key "<")
  (consult-project-root-function #'projectile-project-root)
  ;; Provides consistent display for both `consult-register' and the register
  ;; preview when editing registers.
  (register-preview-delay 0)
  (register-preview-function #'consult-register-preview))

(use-package embark
  :bind ("C-." . embark-act))

(use-package abbrev
  :ensure nil
  :delight
  :hook (text-mode . abbrev-mode)
  :custom (abbrev-file-name (expand-file-name (format "%s/emacs/abbrev_defs" xdg-data)))
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package flyspell
  :ensure nil
  :delight
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  ;; Add correction to abbreviation table.
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_US")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))

(use-package ispell
  :preface
  (defun my/switch-language ()
    "Switch between the English and French for ispell, flyspell, and LanguageTool."
    (interactive)
    (let* ((current-dictionary ispell-current-dictionary)
           (new-dictionary (if (string= current-dictionary "en_US") "fr_BE" "en_US")))
      (ispell-change-dictionary new-dictionary)
      (flyspell-buffer)
      (message "[✓] Dictionary switched to %s" new-dictionary)))
  :custom
  (ispell-hunspell-dict-paths-alist
   '(("en_US" "/usr/share/hunspell/en_US.aff")
     ("fr_BE" "/usr/share/hunspell/fr_BE.aff")))
  ;; Save words in the personal dictionary without asking.
  (ispell-silently-savep t)
  :config
  (setenv "LANG" "en_US")
  (cond ((executable-find "hunspell")
         (setq ispell-program-name "hunspell")
         (setq ispell-local-dictionary-alist '(("en_US"
                                                "[[:alpha:]]"
                                                "[^[:alpha:]]"
                                                "['’-]"
                                                t
                                                ("-d" "en_US" )
                                                nil
                                                utf-8)
                                               ("fr_BE"
                                                "[[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
                                                "[^[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
                                                "['’-]"
                                                t
                                                ("-d" "fr_BE")
                                                nil
                                                utf-8))))
        ((executable-find "aspell")
         (setq ispell-program-name "aspell")
         (setq ispell-extra-args '("--sug-mode=ultra"))))
  ;; Ignore file sections for spell checking.
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_align" . "#\\+end_align"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_align*" . "#\\+end_align*"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_equation" . "#\\+end_equation"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_equation*" . "#\\+end_equation*"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_labeling" . "#\\+end_labeling"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("\\$" . "\\$"))
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")))

(use-package sh-script
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package cc-mode)

(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-ide
  :after projectile
  :init (cmake-ide-setup)
  :hook (c++-mode . my/cmake-ide-find-project)
  :preface
  (defun my/cmake-ide-find-project ()
    "Find the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (setq cmake-ide-compile-command
          (concat "cd " cmake-ide-build-dir " && cmake .. && make"))
    (cmake-ide-load-db))

  (defun my/switch-to-compilation-window ()
    "Switch to the *compilation* buffer after compilation."
    (other-window 1))
  :bind ([remap comment-region] . cmake-ide-compile)
  :config (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window))

(use-package csv-mode :mode ("\\.\\(csv\\|tsv\\)\\'"))

(use-package dockerfile-mode :delight "δ" :mode "Dockerfile\\'")

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :custom (nov-text-width 75))

(use-package gnuplot
  :mode "\\.\\(gp\\|gpi\\|plt\\)'"
  :bind (:map gnuplot-mode-map
              ("C-c C-c".  gnuplot-send-buffer-to-gnuplot)))

(use-package ini-mode :mode "\\.ini\\'")

(use-package gradle-mode
  :hook (java-mode . gradle-mode)
  :preface
  (defun my/switch-to-compilation-window ()
    "Switch to the *compilation* buffer after compilation."
    (other-window 1))
  :bind (:map gradle-mode-map
              ("C-c C-c" . gradle-build)
              ("C-c C-t" . gradle-test))
  :config
  (advice-add 'gradle-build :after #'my/switch-to-compilation-window)
  (advice-add 'gradle-test :after #'my/switch-to-compilation-window))

(use-package js2-mode
  :ensure flycheck
  :mode "\\.js\\'"
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . prettier-js-mode))
  :custom (js-indent-level 2)
  :config (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package prettier-js
  :delight
  :custom (prettier-js-args '("--print-width" "100"
                              "--single-quote" "true"
                              "--trailing-comma" "all")))

(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode)
  :bind (:map js2-mode-map
              ("C-k" . js2r-kill)))

(use-package yarn-mode :mode "yarn\\.lock\\'")

(use-package json-mode
  :delight "J"
  :mode "\\.json\\'"
  :hook (before-save . my/json-mode-before-save-hook)
  :preface
  (defun my/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer)))

  (defun my/json-array-of-numbers-on-one-line (encode array)
    "Print the arrays of numbers in one line."
    (let* ((json-encoding-pretty-print
            (and json-encoding-pretty-print
                 (not (loop for x across array always (numberp x)))))
           (json-encoding-separator (if json-encoding-pretty-print "," ", ")))
      (funcall encode array)))
  :config (advice-add 'json-encode-array :around #'my/json-array-of-numbers-on-one-line))

(use-package tex
  :ensure auctex
  :preface
  (defun my/switch-to-help-window (&optional ARG REPARSE)
    "Switches to the *TeX Help* buffer after compilation."
    (other-window 1))
  :hook (LaTeX-mode . reftex-mode)
  :bind (:map TeX-mode-map
              ("C-c C-o" . TeX-recenter-output-buffer)
              ("C-c C-l" . TeX-next-error)
              ("M-[" . outline-previous-heading)
              ("M-]" . outline-next-heading))
  :custom
  (TeX-auto-save t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (TeX-source-correlate-mode t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  :config
  (advice-add 'TeX-next-error :after #'my/switch-to-help-window)
  (advice-add 'TeX-recenter-output-buffer :after #'my/switch-to-help-window)
  ;; the ":hook" doesn't work for this one... don't ask me why.
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer))

(setq-default TeX-engine 'xetex)

(use-package reftex
  :ensure nil
  :custom
  (reftex-save-parse-info t)
  (reftex-use-multiple-selection-buffers t))

(use-package bibtex
  :ensure nil
  :preface
  (defun my/bibtex-fill-column ()
    "Ensure that each entry does not exceed 120 characters."
    (setq fill-column 120)))

(use-package lua-mode :delight "Λ" :mode "\\.lua\\'")

(use-package markdown-mode
  :delight "μ"
  :mode ("\\.\\(md\\|markdown\\)\\'")
  :custom (markdown-command "/usr/bin/pandoc"))

(use-package markdown-preview-mode
  :commands markdown-preview-mode
  :custom
  (markdown-preview-javascript
   (list (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/highlight.min.js")
         "<script>
            $(document).on('mdContentChange', function() {
              $('pre code').each(function(i, block)  {
                hljs.highlightBlock(block);
              });
            });
          </script>"))
  (markdown-preview-stylesheets
   (list (concat "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/"
                 "3.0.1/github-markdown.min.css")
         (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/styles/github.min.css")

         "<style>
            .markdown-body {
              box-sizing: border-box;
              min-width: 200px;
              max-width: 980px;
              margin: 0 auto;
              padding: 45px;
            }

            @media (max-width: 767px) { .markdown-body { padding: 15px; } }
          </style>")))

(use-package web-mode
  :delight "☸"
  :preface
  (defun enable-minor-mode (my-pair)
    "Enable minor mode if filename match the regexp."
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))
  :mode ("\\.\\(html\\|jsx\\|php\\)\\'" . web-mode)
  :hook (web-mode . (lambda ()
                      (enable-minor-mode
                       '("\\.jsx?\\'" . prettier-js-mode))))
  :custom
  (web-mode-attr-indent-offset 4)
  (web-mode-block-padding 4)
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 4)
  (web-mode-comment-style 4)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 4))

(use-package plantuml-mode
  :mode ("\\.\\(plantuml\\|puml\\)\\'")
  :custom (plantuml-jar-path
           (expand-file-name (format "%s/plantuml.jar" xdg-lib))))

(use-package python
  :ensure flycheck
  :delight "π"
  :preface
  (defun python-remove-unused-imports()
    "Remove unused imports and unused variables with autoflake."
    (interactive)
    (if (executable-find "autoflake")
        (progn
          (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                                 (shell-quote-argument (buffer-file-name))))
          (revert-buffer t t t))
      (warn "[✗] python-mode: Cannot find autoflake executable.")))
  :bind (:map python-mode-map
              ("M-[" . python-nav-backward-block)
              ("M-]" . python-nav-forward-block)
              ("M-|" . python-remove-unused-imports))
  :custom
  (flycheck-pylintrc "~/.pylintrc")
  (flycheck-python-pylint-executable "/usr/bin/pylint"))

(use-package blacken
  :delight
  :hook (python-mode . blacken-mode)
  :custom (blacken-line-length 79))

(use-package py-isort
  :hook ((before-save . py-isort-before-save)
         (python-mode . pyvenv-mode)))

(use-package pyvenv
  :after python
  :custom
  (pyvenv-default-virtual-env-name (expand-file-name (format "%s/myenv/" xdg-data)))
  (pyvenv-workon (expand-file-name (format "%s/myenv/" xdg-data)))
  :config (pyvenv-tracking-mode))

(use-package pyenv-mode
  :hook ((python-mode . pyenv-mode)
         (projectile-switch-project . projectile-pyenv-mode-set))
  :custom (pyenv-mode-set "3.8.5")
  :preface
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset)))))

(use-package sql-mode
  :ensure nil
  :mode "\\.sql\\'")

(use-package sql-indent
  :delight sql-mode "Σ"
  :hook (sql-mode . sqlind-minor-mode))

(use-package css-mode
  :ensure flycheck
  :mode "\\.css\\'"
  :custom (css-indent-offset 4)
  :config (flycheck-stylelintrc "~/.stylelintrc.json"))

(use-package typescript-mode
  :ensure flycheck
  :hook ((typescript-mode . prettier-js-mode))
  :mode ("\\.\\(ts\\|tsx\\)\\'")
  :custom
  ;; TSLint is depreciated in favor of ESLint.
  (flycheck-disable-checker 'typescript-tslint)
  (typescript-indent-level 2)
  :config
  (flycheck-add-mode 'javascript-eslint 'typescript-mode))

(use-package vue-mode
  :delight "V"
  :mode "\\.vue\\'"
  :custom (vue-html-extra-indent 2))

(use-package nxml-mode
  :ensure nil
  :mode ("\\.\\(xml\\|xsd\\|wsdl\\)\\'"))

(use-package yaml-mode
  :delight "ψ"
  :mode ("\\.\\(yaml\\|yml\\)\\'"))

;; common lisp

(setq inferior-lisp-program "/usr/bin/sbcl")
(use-package slime)

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

(use-package highlight-defined)
(use-package highlight-quoted)
;;(use-package highlight-sexp-mode)
(use-package ipretty)
(use-package nameless)
(use-package elsa)
(use-package flycheck-package)


(defun maybe-clang-format ()
  (when (locate-dominating-file default-directory ".clang-format")
    (clang-format-buffer)))

(add-hook 'before-save-hook 'maybe-clang-format)


(use-package paredit)


;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

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
                                    (whitespace-mode -1)))

  (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))

;; end common lisp

(use-package highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(use-package hl-todo)



(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
  :custom (gofmt-command "goimports"))

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  :bind (:map rust-mode-map ("C-c C-c" . rust-run)))



(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "qutebrowser"))

(use-package calc
  :ensure nil
  :custom
  (math-additional-units
   '((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit")))
  ;; Resets the calc's cache.
  (math-units-table nil))

(use-package calendar
  :ensure nil
  :bind ("C-c 0" . calendar)
  :custom
  (calendar-mark-holidays-flag t)
  (calendar-week-start-day 1))

(use-package holidays
  :ensure nil
  :custom
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-oriental-holidays nil)
  (holiday-christian-holidays nil))

(use-package rainbow-mode
  :delight
  :hook ((prog-mode text-mode) . rainbow-mode))

(use-package dashboard
  :custom
  (dashboard-banner-logo-title "With Great Power Comes Great Responsibility!")
  (dashboard-center-content t)
  (dashboard-items '((agenda)
                     (projects . 5)))
  (dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (dashboard-set-file-icons t)
  (dashboard-set-footer nil)
  (dashboard-set-heading-icons t)
  (dashboard-set-navigator t)
  (dashboard-startup-banner 'logo)
  :config (dashboard-setup-startup-hook))

(use-package which-key
  :defer 0.2
  :delight
  :custom (which-key-idle-delay 0.5)
  :config (which-key-mode))

(use-package helpful
  :commands (helpful-at-point
             helpful-callable
             helpful-command
             helpful-function
             helpful-key
             helpful-macro
             helpful-variable)
  :bind
  ([remap display-local-help] . helpful-at-point)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

(use-package editorconfig
  :defer 0.3
  :config (editorconfig-mode))

(use-package savehist
  :ensure nil
  :custom
  (history-delete-duplicates t)
  (history-length 25)
  (savehist-file (expand-file-name (format "%s/emacs/history" xdg-cache)))
  :config (savehist-mode))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :config (unless (find-font (font-spec :name "all-the-icons"))
            (all-the-icons-install-fonts t)))

;; better than electric-indent-mode
(use-package aggressive-indent
  :custom (aggressive-indent-comments-too t))
(global-aggressive-indent-mode 1)

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method 'character))

(use-package circe
  :commands circe
  :preface
  (defun my/circe-count-nicks ()
    "Display the number of users connected on the current channel."
    (interactive)
    (when (eq major-mode 'circe-channel-mode)
      (message "%i users are online on %s."
               (length (circe-channel-nicks)) (buffer-name))))

  (defun my/circe-fetch-password (&rest params)
    "Fetch the NickServ password for an username."
    (require 'auth-source)
    (let ((match (car (apply 'auth-source-search params))))
      (if match
          (let ((secret (plist-get match :secret)))
            (if (functionp secret)
                (funcall secret)
              secret))
        (error "[✗] Password not found for %S" params))))

  (defun my/circe-nickserv-password (server)
    "Fetch the NickServ password for the Libera Chat."
    (my/circe-fetch-password :login "rememberYou" :machine "irc.libera.chat"))
  :custom
  (circe-default-part-message nil)
  (circe-default-quit-message nil)
  (circe-format-say (format "{nick:+%ss}: {body}" 8))
  (circe-network-options
   '(("Libera Chat"
      :nick "rememberYou"
      :tls t
      :port 6697
      :server-buffer-name "⇄ Libera Chat"
      :channels (:after-auth "#archlinux" "#bash" "#emacs" "#linux" "#python" "#qutebrowser" "#sway")
      :nickserv-password my/circe-nickserv-password)))
  (circe-reduce-lurker-spam t)
  (circe-use-cycle-completion t)
  (lui-flyspell-p t)
  :config
  (circe-lagmon-mode)
  (enable-circe-color-nicks)
  (enable-circe-display-images))

(use-package circe-notifications
  :after circe
  :hook (circe-server-connected . enable-circe-notifications))

(use-package flycheck
  :delight
  :bind (:map flycheck-mode-map
              ("M-'" . flycheck-previous-error)
              ("M-\\" . flycheck-next-error))
  :custom (flycheck-display-errors-delay .3))

(use-package message
  :ensure nil
  :after mu4e
  :custom
  (message-citation-line-format "On %B %e, %Y at %l:%M %p, %f (%n) wrote:\n")
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-kill-buffer-on-exit t)
  (message-send-mail-function 'smtpmail-send-it)
  (mml-secure-openpgp-signers '("208FCDBB98190562")))

(use-package ledger-mode
  :mode ("\\.\\(dat\\|ledger\\)\\'")
  :preface
  (defun my/ledger-save ()
    "Clean the ledger buffer at each save."
    (interactive)
    (ledger-mode-clean-buffer)
    (save-buffer))
  :bind (:map ledger-mode-map
              ("C-x C-s" . my/ledger-save))
  :hook (ledger-mode . ledger-flymake-enable)
  :custom
  (ledger-clear-whole-transactions t)
  (ledger-reconcile-default-commodity "EUR")
  (ledger-reports
   '(("account statement" "%(binary) reg --real [[ledger-mode-flags]] -f %(ledger-file) ^%(account)")
     ("balance sheet" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) bal ^assets ^liabilities ^equity")
     ("budget" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:budget")
     ("budget goals" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget goals'")
     ("budget obligations" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget obligations'")
     ("budget debts" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:bank ^assets:receivables ^assets:cash ^assets:'budget debts'")
     ("cleared" "%(binary) cleared [[ledger-mode-flags]] -f %(ledger-file)")
     ("equity" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) equity")
     ("income statement" "%(binary) --invert --real -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^income ^expenses -p \"this month\""))
   (ledger-report-use-header-line nil)))

(use-package flycheck-ledger :after ledger-mode)

(use-package faces
  :ensure nil
  :custom (show-paren-delay 0)
  :config
  (set-face-background 'show-paren-match "#161719")
  (set-face-bold 'show-paren-match t)
  (set-face-foreground 'show-paren-match "#ffffff"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :delight
  :hook (prog-mode . smartparens-mode)
  :bind (("M-'" . sp-backward-sexp)
         ("M-\\" . sp-forward-sexp)
         ("M-(" . sp-wrap-round)
         ("M-[" . sp-wrap-curly))
  :custom (sp-escape-quotes-after-insert nil))

(use-package webpaste
  :defer 0.4
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-p" . webpaste-paste-buffer-or-region)
         ("C-c C-p C-r" . webpaste-paste-region))
  :custom (webpaste-provider-priority '("dpaste.org" "dpaste.com" "ix.io")))

(use-package imgbb
  :commands imgbb-upload
  :bind ("C-c C-p C-i" . imgbb-upload))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :init (pdf-tools-install :no-query))

(use-package pdf-view
  :ensure nil
  :after pdf-tools
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("d" . pdf-annot-delete)
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("t" . pdf-annot-add-text-annotation))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
  ;; Avoid searching for unicodes to speed up pdf-tools.
  (pdf-view-use-unicode-ligther nil)
  ;; Enable HiDPI support, at the cost of memory.
  (pdf-view-use-scaling t))

(use-package projectile
  :diminish (projectile-mode)
  :custom
  (projectile-cache-file (expand-file-name (format "%s/emacs/projectile.cache" xdg-cache)))
  (projectile-enable-caching t)
  (projectile-keymap-prefix (kbd "C-c C-p"))
  (projectile-known-projects-file (expand-file-name (format "%s/emacs/projectile-bookmarks.eld" xdg-cache)))
  (projectile-mode-line '(:eval (projectile-project-name)))
  ;; Define the folder containing git repositories (e.g., ~/.local/share/git).
  (projectile-project-search-path '("~/.local/share/git"))
  (projectile-switch-project-action #'projectile-dired)
  :config (projectile-global-mode))

(use-package consult-projectile
  :after (consult projectile)
  :straight (consult-projectile :type git :host gitlab :repo
                                "OlMon/consult-projectile" :branch "master")
  :commands (consult-projectile))

(use-package ibuffer-projectile
  :after ibuffer
  :preface
  (defun my/ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook (ibuffer . my/ibuffer-projectile))

(use-package electric-operator
  :hook ((css-mode java-mode js2-mode
                  python-mode sql-mode typescript-mode) . electric-operator-mode))

(use-package recentf
  :ensure nil
  :bind ("C-x C-r" . recentf-open-files)
  :init (recentf-mode)
  :custom
  (recentf-exclude (list "/scp:"
                         "/ssh:"
                         "/sudo:"
                         "/tmp/"
                         "~$"
                         "COMMIT_EDITMSG"))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 200)
  (recentf-save-file (expand-file-name (format "%s/emacs/recentf" xdg-cache)))
  ;; Save recent files every 5 minutes to manage abnormal output.
  :config (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package wiki-summary
  :commands (wiki-summary wiki-summary-insert)
  :bind ("C-c W" . wiki-summary)
  :preface
  (defun my/format-summary-in-buffer (summary)
    "Given a summary, sticks it in the *wiki-summary* buffer and displays
     the buffer."
    (let ((buf (generate-new-buffer "*wiki-summary*")))
      (with-current-buffer buf
        (princ summary buf)
        (fill-paragraph)
        (goto-char (point-min))
        (view-mode))
      (pop-to-buffer buf)))
  :config
  (advice-add 'wiki-summary/format-summary-in-buffer
              :override #'my/format-summary-in-buffer))

(use-package try :commands try)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-commit
  :ensure nil
  :preface
  (defun my/git-commit-auto-fill-everywhere ()
    "Ensure that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil))
  :hook (git-commit-mode . my/git-commit-auto-fill-everywhere)
  :custom (git-commit-summary-max-length 50))

(use-package smerge-mode
  :after hydra
  :delight " ∓"
  :commands smerge-mode
  :bind (:map smerge-mode-map
              ("M-g n" . smerge-next)
              ("M-g p" . smerge-prev))
  :hook (magit-diff-visit-file . hydra-merge/body))

(use-package git-gutter
  :defer 0.3
  :delight
  :config (global-git-gutter-mode))

(use-package simple
  :ensure nil
  :delight (auto-fill-function)
  :preface
  (defun my/kill-region-or-line ()
    "When called interactively with no active region, kill the whole line."
    (interactive)
    (if current-prefix-arg
        (progn
          (kill-new (buffer-string))
          (delete-region (point-min) (point-max)))
      (progn (if (use-region-p)
                 (kill-region (region-beginning) (region-end) t)
               (kill-region (line-beginning-position) (line-beginning-position
                                                       2))))))
  :hook ((before-save . delete-trailing-whitespace)
         ((prog-mode text-mode) . turn-on-auto-fill))
  :bind ("C-w" . my/kill-region-or-line)
  :custom (set-mark-command-repeat-pop t))



(use-package org
  :ensure org-contrib
  :delight "Θ"
  :hook (org-mode . turn-off-auto-fill)
  :bind ("C-c i" . org-insert-structure-template)
  :preface
  (defun my/org-archive-done-tasks ()
    "Archive finished or cancelled tasks."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "TODO=\"DONE\"|TODO=\"CANCELLED\"" (if (org-before-first-heading-p) 'file 'tree)))

  (defun my/org-jump ()
    "Jump to a specific task."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-refile)))

  (defun my/org-use-speed-commands-for-headings-and-lists ()
    "Activate speed commands on list items too."
    (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
        (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))

  (defmacro ignore-args (fnc)
    "Returns function that ignores its arguments and invokes FNC."
    `(lambda (&rest _rest)
       (funcall ,fnc)))
  ;;  :hook ((after-save . my/config-tangle)
  ;;         (auto-save . org-save-all-org-buffers)
  ;;        (org-mode . visual-line-mode))
  :custom
  (org-archive-location "~/.personal/archives/%s::")
  (org-blank-before-new-entry '((heading . t)
                                (plain-list-item . t)))
  (org-confirm-babel-evaluate nil)
  (org-cycle-include-plain-lists 'integrate)
  (org-ellipsis " ▾")
  (org-export-backends '(ascii beamer html icalendar latex man md org texinfo))
  (org-hide-emphasis-markers t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-modules '(org-crypt
                 ;;org-habit
                 org-mouse
                 org-protocol
                 org-tempo))
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets '((org-agenda-files :maxlevel . 1)
                        ("~/.personal/agenda/home.org" :maxlevel . 2)
                        ("~/.personal/agenda/work.org" :maxlevel . 2)))
  (org-refile-use-cache nil)
  (org-refile-use-outline-path nil)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-tag-alist
   '((:startgroup . "Context")
     ("@errands" . ?e)
     ("@home" . ?h)
     ("@work" . ?w)
     (:endgroup)
     (:startgroup . "Difficulty")
     ("easy" . ?E)
     ("medium" . ?M)
     ("challenging" . ?C)
     (:endgroup)
     ("bug" . ?b)
     ("car" . ?v)
     ("future" . ?F)
     ("goal" . ?g)
     ("health" . ?H)
     ("house" . ?O)
     ("meeting" . ?m)
     ("planning" . ?p)
     ("phone" . ?0)
     ("purchase" . ?P)
     ("reading" . ?r)
     ("review" . ?R)
     ("study" . ?s)
     ("sport" . ?S)
     ("talk" . ?T)
     ("tech" . ?t)
     ("trip" . ?I)
     ("thinking" . ?i)
     ("update" . ?u)
     ("watch" . ?l)
     ("writing" . ?W)))
  (org-tags-exclude-from-inheritance '("crypt" "project"))
  (org-todo-keywords '((sequence "TODO(t)"
                                 "STARTED(s)"
                                 "NEXT(n)"
                                 "SOMEDAY(.)"
                                 "WAITING(w)""|" "DONE(x!)" "CANCELLED(c@)")))
  (org-use-effective-time t)
  (org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists)
  (org-yank-adjusted-subtrees t)
  :config
  (add-to-list 'org-global-properties '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))
  (add-to-list 'org-speed-commands '("$" call-interactively 'org-archive-subtree))
  (add-to-list 'org-speed-commands '("i" call-interactively 'org-clock-in))
  (add-to-list 'org-speed-commands '("o" call-interactively 'org-clock-out))
  (add-to-list 'org-speed-commands '("s" call-interactively 'org-schedule))
  (add-to-list 'org-speed-commands '("x" org-todo "DONE"))
  (add-to-list 'org-speed-commands '("y" org-todo-yesterday "DONE"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (advice-add 'org-deadline :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-schedule :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-store-log-note :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-todo :after (ignore-args #'org-save-all-org-buffers))
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (org-clock-persistence-insinuate)
  (org-load-modules-maybe t))

(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable))

(use-package org-agenda
  :ensure nil
  :bind (:map org-agenda-mode-map
              ("C-n" . org-agenda-next-item)
              ("C-p" . org-agenda-previous-item)
              ("j" . org-agenda-goto)
              ("X" . my/org-agenda-mark-done-next)
              ("x" . my/org-agenda-mark-done))
  :preface
  (defun my/org-agenda-mark-done (&optional arg)
    "Mark the current TODO as done in org-agenda."
    (interactive "P")
    (org-agenda-todo "DONE"))

  (defun my/org-agenda-mark-done-next ()
    "Mark the current TODO as done and add another task after it."
    (interactive)
    (org-agenda-todo "DONE")
    (org-agenda-switch-to)
    (org-capture 0 "t"))
  :custom
  (org-agenda-category-icon-alist
   `(
     ("home" , "" nil nil :ascent center :mask heuristic)
     ("inbox" , "" nil nil :ascent center :mask heuristic)
     ("people" , "" nil nil :ascent center :mask heuristic)
     ("work" , "" nil nil :ascent center :mask heuristic)
     ("routine" , "" nil nil :ascent center :mask heuristic)
     ))
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))
       (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

     ("n" "Next Tasks"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))))

     ("h" "Home Tasks" tags-todo "@home")
     ("w" "Work Tasks" tags-todo "@work")

     ("E" "Easy Tasks" tags-todo "easy")
     ("C" "Challenging Tasks" tags-todo "challenging")

     ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
      ((org-agenda-overriding-header "Low Effort Tasks")
       (org-agenda-max-todos 20)
       (org-agenda-files org-agenda-files)))))
  (org-agenda-dim-blocked-tasks t)
  (org-agenda-files '("~/.personal/agenda"))
  (org-agenda-inhibit-startup t)
  (org-agenda-show-log t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-span 2)
  (org-agenda-start-on-weekday 6)
  (org-agenda-start-with-log-mode t)
  (org-agenda-sticky nil)
  (org-agenda-tags-column 90)
  (org-agenda-time-grid '((daily today require-timed)))
  (org-agenda-use-tag-inheritance t)
  (org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS")
  (org-default-notes-file "~/.personal/agenda/inbox.org")
  (org-directory "~/.personal")
  (org-enforce-todo-dependencies t)
  (org-habit-completed-glyph ?✓)
  (org-habit-graph-column 80)
  (org-habit-show-habits-only-for-today nil)
  (org-habit-today-glyph ?‖)
  (org-track-ordered-property-with-tag t))

(use-package org-wild-notifier
  :after org
  :custom
  (alert-default-style 'libnotify)
  (org-wild-notifier-notification-title "Agenda Reminder")
  :config (org-wild-notifier-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("●" "►" "▸")))

(use-package org-capture
  :ensure nil
  :preface
  (defvar my/org-active-task-template
    (concat "* NEXT %^{Task}\n"
            ":PROPERTIES:\n"
            ":Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n"
            ":CAPTURED: %<%Y-%m-%d %H:%M>\n"
            ":END:") "Template for basic task.")
  (defvar my/org-appointment
    (concat "* TODO %^{Appointment}\n"
            "SCHEDULED: %t\n") "Template for appointment task.")
  (defvar my/org-basic-task-template
    (concat "* TODO %^{Task}\n"
            ":PROPERTIES:\n"
            ":Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n"
            ":CAPTURED: %<%Y-%m-%d %H:%M>\n"
            ":END:") "Template for basic task.")
  (defvar my/org-contacts-template
    (concat "* %(org-contacts-template-name)\n"
            ":PROPERTIES:\n"
            ":BIRTHDAY: %^{YYYY-MM-DD}\n"
            ":END:") "Template for a contact.")
  :custom
  (org-capture-templates
   `(
     ("c" "Contact" entry (file+headline "~/.personal/agenda/contacts.org" "Inbox"),
      my/org-contacts-template
      :empty-lines 1)

     ("p" "People" entry (file+headline "~/.personal/agenda/people.org" "Tasks"),
      my/org-basic-task-template
      :empty-lines 1)
     ("a" "Appointment" entry (file+headline "~/.personal/agenda/people.org" "Appointments"),
      my/org-appointment
      :empty-lines 1)
     ("m" "Meeting" entry (file+headline "~/.personal/agenda/people.org" "Meetings")
      "* Meeting with %? :meeting:\n%U" :clock-in t :clock-resume t :empty-lines 1)
     ("P" "Phone Call" entry (file+headline "~/.personal/agenda/people.org" "Phone Calls")
      "* Phone %? :phone:\n%U" :clock-in t :clock-resume t)

     ("i" "New Item")
     ("ib" "Book" checkitem (file+headline "~/.personal/items/books.org" "Books")
      "- [ ] %^{Title} - %^{Author}\n  %U"
      :immediate-finish t)
     ("il" "Learning" checkitem (file+headline "~/.personal/items/learning.org" "Things")
      "- [ ] %^{Thing}\n  %U"
      :immediate-finish t)
     ("im" "Movie" checkitem (file+headline "~/.personal/items/movies.org" "Movies")
      "- [ ] %^{Title}\n  %U"
      :immediate-finish t)
     ("ip" "Purchase" checkitem (file+headline "~/.personal/items/purchases.org" "Purchases")
      "- [ ] %^{Item}\n  %U"
      :immediate-finish t)

     ("t" "New Task")
     ("ta" "Active" entry (file+headline "~/.personal/agenda/inbox.org" "Active"),
      my/org-active-task-template
      :empty-lines 1
      :immediate-finish t)
     ("tb" "Backlog" entry (file+headline "~/.personal/agenda/inbox.org" "Backlog"),
      my/org-basic-task-template
      :empty-lines 1
      :immediate-finish t))))

(use-package org-clock
  :ensure nil
  :after org
  :preface
  (defun my/org-mode-ask-effort ()
    "Ask for an effort estimate when clocking in."
    (unless (org-entry-get (point) "Effort")
      (let ((effort
             (completing-read
              "Effort: "
              (org-entry-get-multivalued-property (point) "Effort"))))
        (unless (equal effort "")
          (org-set-property "Effort" effort)))))
  :hook (org-clock-in-prepare-hook . my/org-mode-ask-effort)
  :custom
  (org-clock-clocktable-default-properties
   '(:block thisweek :maxlevel 2 :scope agenda :link t :compact t :formula %
            :step week :fileskip0 t :stepskip0 t :narrow 50
            :properties ("Effort" "CLOCKSUM" "TODO")))
  (org-clock-continuously nil)
  (org-clock-in-switch-to-state "STARTED")
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-persist t)
  (org-clock-persist-file (expand-file-name (format "%s/emacs/org-clock-save.el" xdg-cache)))
  (org-clock-persist-query-resume nil)
  (org-clock-report-include-clocking-task t)
  (org-show-notification-handler (lambda (msg) (alert msg))))

(use-package org-pomodoro
  :after org
  :custom
  (alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
  (org-pomodoro-audio-player "/usr/bin/mpv")
  (org-pomodoro-finished-sound "~/audio/pomodoro_finished.mp3")
  (org-pomodoro-format " %s")
  (org-pomodoro-killed-sound "~/audio/pomodoro_killed.mp3")
  (org-pomodoro-long-break-sound "~/audio/pomodoro_long.mp3")
  (org-pomodoro-overtime-sound "~/audio/pomodoro_overtime.mp3")
  (org-pomodoro-short-break-sound "~/audio/pomodoro_short.mp3")
  (org-pomodoro-start-sound "~/audio/pomodoro_start.mp3")
  (org-pomodoro-start-sound-p t))

;; (use-package org-contacts
;;   :after org
;;   :ensure nil
;;   :custom (org-contacts-files '("~/.personal/agenda/contacts.org")))

(use-package org-faces
  :ensure nil
  :custom
  (org-todo-keyword-faces
   '(("DONE"    . (:foreground "#8abeb7" :weight bold))
     ("NEXT"    . (:foreground "#f0c674" :weight bold))
     ("SOMEDAY" . (:foreground "#b294bb" :weight bold))
     ("TODO"    . (:foreground "#b5bd68" :weight bold))
     ("WAITING" . (:foreground "#de935f" :weight bold)))))

(use-package org-crypt
  :ensure nil
  :init (org-crypt-use-before-save-magic)
  :custom (org-crypt-key "E9AADC36E94A672D2A17D49B208FCDBB98190562"))

(setq epa-file-encrypt-to "zubairstudytech@gmail.com")
(setq epa-file-select-keys "auto")

;; (use-package jupyter :ensure nil :after org)
(use-package python :ensure nil :after org)
(use-package ob-C :ensure nil :after org)
(use-package ob-css :ensure nil :after org)
(use-package ob-dot :ensure nil :after org)
;;(use-package ob-ein :ensure nil :after org)
(use-package ob-emacs-lisp :ensure nil :after org)
(use-package ob-gnuplot :ensure nil :after org)
(use-package ob-java :ensure nil :after org)
(use-package ob-js :ensure nil :after org)
(use-package ob-latex
  :ensure nil
  :after org
  :custom (org-latex-compiler "xelatex"))
(use-package ob-ledger :ensure nil :after org)
(use-package ob-makefile :ensure nil :after org)
(use-package ob-org :ensure nil :after org)
(use-package ob-plantuml
  :ensure nil
  :after org
  :custom (org-plantuml-jar-path (expand-file-name (format "%s/plantuml.jar" xdg-lib))))
(use-package ob-python :ensure nil :after org)
(use-package ob-shell :ensure nil :after org)
(use-package ob-sql :ensure nil :after org)

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  (setq my/daily-note-filename "%<%Y-%m-%d>.org.gpg"
        my/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
  :custom
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "journal/")
  (org-roam-dailies-capture-templates
   `(("d" "default" plain
      "* %?"
      :if-new (file+head ,my/daily-note-filename
                         ,my/daily-note-header)
      :empty-lines 1)

     ("j" "journal" plain
      "** %<%I:%M %p>  :journal:\n\n%?\n\n"
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Journal"))
      :empty-lines 1)
     ("m" "meeting" entry
      "** %<%I:%M %p> - %^{Meeting Title}  :meeting:\n\n%?\n\n"
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Meetings"))
      :empty-lines 1)))
  (org-roam-directory "~/.personal/notes")
  :custom (org-roam-graph-viewer "/usr/bin/qutebrowser")
  :config (org-roam-setup))

(defun my-programming-mode-hook ()
  (setq tab-width 4)
  (setq indent-tabs-mode nil)  ; Optional: use spaces instead of tabs
  (setq c-basic-offset 4)      ; For C-like modes
  (setq js-indent-level 4)     ; For JavaScript
  (setq python-indent-offset 4); For Python
  (setq web-mode-code-indent-offset 4) ; For web-mode
  (setq web-mode-markup-indent-offset 4) ; For web-mode
  (setq web-mode-css-indent-offset 4) ; For web-mode
  ;; (setq lsp-mode-indent-offset 4)
  )

(add-hook 'c-mode-common-hook 'my-programming-mode-hook)
(add-hook 'js-mode-hook 'my-programming-mode-hook)
(add-hook 'python-mode-hook 'my-programming-mode-hook)
(add-hook 'web-mode-hook 'my-programming-mode-hook)
;; (add-hook 'lsp-mode-hook 'my-programming-mode-hook)

(setq c-doc-comment-style '((c-mode . doxygen)
                            (c++-mode . doxygen)))

(use-package eglot
  :hook ((c-mode c++-mode python-mode) . eglot-ensure)
  :config
  ;; Disable flymake (eglot enables it by default)
  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))

  ;; Optional: disable eldoc hints if you want absolute minimalism
  (setq eldoc-display-functions nil)

  ;; Disable snippet support (eglot doesn’t send snippet capability by default anyway, so you're good)
  ;; But you can be explicit:
  (setq eglot-extend-to-xref nil))

;; No auto-completion
(setq completion-at-point-functions nil) ;; disables LSP + default CAPF
