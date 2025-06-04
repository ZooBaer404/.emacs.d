;;; custom.el --- Custom Emacs configuration inserted by Emacs itself  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  zoobaer

;; Author: zoobaer <zoobaer@fedora>
;; Keywords: lisp

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ad-redefinition-action 'accept)
 '(backup-directory-alist `(("." \, (concat user-emacs-directory "backups"))))
 '(confirm-kill-processes nil)
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "088cd6f894494ac3d4ff67b794467c2aa1e3713453805b93a8bcb2d72a0d1b53"
     "fffef514346b2a43900e1c7ea2bc7d84cbdd4aa66c1b51946aade4b8d343b55a"
     default))
 '(indent-tabs-mode nil)
 '(kill-whole-line t)
 '(load-prefer-newer t t)
 '(mouse-yank-at-point t)
 '(org-agenda-files
   '("~/gtd" "~/Work/work.org.gpg" "~/org/ideas.org" "~/org/projects.org"
     "~/org/diary.org"))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(show-trailing-whitespace nil)
 '(use-package-always-defer t)
 '(use-package-verbose nil))

(provide 'custom)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
