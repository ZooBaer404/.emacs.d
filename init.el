;; init.el starts here

;; Make startup faster by reducing the frequency of garbage
;; collection.
(setq gc-cons-threshold (* 100 1024 1024))

(require 'package)
(package-initialize)

(load-file (expand-file-name "config.el" user-emacs-directory))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 10 1000 1000))


;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
 '(org-agenda-files nil nil nil "Customized with use-package org-agenda"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
