;; init.el --- Emacs configuration

(when (string-equal system-name "rebala") ;; for laptop
  (set-face-attribute 'default nil :height 165))

(toggle-frame-maximized)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(setq gc-cons-threshold (* 256 1024 1024))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; initialize package setup
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; need use-package if we don't have it already
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :config
  (setq use-package-always-ensure t
        use-package-always-demand t
        use-package-always-defer nil
        use-package-verbose t
        use-package-minimum-reported-time 0.0001))

(use-package paradox
  :config
  (paradox-enable))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'sorend-general-config)

(require 'sorend-lsp-config)

(require 'sorend-programming-config)

(require 'sorend-paper-config)




;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lsp-ui lsp-mode go-mode company-tabnine auctex crontab-mode company ansible-vault ansible-doc ansible pdf-tools use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
