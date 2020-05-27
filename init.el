;; init.el --- Emacs configuration

;; BASIC INSTALL PACKAGES
;; ----------------------
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; at work (windows)
;; (when (eq system-type 'windows-nt)
;;   (setq url-proxy-services '(("no_proxy" . "127.0.0.1")
;;                              ("http" . "127.0.0.1:3128")
;;                              ("https" . "127.0.0.1:3128")))
;;   )
;; at home (linux)
(when (eq system-type 'gnu/linux) ;; for home
  (set-face-attribute 'default nil :height 165)
  )


(toggle-frame-maximized)

;; initialize package setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; need use-package if we don't have it already
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package better-defaults
  :config
  ;; backups: use system tmp dir instead of user emacs dir
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

;;(use-package solarized-theme
;;  :config
;;  (load-theme 'solarized-light t))


;;
;; BASIC CONFIG
;; ------------
(windmove-default-keybindings)
(setq inhibit-startup-message t) ;; hide the startup message
;; (load-theme 'material t) ;; load material theme
(global-hl-line-mode)

;; disable auto fill mode
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; we hate this crap
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;

(use-package flycheck)

;;
;; PYTHON CONFIG
;; -------------
(use-package elpy
  :ensure t
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  ;; use flycheck instead of flymake
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (use-package py-autopep8
    :config
    (setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
    (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))
  (use-package pyenv-mode
    :commands pyenv-mode)
  (use-package pyenv-mode-auto)
  :bind ("RET" . newline-and-indent))

;;
;; JAVASCRIPT CONFIG
;; -----------------
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset 4)
  (js2-imenu-extras-mode)
  (use-package js2-refactor)
  (use-package skewer-mode
    :commands skewer-mode))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (defun jcf-json-mode-hook ()
    (interactive)
    (setq js-indent-level 4)
    (rainbow-delimiters-mode))

  (add-hook 'json-mode-hook 'jcf-json-mode-hook))


(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t))

(use-package css-mode
  :commands css-mode
  :init
  (setq css-indent-offset 2)
  :config
  (use-package rainbow-mode
    :init
    (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
      (add-hook hook 'rainbow-mode)))
  (use-package css-eldoc)
  (use-package mmm-mode
    :config
    (mmm-add-group
     'html-css
     '((css-cdata
        :submode css-mode
        :face mmm-code-submode-face
        :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
        :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
        :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                     @ "\n" _ "\n" @ "</script>" @)))
       (css
        :submode css-mode
        :face mmm-code-submode-face
        :front "<style[^>]*>[ \t]*\n?"
        :back "[ \t]*</style>"
        :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                     @ "\n" _ "\n" @ "</style>" @)))
       (css-inline
        :submode css-mode
        :face mmm-code-submode-face
        :front "style=\""
        :back "\"")))))

;;
;; LaTeX CONFIG
;; ------------
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
;;    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t)
  (setq-default TeX-master nil))

;; (use-package preview
;;   :commands LaTeX-preview-setup
;;   :init
;;   (progn
;;     (setq-default preview-scale 1.4
;; 		  preview-scale-function '(lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))))

(use-package reftex
  :commands turn-on-reftex
  :init
  (progn
    (setq reftex-plug-into-AUCTeX t)))

(use-package bibtex
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))

;;(defvar myPackages
;;  '(ein
;;    elpy
;;    flycheck
;;    material-theme
;;    py-autopep8
;;    pyenv-mode
;;    pyenv-mode-auto))

;; setup tramp
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave-dir")
  (setq password-cache-expiry 3600)
  (setq recentf-keep '(file-remote-p file-readable-p)))

;; setup ace-window
(use-package ace-window
             :bind ("M-p" . ace-window))

;;
;; rexx mode
;;
(use-package rexx-mode
  :ensure f
  :load-path "lisp/"
  :mode ("\\.rexx$" . rexx-mode))

;; needed for groovy mode
(use-package cl)

;;
;; Gradle/groovy support
;;
(use-package groovy-mode
  :mode      ("\\.\\(groovy\\|gradle\\)$" . groovy-mode)
  :config
  (require 'groovy-electric)
  (add-hook 'after-init-hook '(lambda() (groovy-electric-mode))))

;;
;; markdown
;;
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (use-package markdown-preview-mode))

;;
;; yaml
;;
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

;;
;; Multiple cursors
;;
(use-package multiple-cursors
  :bind (("s-SPC" . mc/edit-lines)
         ("s-k" . mc/mark-next-like-this)
         ("s-j" . mc/mark-previous-like-this)
         ("s-S-k" . mc/mark-all-like-this)))

;;
;; GIT CONFIG
;; ----------
(use-package magit
             :bind ("C-x g" . magit-status)
             :config
             ;; disable auto-revert-mode (a bit faster w/o)
             (magit-auto-revert-mode 0)
             ;; display the magit in a full screen buffer
             (setq magit-display-buffer-function
                   'magit-display-buffer-fullframe-status-v1))

(use-package magit-find-file
             :bind ("C-c p" . magit-find-file-completing-read))

(use-package forge
  :after magit)

;;
;; Dockerfile support
;; ------
(use-package dockerfile-mode
  :mode (("Dockerfile'" . dockerfile-mode)))

;;
;; Racket support
;;
(use-package racket-mode
  :mode      ("\\.\\(rkt\\|scm\\)$" . racket-mode))

;;
;; Haskell mode
;;
(use-package haskell-mode
  :mode      ("\\.\\(hs\\)$" . haskell-mode)
  :init
  ;(progn
  ;  (use-package intero)
                                        ;  (add-hook 'haskell-mode-hook 'intero-mode)))
  )

;; add company-tabnine
(use-package company-tabnine
  :ensure t
  :config
  (add-to-list 'company-backends #'company-tabnine)
  ;; trigger completion immediately
  (setq company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  (company-tabnine-install-binary))

;;
;; Prolog mode
;;
;; (use-package prolog-mode
;;   :mode      ("\\.\\(pro\\)$" . prolog-mode)
;;   :init
;;   (progn
;;     (use-package ediprolog)
;;     (local-set-key [f10] 'ediprolog-dwim)
;;   ))


;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (intero flymake-racket flylisp markdown-preview-mode dockerfile-mode monky magit-find-file magit yaml-mode markdown-mode groovy-mode ace-window auctex mmm-mode css-eldoc rainbow-mode web-mode json-mode skewer-mode js2-refactor js2-mode pyenv-mode-auto pyenv-mode py-autopep8 elpy flycheck solarized-theme better-defaults use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
