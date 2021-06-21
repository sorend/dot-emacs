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

(when (window-system)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq-default fram-title-format '("Emacs " emacs-version)))

(windmove-default-keybindings)
(global-hl-line-mode)
(auto-fill-mode -1)

(setq inhibit-startup-message t)
(setq-default c-basic-offset 4
              indent-tabs-mode nil
              tab-width 4
              display-time-24hr-format t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(use-package pdf-tools
    :config
    (pdf-loader-install))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package dash)

(use-package better-defaults
  :config
  ;; backups: use system tmp dir instead of user emacs dir
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(use-package monokai-theme
  :defer nil
  :config
  (load-theme 'monokai t))

(use-package bs)
(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer))

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-to-list 'company-backends 'company-ansible)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package crontab-mode)

;; refresh whee
(global-set-key (kbd "<f5>") 'revert-buffer)

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package consult)

(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

;; search improvement
(use-package ctrlf
  :config
  (ctrlf-mode +1))

(use-package bufler
  :config
  (defun sorend/bufler-one-window (&optional force-refresh)
    (interactive "P")
    (bufler-list)
    (delete-other-windows))
  (global-set-key (kbd "C-x C-b") 'sorend/bufler-one-window))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-c x" . helm-all-mark-rings)))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package projectile)

(use-package helm-projectile
  :hook projectile-mode
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package auto-package-update
  :config
  (auto-package-update-at-time "02:00"))

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
;; clojure
;;
(use-package cider)


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


;; setup tramp
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"
        tramp-auto-save-directory "~/.emacs.d/tramp-autosave-dir"
        password-cache-expiry 3600
        recentf-keep '(file-remote-p file-readable-p)))

;; setup ace-window
(use-package ace-window
  :bind ("M-p" . ace-window))

;; needed for groovy mode
(use-package cl-lib)

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
(use-package yaml-mode)
;  :mode (("\\.yml\\'" . yaml-mode)
;         ("\\.yaml\\'" . yaml-mode)))

;;
;; Multiple cursors
;;
(use-package multiple-cursors
  :bind
  (("s-SPC" . mc/edit-lines)
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

;;
;; Dockerfile support
;; ------
(use-package dockerfile-mode
  :mode (("Dockerfile'" . dockerfile-mode)))

;; add company-tabnine
;; (use-package company-tabnine
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends #'company-tabnine)
;;   ;; trigger completion immediately
;;   (setq company-idle-delay 0)
;;   ;; Number the candidates (use M-1, M-2 etc to select completions).
;;   (setq company-show-numbers t)
;;   (company-tabnine-install-binary))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package exec-path-from-shell
  :config
  (dolist (shell-variable '("SSH_AUTH_SOCK"
                            "SSH_AGENT_PID"
                            "BROWSER"
                            "CI"
                            "GOPATH"
                            "LANG"
                            "MANPATH"
                            "PATH"))
    (add-to-list 'exec-path-from-shell-variables shell-variable))
  (exec-path-from-shell-initialize))

(use-package go-mode
  :after (lsp-mode)
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  ;; keybindings
  (add-to-list 'lsp-enabled-clients 'gopls)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)
)

;;
;; lsp mode
;;
(use-package lsp-mode
  :config
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
        lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands
  lsp-treemacs-errors-list)


;; optional if you want which-key integration
(use-package which-key
    :config
    (setq which-key-show-erly-on-C-h t)
    (setq which-key-idle-delay 3)
    (setq which-key-idle-secondary-delay 0.05)
    (which-key-mode))

(use-package python-mode
  :after (lsp-mode)
  :hook ((python-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  ;; keybindings
  (add-to-list 'lsp-enabled-clients 'pyls))

(use-package pyenv-mode)

(use-package pyenv-mode-auto)

;; java
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

(use-package dap-java
  :ensure nil)


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
    (lsp-ui lsp-mode go-mode company-tabnine auctex crontab-mode company ansible-vault ansible-doc ansible pdf-tools use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
