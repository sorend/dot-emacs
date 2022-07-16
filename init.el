;; init.el --- Emacs configuration

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
  :custom
  (use-package-always-ensure t)
  ;; (use-package-always-demand t)
  (use-package-always-defer nil)
  (use-package-verbose t)
  (use-package-minimum-reported-time 0.0001))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;;
;; general configuration features
;;

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  ;; (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :config
  (progn
    (when (window-system)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (setq-default fram-title-format '("Emacs " emacs-version)))

    (recentf-mode 1)
    (add-to-list 'default-frame-alist '(font . "JetBrains Mono-14"))
    (add-to-list 'default-frame-alist '(line-spacing . 0.2))

    (when (string-equal system-name "rebala") ;; for laptop
      (set-face-attribute 'default nil :height 165))

    (toggle-frame-maximized)
    (setq inhibit-splash-screen t)
    (setq inhibit-startup-message t)

    (setq gc-cons-threshold (* 256 1024 1024))

    (windmove-default-keybindings)
    (global-hl-line-mode)
    (auto-fill-mode -1)
    (global-display-line-numbers-mode 1)

    (auth-source-pass-enable) ;; start auth source pass
    (setq auth-source-debug t
          auth-source-do-cache nil
          auth-sources '(password-store))

    (setq inhibit-startup-message t)
    (setq-default c-basic-offset 4
                  indent-tabs-mode nil
                  tab-width 4
                  display-time-24hr-format t
                  completion-cycle-threshold 3
                  tab-always-indent 'complete)

    (add-hook 'before-save-hook 'delete-trailing-whitespace)
    (remove-hook 'text-mode-hook #'turn-on-auto-fill)
    ;; refresh whee
    (global-set-key (kbd "<f5>") 'revert-buffer)
    ;; theme
    (load-theme 'modus-vivendi t)
    
    ))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :config
  (marginalia-mode))

;; (use-package embark
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;   :init
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; ;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

(use-package beacon
  :config
  (beacon-mode 1))

(use-package dash)

(use-package better-defaults
  :config
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

;; (use-package bs)
;; (use-package ibuffer
;;   :config
;;   (defalias 'list-buffers 'ibuffer))

(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first nil) ;; Disable candidate preselection
  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))



;; search improvement
(use-package ctrlf
  :config
  (ctrlf-mode +1))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package projectile)

;; automatically update packages every 7 days
(use-package auto-package-update
  :demand t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))


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
;; Functions to copy external files in place
;;
(defun sorend/copy-cfg-file (src dest)
  (let ((full-src (if (file-name-absolute-p src) (src) (expand-file-name src "~/.emacs.d/external-configs"))))
    (copy-file full-src dest t)))
(defun sorend/setup-external (ext-dir &rest ext-files)
  (let ((ext-dir-full (expand-file-name ext-dir)))
    (progn
      (make-directory ext-dir t)
      (cl-loop for fn in ext-files do (sorend/copy-cfg-file fn ext-dir-full)))))



;;
;; Multiple cursors
;;
(use-package multiple-cursors
  :bind
  (("C-c SPC" . mc/edit-lines)
   ("C-c C-<down>" . mc/mark-next-like-this)
   ("C-c C-<up>" . mc/mark-previous-like-this)
   ("C-c C-<right>" . mc/mark-all-like-this)))

;;
;; magit configuation
;;
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

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package exec-path-from-shell
  :config
  (dolist (shell-variable '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "BROWSER" "CI"
                            "GOPATH" "LANG" "MANPATH" "PATH"))
    (add-to-list 'exec-path-from-shell-variables shell-variable))
  (exec-path-from-shell-initialize))


;; (use-package flycheck
;;   :requires helm-flycheck flycheck-pycheckers flycheck-inline
;;   :config
;;   (flycheck-inline-mode)
;;   )

;; (use-package flycheck-inline)

;; ;; Requires local dependencies:
;; ;;   pip install flake8 bandit
;; (use-package flycheck-pycheckers
;;   :config
;;   (setq flycheck-pycheckers-checkers '(flake8 bandit)
;;         flycheck-pycheckers-ignore-codes
;;         '("C0411" "C0413" "C0103" "C0111" "W0142" "W0201" "W0232" "W0403" "W0511" "E1002" "E1101"
;;           "E1103" "R0201" "R0801" "R0903" "R0904" "R0914" "W503" "W504"
;;           ;; flake8
;;           "E111" "E114" "E121" "E126" "E127" "E221" "E241" "E302" "E305"
;;           ;; bandit
;;           "B101" "B322")
;;         flycheck-pycheckers-max-line-length 180
;;         flycheck-pycheckers-multi-thread "true")

;;   (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)

;;   ;; Ensure that the correct python checker is chosen.
;;   (add-hook 'python-mode-hook (lambda () (flycheck-select-checker 'python-pycheckers))))

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
  (setq lsp-pylsp-plugins-flake8-enabled t
        lsp-pylsp-plugins-jedi-completion-enabled t
        lsp-pylsp-plugins-pycodestyle-enabled t
        lsp-pylsp-plugins-pydocstyle-enabled t
        lsp-pylsp-plugins-jedi-use-pyenv-environment t
        lsp-pylsp-plugins-pyflakes-enabled t)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-diagnostic-package :flycheck)
  (lsp-prefer-capf t)
  (read-process-output-max (* 1024 1024))
  :hook
  ((go-mode python-mode) . lsp)
  :commands
  (lsp lsp-deferred))

;; optionally
(use-package lsp-ui
  :requires lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable nil
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  :hook
  ((lsp-mode . lsp-ui-mode)))

(use-package lsp-treemacs)

;; optional if you want which-key integration
(use-package which-key
    :config
    (setq which-key-show-erly-on-C-h t)
    (setq which-key-idle-delay 3)
    (setq which-key-idle-secondary-delay 0.05)
    (which-key-mode))

;;
;; Python configuration
;;
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(use-package python-mode
  :after (lsp-mode)
  :hook
  ((python-mode . lsp-deferred)
   (before-save . lsp-format-buffer)
   (before-save . lsp-organize-imports))
  :config
  ;; keybindings
  ;; (add-to-list 'lsp-enabled-clients 'pylsp))
  )

(use-package pyenv-mode)

(use-package pyenv-mode-auto)

(use-package python-pytest
  :after python
  :bind
  (("C-c t" . python-pytest-dispatch))
  :custom
  (python-pytest-arguments
   '("--color"          ;; colored output in the buffer
     "--failed-first"   ;; run the previous failed tests first
     "--maxfail=5"))    ;; exit in 5 continuous failures in a run
  :config
  ;; (which-key-declare-prefixes-for-mode 'python-mode "SPC pt" "Testing"))
  )

;;(use-package cl-1.0)
;;(use-package poetry
;;  :ensure t)



;;
;; Misc file editing modes
;;
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (use-package markdown-preview-mode))

;; yaml
(use-package yaml-mode)

;; toml
(use-package toml-mode)

;;
;; LaTeX setup
;;
;;
;; LaTeX setup
;;

(use-package pdf-tools
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  (pdf-misc-print-programm "/usr/bin/lpr")
  (pdf-misc-print-programm-args (quote ("-o media=A4" "-o fitplot")))
  :config
  (pdf-tools-install)
  :bind
  (:map pdf-view-mode-map
        ("C-s" . 'isearch-forward)
        ("C-r" . 'isearch-backward)))
;  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
;  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward))

(use-package auctex-latexmk
  :after (tex-site tex-buf)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package reftex
  :defer t
  :custom
  (reftex-cite-prompt-optional-args t))

(use-package cdlatex)


(use-package auto-dictionary
  :init
  (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

;; (use-package company-auctex
;;   :ensure t
;;   :after tex-site company yasnippet
;;   :hook LaTeX-mode
;;   :init (company-auctex-init))

(use-package tex-site
  :ensure auctex
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (setq TeX-source-correlate-mode t
	    TeX-source-correlate-method 'synctex
	    TeX-auto-save t
	    TeX-parse-self t
	    reftex-plug-into-AUCTeX t
	    TeX-view-program-selection '((output-pdf "PDF Tools"))
		TeX-source-correlate-start-server t)
  (setq-default TeX-master "paper.tex")
  (pdf-tools-install)
	;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
		    #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook
		    (lambda ()
			  (reftex-mode t)
			  (flyspell-mode t)))
  )

;; (use-package tex-site
;;   :ensure auctex
;;   :mode ("\\.tex\\'" . latex-mode)
;;   :commands (latex-mode LaTeX-mode plain-tex-mode)
;;   :config
;;   (progn
;;     (setq-default TeX-source-correlate-mode t
;;                   TeX-source-correlate-method 'synctex
;;                   reftex-plug-into-AUCTeX t
;;                   TeX-auto-save t
;;                   TeX-parse-self t
;;                   TeX-save-query nil
;;                   TeX-PDF-mode t
;;                   TeX-master nil)
;; 	(auctex-latexmk-setup)
;; 	(setq-default TeX-view-program-selection '((output-pdf "PDF Tools"))
;; 				  TeX-source-correlate-start-server t)
;;   ;; Update PDF buffers after successful LaTeX runs
;;     (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;; 	(add-hook 'LaTeX-mode-hook (lambda () (reftex-mode t) (flyspell-mode t)))
;;     )
;;   :hook
;;   ((LaTeX-mode . flyspell-mode)
;;    (LaTeX-mode . turn-on-reftex)
;;    (LaTeX-mode . LaTeX-math-mode)))


;; reftex
;; (use-package reftex
;;   :commands turn-on-reftex
;;   :init
;;   (setq reftex-extra-bindings t
;;         reftex-plug-into-AUCTeX t))

;; bibtex
(use-package bibtex
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))

(use-package biblio)

(use-package parsebib)

(use-package ebib
  :after (tex biblio parsebib)
  :commands (ebib)
  :bind
  (("C-c e" . ebib)
   :map ebib-multiline-mode-map
   ("C-c C-c" . ebib-quit-multiline-buffer-and-save)))


;;
;; terminal stuff
;;
(use-package vterm
  :if (string-equal system-type "gnu/linux")
  :pin melpa
  :bind
  (("C-x C-t" . vterm)))

;;
;; Email configuration
;;
;;

;; put notmuch files in place
(sorend/setup-external "~/Mail/.notmuch/hooks/" "pre-new" "post-new")
(sorend/setup-external "~/" ".notmuch-config")

;; functions to setup
(use-package notmuch
  :pin melpa-stable
  :custom
  (notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a" :sort-order newest-first))))
  (message-kill-buffer-on-exit t)
  (mail-specify-envelope-from t)
  (message-send-mail-function 'smtpmail-send-it)
  (message-sendmail-f-is-evil t)
  (mail-envelope-from 'header)
  (message-sendmail-envelope-from 'header)
  :config
  (defun copy-cfg (src dest)
    (let ((full-src (if (file-name-absolute-p src) (src) (expand-file-name src "~/.emacs.d/external-configs"))))
      (copy-file full-src dest)))
  (let ((hooks-dir (expand-file-name "~/Mail/.notmuch/hooks/")))
    (progn
      (make-directory hooks-dir t) ;;
      (copy-cfg "pre-new" hooks-dir)
      (copy-cfg "post-new" hooks-dir)))
  (global-set-key (kbd "C-c m") `notmuch)
  :bind
  (:map notmuch-show-mode-map
        ("S" . (lambda () "mark message as spam"
                 (interactive)
                 (notmuch-show-tag (list "+spam" "-inbox"))))
        ("d" . (lambda() "mark message as deleted"
                 (interactive)
                 (notmuch-show-tag (list "+deleted" "-inbox"))
                 (notmuch-show-next-thread))))
  (:map notmuch-search-mode-map
        ("S" . (lambda () "mark message as spam"
                 (interactive)
                 (notmuch-search-tag (list "+spam" "-inbox"))))
        ("d" . (lambda() "mark message as deleted"
                 (interactive)
                 (notmuch-search-tag (list "+deleted" "-inbox"))
                 (notmuch-search-next-thread))))
  )

(use-package gnus-alias
  :after notmuch message
  :custom
  (gnus-alias-identity-alist
        '(("gmail" "" "Soren A D <sorend@gmail.com>" ""
           (("X-Message-SMTP-Method" . "smtp smtp.gmail.com 465 sorend@gmail.com"))
           "" "")
          ("svu" "" "Soren Atmakuri Davidsen <sorend@cs.svu-ac.in>" ""
           (("X-Message-SMTP-Method" . "smtp pixel.mxrouting.net 465 sorend@cs.svu-ac.in"))
           "" "")
          ("corp" "" "Soren Atmakuri Davidsen <soren@hamisoke.com>" ""
           (("X-Message-SMTP-Method" . "smtp pixel.mxrouting.net 465 soren@hamisoke.com"))
           "" "")
          ("sadcom" "" "Soren Atmakuri Davidsen <soren@atmakuridavidsen.com>" ""
           (("X-Message-SMTP-Method" . "smtp smtp.zoho.com 465 soren@atmakuridavidsen.com"))
           "" "")))
  (gnus-alias-identity-rules
        '(("gmail" ("from" "sorend@gmail.com" both) "gmail")
          ("corp" ("from" "soren@hamisoke.com" both) "corp")
          ("svu" ("from" "sorend@cs.svu-ac.in" both) "svu")
          ("sadcom" ("from" "soren@atmakuridavidsen.com" both) "sadcom")))
  :config
  (gnus-alias-init))


;;
;; epub
;;
;; (use-package nov)

;; (use-package nov-xwidget
;;   :demand t
;;   :after nov
;;   :config
;;   (map! :map nov-mode-map
;;         :n "gv" 'nov-xwidget-view)
;;   (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

;; latex
;; (use-package tex-site
;;   :ensure auctex
;;   :mode ("\\.tex\\'" . latex-mode)
;;   :commands (latex-mode LaTeX-mode plain-tex-mode)
;;   :config
;;   (progn
;;     (setq-default TeX-source-correlate-mode t
;;                   TeX-source-correlate-method 'synctex
;;                   reftex-plug-into-AUCTeX t
;;                   TeX-auto-save t
;;                   TeX-parse-self t
;;                   TeX-save-query nil
;;                   TeX-PDF-mode t
;;                   TeX-master nil)
;; 	(auctex-latexmk-setup)
;; 	(setq-default TeX-view-program-selection '((output-pdf "PDF Tools"))
;; 				  TeX-source-correlate-start-server t)
;;   ;; Update PDF buffers after successful LaTeX runs
;;     (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;; 	(add-hook 'LaTeX-mode-hook (lambda () (reftex-mode t) (flyspell-mode t)))
;;     )
;;   :hook
;;   ((LaTeX-mode . flyspell-mode)
;;    (LaTeX-mode . turn-on-reftex)
;;    (LaTeX-mode . LaTeX-math-mode)))

;;
;; mu4e configuration (currently not used)
;;
;; (use-package mu4e
;;   :defer 20 ; Wait until 20 seconds after startup
;;   :load-path "/snap/maildir-utils/current/share/emacs/site-lisp/mu4e"
;;   :config
;;     (require 'org-mu4e)

;;     ;; Refresh mail using isync every 10 minutes
;;     (setq mu4e-update-interval (* 10 60)
;;           mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/mbsync-gmail -a"
;;           ;; where to get attachments from
;;           mu4e-attachment-dir "~/Downloads"
;;           mu4e-save-multiple-attachments-without-asking t
;;           ;; maildir
;;           mu4e-maildir "~/Mail"
;;           ;; hack for imap to behave
;;           mu4e-change-filenames-when-moving t
;;           ;; how to chose context
;;           mu4e-context-policy 'pick-first
;;           ;; Display options
;;           mu4e-view-show-images t
;;           mu4e-view-show-addresses 't
;;           ;; Composing mail
;;           mu4e-compose-dont-reply-to-self t
;;           ;; Use mu4e for sending e-mail
;;           mail-user-agent 'mu4e-user-agent
;;           message-send-mail-function 'smtpmail-send-it
;;           ;; don't keep message buffers around
;;           message-kill-buffer-on-exit t)

;;     ;; Set up contexts for email accounts
;;     (setq mu4e-contexts
;;           `( ,(make-mu4e-context
;;                :name "Gmail"
;;                :match-func (lambda (msg) (when msg
;;                                            (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
;;                :vars '((user-full-name . "Soren A D")
;;                        (user-mail-address . "sorend@gmail.com")
;;                        (smtpmail-smtp-server . "smtp.gmail.com")
;;                        (smtpmail-smtp-user . "sorend@gmail.com")
;;                        (smtpmail-smtp-service . 465)
;;                        (smtpmail-stream-type . ssl)
;;                        (smtpmail-debug-info . t)
;;                        (mu4e-sent-folder . "/gmail/Sent Mail")
;;                        (mu4e-trash-folder . "/gmail/Trash")
;;                        (mu4e-drafts-folder . "/gmail/Drafts")
;;                        (mu4e-refile-folder . "/gmail/All Mail")
;;                        (mu4e-sent-messages-behavior . sent)
;;                        ))
;;              ,(make-mu4e-context
;;                :name "Hamisoke"
;;                :match-func (lambda (msg) (when msg
;;                                            (string-prefix-p "/hamisoke" (mu4e-message-field msg :maildir))))
;;                :vars '((user-full-name . "Soren A D")
;;                        (user-mail-address . "soren@hamisoke.com")
;;                        (smtpmail-smtp-server . "pixel.mxrouting.net")
;;                        (smtpmail-smtp-user . "soren@hamisoke.com")
;;                        (smtpmail-smtp-service . 465)
;;                        (smtpmail-stream-type . ssl)
;;                        (smtpmail-debug-info . t)
;;                        (mu4e-sent-folder . "/hamisoke/Sent")
;;                        (mu4e-trash-folder . "/hamisoke/Trash")
;;                        (mu4e-drafts-folder . "/hamisoke/Drafts")
;;                        (mu4e-refile-folder . "/gmail/All Mail")
;;                        (mu4e-sent-messages-behavior . sent)
;;                        ))
;;              ,(make-mu4e-context
;;                :name "SVU"
;;                :match-func (lambda (msg) (when msg
;;                                            (string-prefix-p "/svu" (mu4e-message-field msg :maildir))))
;;                :vars '((user-full-name . "Soren A D")
;;                        (user-mail-address . "sorend@cs.svu-ac.in")
;;                        (smtpmail-smtp-server . "pixel.mxrouting.net")
;;                        (smtpmail-smtp-user . "sorend@cs.svu-ac.in")
;;                        (smtpmail-smtp-service . 465)
;;                        (smtpmail-stream-type . ssl)
;;                        (smtpmail-debug-info . t)
;;                        (mu4e-sent-folder . "/svu/Sent")
;;                        (mu4e-trash-folder . "/svu/Trash")
;;                        (mu4e-drafts-folder . "/svu/Drafts")
;;                        (mu4e-refile-folder . "/gmail/All Mail")
;;                        (mu4e-sent-messages-behavior . sent)
;;                        ))
;;              ,(make-mu4e-context
;;                :name "Personal"
;;                :match-func (lambda (msg) (when msg
;;                                            (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
;;                :vars '(
;;                        (mu4e-sent-folder . "/Personal/Sent")
;;                        (mu4e-trash-folder . "/Personal/Deleted")
;;                        (mu4e-refile-folder . "/Personal/Archive")
;;                        ))
;;              ))

;;     ;; Prevent mu4e from permanently deleting trashed items
;;     ;; This snippet was taken from the following article:
;;     ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
;;     (defun remove-nth-element (nth list)
;;       (if (zerop nth) (cdr list)
;;         (let ((last (nthcdr (1- nth) list)))
;;           (setcdr last (cddr last))
;;           list)))
;;     (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
;;     (add-to-list 'mu4e-marks
;;                  '(trash
;;                    :char ("d" . "▼")
;;                    :prompt "dtrash"
;;                    :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
;;                    :action (lambda (docid msg target)
;;                              (mu4e~proc-move docid
;;                                              (mu4e~mark-check-target target) "-N"))))

;;     ;; Signing messages (use mml-secure-sign-pgpmime)
;;     ;; (setq mml-secure-openpgp-signers '("53C41E6E41AAFE55335ACA5E446A2ED4D940BF14"))

;;     ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;;     ;; additional non-Gmail addresses and want assign them different
;;     ;; behavior.)

;;     ;; setup some handy shortcuts
;;     ;; you can quickly switch to your Inbox -- press ``ji''
;;     ;; then, when you want archive some messages, move them to
;;     ;; the 'All Mail' folder by pressing ``ma''.
;;     (setq mu4e-maildir-shortcuts
;;           '(("/gmail/INBOX"       . ?i)
;;             ("/gmail/Sent Mail"   . ?s)
;;             ("/gmail/All Mail"   . ?a)
;;             ("/gmail/Trash"       . ?t)))

;;     (add-to-list 'mu4e-bookmarks
;;                  (make-mu4e-bookmark
;;                   :name "All Inboxes"
;;                   :query "maildir:/gmail/INBOX OR maildir:/Personal/INBOX OR maildir:/hamisoke/INBOX OR maildir:/svu/INBOX"
;;                   :key ?i))

;;     (setq dw/mu4e-inbox-query
;;           "(maildir:/Personal/INBOX OR maildir:/gmail/INBOX OR maildir:/hamisoke/INBOX OR maildir:/svu/INBOX) AND flag:unread")

;;     (defun dw/go-to-inbox ()
;;       (interactive)
;;       (mu4e-headers-search dw/mu4e-inbox-query))

;;     ;; (dw/leader-key-def
;;     ;;  "m"  '(:ignore t :which-key "mail")
;;     ;;  "mm" 'mu4e
;;     ;;  "mc" 'mu4e-compose-new
;;     ;;  "mi" 'dw/go-to-inbox
;;     ;;  "ms" 'mu4e-update-mail-and-index)

;;     ;; Start mu4e in the background so that it syncs mail periodically
;;     (mu4e t))

;; (use-package mu4e-alert
;;   :after mu4e
;;   :config
;;   ;; Show unread emails from all inboxes
;;   (setq mu4e-alert-interesting-mail-query dw/mu4e-inbox-query)
;;   ;; Show notifications for mails already notified
;;   (setq mu4e-alert-notify-repeated-mails nil)
;;   (mu4e-alert-enable-notifications))

;; init.el ends here
