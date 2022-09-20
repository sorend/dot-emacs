;; init.el --- Emacs configuration

;; for work (only at work I use windows)
(if (string-equal system-type "windows-nt")
    (setq url-proxy-services
          '(("no_proxy" . "\\(localhost\\|bdpnet.dk\\|bdunet.dk\\)")
            ("http" . "httpproxy.bdpnet.dk:8080")
            ("https" . "httpproxy.bdpnet.dk:8080"))))

;; straight setup
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

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
  :straight (:type built-in)
  :config
  (progn
    (when (window-system)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (setq-default fram-title-format '("Emacs " emacs-version)))

    (recentf-mode 1)
    (add-to-list 'default-frame-alist '(font . "JetBrains Mono-14"))
    ;; (add-to-list 'default-frame-alist '(line-spacing . 0.2))

    (when (string-equal system-name "rebala") ;; for laptop
      (set-face-attribute 'default nil :height 125))

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

    ;; ediff
    (setq ediff-split-window-function 'split-window-horizontally
          ediff-diff-options "-w"
          ediff-window-setup-function 'ediff-setup-windows-plain)

    ;; diable warnings
    (setq warning-minimum-level :error)

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
    ;; (load-theme 'modus-operandi t)
    (setq modus-themes-org-blocks 'tinted)
    ;; yes/no -> y/n
    (defalias 'yes-or-no-p 'y-or-n-p)
    (setq revert-without-query '(".*pdf$"))
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

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :demand t
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
         ("M-r" . consult-history))


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
  :straight (:host nil :repo "https://git.sr.ht/~technomancy/better-defaults")
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

;; disable for now, use project.el (use-package projectile)

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

;; (use consult-git-grep instead M-s-G)
;; (use-package magit-find-file
;;   :bind ("C-c p" . magit-find-file-completing-read))

(use-package emojify
  :custom
  (emojify-download-emojis-p t)
  :hook
  (after-init . global-emojify-mode))

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
  ;; (pdf-tools-install) ;; only do it on demand
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
  :straight auctex
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

(use-package biblio-zotero
  :straight (biblio-zotero :type git :host github :repo "gkowzan/biblio-zotero")
  :commands (biblio-zotero-insert-bibtex))

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
  :bind
  (("C-x C-t" . vterm)))

;; dired
(use-package dired
  :straight (:type built-in)
  :bind
  (("C-x C-j" . dired-jump)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-single
  :after dired)

;;
;; Email configuration
;;
;;

;; put notmuch files in place
(sorend/setup-external "~/Mail/.notmuch/hooks/" "pre-new" "post-new")
(sorend/setup-external "~/" ".notmuch-config")

;; functions to setup
(use-package notmuch
  :straight (:host github :repo "notmuch/notmuch")
  :after message gnus-alias
  :custom
  (notmuch-fcc-dirs '(("sorend@gmail.com" . nil)))
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
  ;; :config
  ;; (global-set-key (kbd "C-c m") `notmuch)
  ;; :bind
  ;; (:map notmuch-show-mode-map
  ;;       ("S" . (lambda () "mark message as spam"
  ;;                (interactive)
  ;;                (notmuch-show-tag (list "+spam" "-inbox"))))
  ;;       ("d" . (lambda() "mark message as deleted"
  ;;                (interactive)
  ;;                (notmuch-show-tag (list "+deleted" "-inbox"))
  ;;                (notmuch-show-next-thread))))
  ;; (:map notmuch-search-mode-map
  ;;       ("S" . (lambda () "mark message as spam"
  ;;                (interactive)
  ;;                (notmuch-search-tag (list "+spam" "-inbox"))))
  ;;       ("d" . (lambda() "mark message as deleted"
  ;;                (interactive)
  ;;                (notmuch-search-tag (list "+deleted" "-inbox"))
  ;;                (notmuch-search-next-thread))))
  )



;; use gnus-alias X-Message-SMTP-Header
(use-package gnus-alias
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

;; allow to switch identity while writing mail
(use-package message
  :straight (:type built-in)
  :bind
  (:map message-mode-map
        ("C-c C-i" . sorend/message-switch-identity))
  :config
  (defun sorend/message-switch-identity ()
    (interactive)
    (message-remove-header "Fcc")
    (message-remove-header "Organization")
    (gnus-alias-select-identity)
    (notmuch-fcc-header-setup)))

;; notmuch-x
(use-package notmuch-x
  :straight (notmuch-x :host github :repo "bcardoso/notmuch-x")
  :after notmuch
  :bind (("C-c m"            . notmuch-x-run-notmuch)
         ("C-c M"            . notmuch-x-update-dwim)
         ("C-x m"            . notmuch-mua-new-mail)
         (:map notmuch-search-mode-map
               ("Q"          . notmuch-x-kill-all-search-buffers)
               ("S"          . notmuch-x-edit-current-search)
               ("U"          . notmuch-unthreaded)
               ("u"          . sorend/notmuch-tag-toggle-unread)
               ("f"          . sorend/notmuch-tag-toggle-flagged)
               ("a"          . sorend/notmuch-tag-archived)
               ("T"          . sorend/notmuch-tag-todo)
               ("i"          . sorend/notmuch-tag-inbox)
               ("d"          . sorend/notmuch-tag-trash))
         (:map notmuch-show-mode-map
               ("<C-return>" . notmuch-x-toggle-thread-visibility)
               ("<RET>"      . notmuch-x-toggle-message-or-browse-url)
               ("<tab>"      . notmuch-x-next-button-or-link)
               ("<backtab>"  . notmuch-x-previous-button-or-link)
               ("n"          . notmuch-show-next-message)
               ("N"          . notmuch-show-next-open-message)
               ("p"          . notmuch-show-previous-message)
               ("P"          . notmuch-show-previous-open-message)
               ("o"          . notmuch-x-view-part-in-browser)
               ("u"          . sorend/notmuch-tag-toggle-unread)
               ("f"          . sorend/notmuch-tag-toggle-flagged)
               ("a"          . sorend/notmuch-tag-archived)
               ("T"          . sorend/notmuch-tag-todo)
               ("i"          . sorend/notmuch-tag-inbox)
               ("d"          . sorend/notmuch-tag-trash)))
  :config
  (defun sorend/notmuch-tag-toggle-unread ()
    "Toggle 'unread' tag."
    (interactive)
    (notmuch-x-tag-toggle-dwim "unread"))

  (defun sorend/notmuch-tag-toggle-flagged ()
    "Toggle 'flagged' tag."
    (interactive)
    (notmuch-x-tag-toggle-dwim "flagged"))

  (defun sorend/notmuch-tag-archived ()
    "Tag thread as 'archived'."
    (interactive)
    (notmuch-x-tag-thread-dwim '("-inbox" "-todo" "-trash" "-unread") t))

  (defun sorend/notmuch-tag-todo ()
    "Tag selected message(s) as 'todo'."
    (interactive)
    (notmuch-x-tag-dwim '("+todo" "+inbox" "-trash")))

  (defun sorend/notmuch-tag-inbox ()
    "Tag selected message(s) as 'inbox'."
    (interactive)
    (notmuch-x-tag-dwim '("+inbox" "-trash")))

  (defun sorend/notmuch-tag-spam ()
    "Tag selected message(s) as 'inbox'."
    (interactive)
    (notmuch-x-tag-dwim '("-inbox" "+spam")))
  
  (defun sorend/notmuch-tag-trash ()
    "Tag selected message(s) as 'trash'."
    (interactive)
    (notmuch-x-tag-dwim '("+trash" "-inbox" "-todo" "-unread"))))

;;
;; org related configuration
;;




(use-package org
  :straight (:type built-in)
  :demand t
  :custom
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))
  (org-directory (expand-file-name "~/Mega/notes/"))
  (org-default-notes-file org-directory)
  (org-latex-pdf-process '("latexmk -f -pdf -%latex -bibtex -interaction=nonstopmode -output-directory=%o %f"))
  ;; (org-capture-templates
  ;;  '(("t" "Todo" entry (file+headline (expand-file-name "gtd.org" org-directory) "Tasks"))
  ;;    ("a" "Article" entry ))
  :config
  (defun sorend/org-grep (&optional initial)
    (interactive "P")
    (consult-ripgrep org-directory initial))
  ;; (plist-put org-format-latex-options :scale 2)
  (require 'org-tempo)  ;; make
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  (add-hook 'org-mode-hook #'visual-line-mode)
  ;; latex headlines can be ignored
  ;; latex can handle IEEEtran class
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("IEEEtran"
                   "\\documentclass{IEEEtran}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  :bind
  (("C-c n c" . org-capture)
   ("C-c n f" . sorend/org-grep)))

(use-package org-contrib
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

;; Nice bullets
(use-package org-superstar
  :after org
  :custom
  (org-superstar-special-todo-items t)
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

;; org-present
(use-package org-present
  :after org
  :hook
  (org-present-mode . (lambda ()
                        (display-line-numbers-mode -1)
                        (org-present-big)
                        (org-display-inline-images)
                        (org-present-hide-cursor)
                        (org-present-read-only)))
  (org-present-mode-quit . (lambda ()
                             (display-line-numbers-mode +1)
                             (org-present-small)
                             (org-remove-inline-images)
                             (org-present-show-cursor)
                             (org-present-read-write))))

(use-package citar
  :after org
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (citar-bibliography '("~/Mega/research/bib/references.bib"))
  :config
  (setq
        org-cite-export-processors '((latex biblatex) (t csl))
        org-support-shift-select t)
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)
  (require 'oc)
  (require 'oc-biblatex)
  (require 'oc-csl)
  (require 'oc-natbib))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;;
;; notebooks
;;
;; (use-package ein)

;;
;; welcome startup
;;
(find-file (expand-file-name "welcome.org" org-directory))

;;
;; epub
;;
;; (use-package nov)

;; init.el ends here
