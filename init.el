;; init.el --- Emacs configuration

;; conditional for laptop or not
(setq is-mine? (string= system-name 'rebala))
(setq is-bankdata? (string= system-type 'windows-nt))
(message (format "is-mine? %s  is-bankdata? %s" is-mine? is-bankdata?))

;; proxy at bankdata
;;(if is-bankdata?
;;    (setq url-proxy-services
;;          '(("no_proxy" . "\\(localhost\\|bdpnet.dk\\|bdunet.dk\\)")
;;            ("http" . "httpproxy.bdpnet.dk:8080")
;;            ("https" . "httpproxy.bdpnet.dk:8080"))))

;; use develop because emacs29 fix is not in master yet
(setq straight-repository-branch "develop")

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
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
  :custom
  (vertico-cycle t)
;;:bind (:map vertico-map
;;			  ("C-j" . vertico-next)
;;			  ("C-k" . vertico-previous))
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :straight (:type built-in)
  :config
  (when (window-system)
    (menu-bar-mode -1)
    ;; (scroll-bar-mode -1)
    (setq-default fram-title-format '("Emacs " emacs-version)))

  (recentf-mode 1)
  (add-to-list 'default-frame-alist '(font . "iosevka-15"))
  ;; (add-to-list 'default-frame-alist '(line-spacing . 0.2))

  (when is-mine? ;; for laptop
    (set-face-attribute 'default nil :height 125))
  (when is-bankdata?
    (set-face-attribute 'default nil :height 125))

  (setq inhibit-splash-screen t)
  (setq inhibit-startup-message t)

  ;; (setq garbage-collection-messages t)
  (add-function :after
                after-focus-change-function
                (lambda () (unless (frame-focus-state) (garbage-collect))))
  ;; (add-function :after after-focus-change-function 'garbage-collect)

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
  ;; display time mode on
  ;; (display-time-mode 1)
  )

(use-package gcmh
  :config
  (gcmh-mode 1))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; marginalia shows documentation while completing
(use-package marginalia
  :straight t
  :after vertico
  :config
  (marginalia-mode))

(use-package embark
  :straight t
  :after vertico
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
  :straight t
  :after vertico
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ([remap yank-pop] . consult-yank-pop)
         ;; ([remap yank] . consult-yank-pop)
         ([remap isearch-forward] . consult-line)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap goto-line] . consult-goto-line)
         ([remap ibuffer] . consult-buffer)
         ;; ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ;; ("C-c k" . consult-kmacro)
         ;; ;; C-x bindings (ctl-x-map)
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; ;; M-g bindings (goto-map)
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; ;; M-s bindings (search-map)
         ;; ("M-s d" . consult-find)
         ;; ("M-s D" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s m" . consult-multi-occur)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ("C-f" . consult-ripgrep)
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
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark ;; consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

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
  :straight (:host nil :repo "https://github.com/emacsmirror/better-defaults")
;;  :straight (:host nil :repo "https://git.sr.ht/~technomancy/better-defaults")
  :config
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.5)
  (corfu-exit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
    ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("M-SPC" . corfu-insert-separator)
        ("RET" . nil)
        ("S-<return>" . corfu-insert)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (corfu-history-mode))


;; search improvement
(use-package ctrlf
  :config
  (ctrlf-mode +1))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; setup tramp
(use-package tramp
  :straight (:type built-in)
  :custom
  (tramp-default-method "ssh")
  (tramp-auto-save-directory "~/.cache/tramp-autosave-dir")
  (password-cache-expiry 3600)
  (recentf-keep '(file-remote-p file-readable-p)))

;; setup ace-window
(use-package ace-window
  :bind ("M-o" . ace-window))

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
;; Flymake configuration
;;
(use-package flymake
  :straight (:type built-in)
  :bind
  (("C-c C-1 n" . flymake-goto-next-error)
   ("C-c C-1 p" . flymake-goto-prev-error)))

;; (use-package flymake-ruff
;;   :straight (flymake-ruff
;;              :type git
;;              :host github
;;              :repo "erickgnavar/flymake-ruff"))

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

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired
  :straight (:type built-in))

(use-package dired-single
  :after dired)


(defun wsl-copy-region-to-clipboard (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

(defun wsl-clipboard-to-string ()
  "Return Windows clipboard as string."
  (let ((coding-system-for-read 'dos))
    (substring				; remove added trailing \n
     (shell-command-to-string
      "powershell.exe -Command Get-Clipboard") 0 -1)))

(defun wsl-paste-from-clipboard (arg)
  "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
  (interactive "P")
  (let ((clip (wsl-clipboard-to-string)))
    (insert clip)
    (if arg (kill-new clip))))


(global-set-key (kbd "C-c r")  'rename-visited-file)

;; (when is-bankdata?
;;   ;; keychains
;;   (defun sorend/keychain-file-contents (filename)
;;     "Return the contents of FILENAME."
;;     (with-temp-buffer
;;       (insert-file-contents filename)
;;       (buffer-string)))

;;   (defun sorend/keychain-refresh-environment ()
;;     (interactive)
;;     (let* ((ssh (sorend/keychain-file-contents (car (file-expand-wildcards "~/.keychain/*-sh" t)))))
;;       (list (and ssh
;;                  (string-match "SSH_AUTH_SOCK[=\s]\\([^\s;\n]*\\)" ssh)
;;                  (setenv       "SSH_AUTH_SOCK" (match-string 1 ssh)))
;;             (and ssh
;;                  (string-match "SSH_AGENT_PID[=\s]\\([0-9]*\\)?" ssh)
;;                  (setenv       "SSH_AGENT_PID" (match-string 1 ssh))))))

;;   (sorend/keychain-refresh-environment))


;; (use consult-git-grep instead M-s-G)
;; (use-package magit-find-file
;;   :bind ("C-c p" . magit-find-file-completing-read))

(use-package emojify
  :custom
  (emojify-download-emojis-p t)
  :hook
  (after-init . global-emojify-mode))

(use-package nyan-mode
  :config
  (nyan-mode))


(when (string= system-type "gnu/linux")
  (use-package exec-path-from-shell
    :config
    (dolist (shell-variable '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "BROWSER" "CI"
                              "GOPATH" "LANG" "MANPATH" "PATH"))
      (add-to-list 'exec-path-from-shell-variables shell-variable))
    (exec-path-from-shell-initialize)))

;; spell checking
(use-package jinx
  :if is-mine?
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-l" . jinx-languages)))

;;
;; use tree-sitter mode instead of "normal" mode
;;
;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;; lsp
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c a r" . #'eglot-rename)
              ("C-<down-mouse-1>" . #'xref-find-definitions)
              ("C-S-<down-mouse-1>" . #'xref-find-references)
              ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-autoshutdown t))

(use-package ledger-mode)

(use-package beancount
  :straight (beancount-mode
             :type git
             :host github
             :repo "beancount/beancount-mode"))


;;
;; rust programming
;;
(use-package rust-mode
  :init
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'rust-ts-mode-hook 'eglot-ensure))

;;
;; Go programming
;;
(use-package go-mode
  :init
  (add-hook 'go-mode-hook #'eglot-ensure)
  (add-hook 'go-ts-mode-hook #'eglot-ensure)
  )


;; optional if you want which-key integration
(use-package which-key
    :config
    (setq which-key-show-erly-on-C-h t)
    (setq which-key-idle-delay 3)
    (setq which-key-idle-secondary-delay 0.05)
    (which-key-mode))

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
  :if is-mine?
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  (pdf-misc-print-programm "/usr/bin/lpr")
  (pdf-misc-print-programm-args (quote ("-o media=A4" "-o fitplot")))
  :config
  (pdf-tools-install t) ;; install without asking
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

(setq my-org-directory (expand-file-name "~/Mega/notes/"))

(when is-mine?
  ;; put notmuch files in place
  ;; (sorend/setup-external "~/Mail/.notmuch/hooks/" "pre-new" "post-new")
  ;; (sorend/setup-external "~/" ".notmuch-config")

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
    (message-sendmail-envelope-from 'header))

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
    :custom
    (notmuch-x-auto-update nil)
    (notmuch-x-auto-update-mode nil)
    :bind (("C-c m"            . notmuch)
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
      (notmuch-x-tag-toggle "unread"))

    (defun sorend/notmuch-tag-toggle-flagged ()
      "Toggle 'flagged' tag."
      (interactive)
      (notmuch-x-tag-toggle "flagged"))

    (defun sorend/notmuch-tag-archived ()
      "Tag thread as 'archived'."
      (interactive)
      (notmuch-x-tag-thread '("-inbox" "-todo" "-trash" "-unread") t))

    (defun sorend/notmuch-tag-todo ()
      "Tag selected message(s) as 'todo'."
      (interactive)
      (notmuch-x-tag '("+todo" "+inbox" "-trash")))

    (defun sorend/notmuch-tag-inbox ()
      "Tag selected message(s) as 'inbox'."
      (interactive)
      (notmuch-x-tag '("+inbox" "-trash")))

    (defun sorend/notmuch-tag-spam ()
      "Tag selected message(s) as 'inbox'."
      (interactive)
      (notmuch-x-tag '("-inbox" "+spam")))

    (defun sorend/notmuch-tag-trash ()
      "Tag selected message(s) as 'trash'."
      (interactive)
      (notmuch-x-tag '("+trash" "-inbox" "-todo" "-unread"))))


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
    (org-directory my-org-directory)
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
       (shell . t)
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
  ;; (use-package org-superstar
  ;;   :after org
  ;;   :custom
  ;;   (org-superstar-special-todo-items t)
  ;;   :config
  ;;   (add-hook 'org-mode-hook (lambda ()
  ;;                              (org-superstar-mode 1))))


  (use-package org-modern
    :after org
    :config
    (global-org-modern-mode))

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
    :bind
    (:map org-mode-map
          :package org
          ("C-c b" . #'org-cite-insert))
    :custom
    (org-cite-global-bibliography '("~/Mega/research/bib/references.bib"))
    (citar-bibliography org-cite-global-bibliography)
    (org-cite-insert-processor 'citar)
    (org-cite-follow-processor 'citar)
    (org-cite-activate-processor 'citar)
    (org-cite-export-processors '((latex biblatex) (t csl)))
    :config
    (require 'oc-biblatex))

  (use-package citar-embark
    :after citar embark
    :no-require
    :config (citar-embark-mode)) )

;; (use-package calfw)

;; (use-package calfw-ical
;;   :after calfw
;;   :config
;;   (defun my-open-calendar ()
;;     (interactive)
;;     (cfw:open-calendar-buffer
;;      :contents-sources
;;      (list
;;       (cfw:ical-create-source "DK" "https://www.officeholidays.com/ics/denmark" "IndianRed")
;;       (cfw:ical-create-source "IN" "https://www.officeholidays.com/ics/india" "IndianRed")
;;       ))))



;;
;; welcome startup
;;
(find-file (expand-file-name "welcome.org" my-org-directory))

;; Local Variables:
;; jinx-local-words: "linux"
;; End:
