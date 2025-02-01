;; init.el --- Emacs configuration

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; no warnings/compile-log
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; load host specifics
(add-to-list 'load-path "~/.emacs.d/hosts.d/")
(require (intern (downcase system-name)))

;; configure custom file
(use-package cus-edit
  :ensure nil
  :custom
  (custom-file (locate-user-emacs-file "custom.el"))
  :init
  (load custom-file :no-error-if-file-is-missing))

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))
  ;; (auto-package-update-at-time "09:00"))

;;
;; general configuration features
;;
(use-package emacs
  :ensure nil
  :config
  (when (window-system)
    (menu-bar-mode -1)
    ;; (scroll-bar-mode -1)
    (setq-default fram-title-format '("Emacs " emacs-version)))

  (recentf-mode 1)
  (add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font-15"))
  ;; (add-to-list 'default-frame-alist '(line-spacing . 0.2))

  (set-face-attribute 'default nil :height 125)
  (set-face-attribute 'default nil :height 125)

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
  (setq auth-source-debug nil ;; show output
        auth-source-do-cache t ;; cache
        auth-sources '(password-store))

  ;; ediff
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-w"
        ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; auto out of the way
  (setq backup-directory-alist `(("." . "~/.cache/emacs-backups"))
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

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
  ;; (load-theme 'modus-vivendi t)
  (load-theme 'modus-operandi-tinted t)
  (setq modus-themes-org-blocks 'tinted)
  ;; yes/no -> y/n
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq revert-without-query '(".*pdf$"))
  ;; display time mode on
  ;; (display-time-mode 1)
  )

(use-package delsel
  :ensure nil ; it is built-in
  :hook (after-init . delete-selection-mode))


(defun sorend/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'sorend/keyboard-quit-dwim)

;; Enable vertico
(use-package vertico
  :ensure t
  :vc (:url "https://github.com/minad/vertico"
            :rev :newest
            :lisp-dir "extensions/")
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("M-DEL" . #'vertico-directory-delete-word)
              ("RET" . #'vertico-directory-enter)
              ("DEL" . #'vertico-directory-delete-char))
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;; move-text
(use-package move-text
  :ensure t
  :init
  (move-text-default-bindings))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; marginalia shows documentation while completing
(use-package marginalia
  :ensure t
  :after vertico
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :after (vertico wgrep)
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
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  ;; :straight t
  :after (vertico wgrep)
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ([remap yank-pop] . consult-yank-pop)
         ;; ([remap yank] . consult-yank-pop)
         ([remap isearch-forward] . consult-line)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap goto-line] . consult-goto-line)
         ([remap ibuffer] . consult-buffer)
         ("C-f" . consult-ripgrep)
         ("C-S-f" . consult-find)
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

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;; (use-package beacon
;;   :config
;;   (beacon-mode 1))

(use-package dash
  :ensure t)

;; (use-package better-defaults
;;   :straight (:host nil :repo "https://github.com/emacsmirror/better-defaults")
;; ;;  :straight (:host nil :repo "https://git.sr.ht/~technomancy/better-defaults")
;;   :config
;;   (setq backup-directory-alist
;;         `((".*" . ,temporary-file-directory)))
;;   (setq auto-save-file-name-transforms
;;         `((".*" ,temporary-file-directory t))))

(use-package corfu
  :ensure t
  ;; :vc (corfu :files (:defaults "extensions/*")
  ;;                  :includes (corfu-popupinfo))
  ;; TAB-and-Go customizations
  ;; :custom
  ;; (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)
  ;; (corfu-auto-prefix 2)
  ;; (corfu-auto-delay 0.5)
  ;; (corfu-exit-at-boundary 'separator)
  ;; (corfu-echo-documentation 0.25)
  ;; (corfu-preview-current 'insert)
  ;; (corfu-preselect-first nil)
    ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("<tab>" . corfu-complete))
        ;; ("M-SPC" . corfu-insert-separator)
        ;; ("RET" . nil)
        ;; ("S-<return>" . corfu-insert)
        ;; ("TAB" . corfu-next)
        ;; ([tab] . corfu-next)
        ;; ("S-TAB" . corfu-previous)
        ;; ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)
  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; Part of corfu
;; (use-package corfu-popupinfo
;;   :after corfu
;;   :hook (corfu-mode . corfu-popupinfo-mode)
;;   :custom
;;   (corfu-popupinfo-delay '(0.25 . 0.1))
;;   (corfu-popupinfo-hide nil)
;;   :config
;;   (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :ensure t
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; search improvement
(use-package ctrlf
  :ensure t
  :config
  (ctrlf-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; setup tramp
(use-package tramp
  ;; :straight (:type built-in)
  :custom
  (tramp-default-method "ssh")
  (tramp-auto-save-directory "~/.cache/tramp-autosave-dir")
  (password-cache-expiry 3600)
  (recentf-keep '(file-remote-p file-readable-p)))

;; setup ace-window
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;; needed for groovy mode
(use-package cl-lib
  :ensure t)
(use-package groovy-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t)


;;
;; Multiple cursors
;;
;; (use-package multiple-cursors
;;   :bind

;;   (("C-c SPC" . mc/edit-lines)
;;    ("C-c C-<down>" . mc/mark-next-like-this)
;;    ("C-c C-<up>" . mc/mark-previous-like-this)
;;    ("C-c C-<right>" . mc/mark-all-like-this)))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :init
  (global-set-key (kbd "C-d") 'mc/mark-next-like-this))

;;
;; Flymake configuration
;;
(use-package flymake
  ;; :straight (:type built-in)
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
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  ;; disable auto-revert-mode (a bit faster w/o)
  (magit-auto-revert-mode 0)
  ;; display the magit in a full screen buffer
  (setq magit-display-buffer-function
        'magit-display-buffer-fullframe-status-v1))

(use-package difftastic
  :ensure t
  :bind (:map magit-blame-read-only-mode-map
         ("D" . difftastic-magit-show)
         ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))


(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package dired
  ;; :straight (:type built-in)
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-make-directory-clickable t))

(use-package dired-single
  :ensure t
  :vc (:url "https://codeberg.org/amano.kenji/dired-single" :rev :newest)
  :after dired)


(global-set-key (kbd "C-c r")  'rename-visited-file)


;; (use consult-git-grep instead M-s-G)
;; (use-package magit-find-file
;;   :bind ("C-c p" . magit-find-file-completing-read))

(use-package emojify
  :ensure t
  :custom
  (emojify-download-emojis-p t)
  :hook
  (after-init . global-emojify-mode))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))


(when (string= system-type "gnu/linux")
  (use-package exec-path-from-shell
    :ensure t
    :config
    (dolist (shell-variable '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "BROWSER" "CI"
                              "GOPATH" "LANG" "MANPATH" "PATH"))
      (add-to-list 'exec-path-from-shell-variables shell-variable))
    (exec-path-from-shell-initialize)))

;; spell checking
(use-package jinx
  :ensure t
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
              ("C-c b" . #'xref-go-back)
              ("C-S-<down-mouse-1>" . #'xref-find-references)
              ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t)
  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  )

;;
;; rust programming
;;
(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'rust-ts-mode-hook 'eglot-ensure))

;;
;; Go programming
;;
(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook #'eglot-ensure)
  (add-hook 'go-ts-mode-hook #'eglot-ensure)
  )


;; optional if you want which-key integration
(use-package which-key
  :ensure nil
  :config
  (setq which-key-show-erly-on-C-h t)
  (setq which-key-idle-delay 3)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(use-package python-pytest
  :ensure t
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
  :ensure t
  :hook
  (markdown-mode . visual-line-mode)
  :init
  (setq markdown-command "multimarkdown"))

(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode)

;; yaml
(use-package yaml-mode
  :ensure t)

;; toml
(use-package toml-mode
  :ensure t)

;;
;; LaTeX setup
;;
;;
;; LaTeX setup
;;

(use-package pdf-tools
  :ensure t
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
  :ensure t
  :after (tex-site tex-buf)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package reftex
  :defer t
  :custom
  (reftex-cite-prompt-optional-args t))

(use-package cdlatex
  :ensure t)

(use-package auto-dictionary
  :ensure t
  :init
  (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

;; (use-package company-auctex
;;   :ensure t
;;   :after tex-site company yasnippet
;;   :hook LaTeX-mode
;;   :init (company-auctex-init))

(use-package tex-site
  ;; :straight auctex
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

(use-package biblio
  :ensure t)

;; (use-package biblio-zotero
;;   :straight (biblio-zotero :type git :host github :repo "gkowzan/biblio-zotero")
;;   :commands (biblio-zotero-insert-bibtex))

(use-package parsebib)

(use-package ebib
  :after (tex biblio parsebib)
  :commands (ebib)
  :bind
  (("C-c e" . ebib)
   :map ebib-multiline-mode-map
   ("C-c C-c" . ebib-quit-multiline-buffer-and-save)))

(unless (boundp 'my-org-directory)
  (setq my-org-directory (expand-file-name "~/Mega/notes/")))

;; functions to setup
(use-package notmuch
  :vc (:url "https://github.com/notmuch/notmuch" :rev :newest)
  :if feature-notmuch?
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
  :ensure t
  :if feature-notmuch?
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
  ;; :straight (:type built-in)
  :if feature-notmuch?
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
  ;; :straight (notmuch-x :host github :repo "bcardoso/notmuch-x")
  :vc (:url "https://github.com/bcardoso/notmuch-x" :rev :newest)
  :if feature-notmuch?
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
  ;; :straight (:type built-in)
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
  (org-format-latex-options (list :foreground 'auto
                                  :background 'auto
                                  :scale 1.0
                                  :html-foreground "Black"
                                  :html-background "Transparent"
                                  :html-scale 1.0
                                  :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
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
     (python . t)
     (ditaa . t)))
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

(when window-system
  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo") '(:font "ETBembo"))
                ((x-list-fonts "EB Garamond") '(:font "EB Garamond"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil)))))))


(use-package org-contrib
  :ensure t
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode))

(use-package org-modern
  :ensure t
  :after org
  :config
  (global-org-modern-mode))

;; (use-package org-latex-preview
;;   :after org
;;   :config
;;   ;; Increase preview width
;;   (plist-put org-latex-preview-appearance-options :page-width 0.8)
;;   ;; Use dvisvgm to generate previews
;;   ;; You don't need this, it's the default:
;;   (setq org-latex-preview-process-default 'dvisvgm)
;;   ;; Turn on auto-mode, it's built into Org and much faster/more featured than
;;   ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
;;   (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)
;;   ;; Block C-n and C-p from opening up previews when using auto-mode
;;   (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
;;   (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)
;;   ;; Enable consistent equation numbering
;;   (setq org-latex-preview-numbered t)
;;   ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
;;   ;; fragment and updates the preview in real-time as you edit it.
;;   ;; To preview only environments, set it to '(block edit-special) instead
;;   (setq org-latex-preview-live t)
;;   ;; More immediate live-previews -- the default delay is 1 second
;;   (setq org-latex-preview-live-debounce 0.25))

;; org-present
(use-package org-present
  :ensure t
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
  :ensure t
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
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(when feature-wsl?
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
      (if arg (kill-new clip)))))

(use-package fish-mode)


(use-package 1password
  :ensure t
  ;; :straight '(1password :host github :repo "justinbarclay/1password.el" :branch "main")
  :vc (:url "https://github.com/justinbarclay/1password.el" :rev :newest)
  :if feature-1password?
  :init
  (1password-enable-auth-source))
  ;; :custom
  ;; ((1password-results-formatter . '1password-colour-formatter)))


(use-package gptel
  :ensure t)


(defun crontab-e ()
    "Run `crontab -e' in a emacs buffer."
    (interactive)
    (with-editor-async-shell-command "crontab -e"))


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
;; End:
