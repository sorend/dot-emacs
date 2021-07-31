
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
  :hook
  ((go-mode . lsp)
   (python-mode . lsp))
  :commands (lsp lsp-deferred))

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
  ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; if you are helm user
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;;(use-package lsp-ivy
;;  :commands lsp-ivy-workspace-symbol)

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


;; lsp java
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

(use-package dap-java
  :ensure nil)


(use-package lsp-latex
  :after tex-site
  :hook
  ((tex-mode . lsp)
   (latex-mode . lsp)
   (bibtex-mode . lsp)))


(provide 'sorend-lsp-config)
