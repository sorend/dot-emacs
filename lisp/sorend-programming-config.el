;;
;; javascript and web
;;
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
  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-enable-auto-pairing t
                web-mode-enable-auto-closing t
                web-mode-enable-current-element-highlight t
                web-mode-enable-current-column-highlight t))

(use-package css-mode
  :commands css-mode
  :init
  (setq-default css-indent-offset 2)
  :config
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

;; yaml
(use-package yaml-mode)

;; Dockerfile support
(use-package dockerfile-mode
  :mode (("Dockerfile'" . dockerfile-mode)))

;; go mode
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


(use-package python-mode
  :after (lsp-mode)
  :hook
  ((python-mode . lsp-deferred)
   (before-save . lsp-format-buffer)
   (before-save . lsp-organize-imports))
  :config
  ;; keybindings
  (add-to-list 'lsp-enabled-clients 'pylsp))

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

;;(use-package tox
;;  :config
;;  (setq tox-runner 'py.test))


(provide 'sorend-programming-config)
