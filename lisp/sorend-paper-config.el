
;; pdf tools for viewing pdf
(use-package pdf-tools
    :config
    (pdf-loader-install))

;; use latexmk for building
(use-package auctex-latexmk)

;; reftex
(use-package reftex
  :commands turn-on-reftex
  :init
  (setq reftex-extra-bindings t
        reftex-plug-into-AUCTeX t))

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
  :after (tex-site biblio parsebib)
  :commands (ebib)
  :bind
  (("C-c e" . ebib)
   :map ebib-multiline-mode-map
   ("C-c C-c" . ebib-quit-multiline-buffer-and-save)))


;; latex
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (progn
    (setq-default TeX-source-correlate-mode t
                  TeX-source-correlate-method 'synctex
                  reftex-plug-into-AUCTeX t
                  TeX-auto-save t
                  TeX-parse-self t
                  TeX-save-query nil
                  TeX-PDF-mode t
                  TeX-master nil)
	(auctex-latexmk-setup)
	(setq-default TeX-view-program-selection '((output-pdf "PDF Tools"))
				  TeX-source-correlate-start-server t)
  ;; Update PDF buffers after successful LaTeX runs
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
	(add-hook 'LaTeX-mode-hook (lambda () (reftex-mode t) (flyspell-mode t)))
    )
  :hook
  ((LaTeX-mode . flyspell-mode)
   (LaTeX-mode . turn-on-reftex)
   (LaTeX-mode . LaTeX-math-mode)))



(provide 'sorend-paper-config)
