
;;
;; LaTeX setup
;;

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  (add-hook 'pdf-view-mode-hook (lambda ()
				  (bms/pdf-midnite-amber))) ; automatically turns on midnight-mode for pdfs
  )

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)) ;; Prompt for empty optional arguments in cite

(use-package auto-dictionary
  :ensure t
  :init(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package company-auctex
  :ensure t
  :after tex company yasnippet
  :hook LaTeX-mode
  :init (company-auctex-init))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (progn
	(setq TeX-source-correlate-mode t)
	(setq TeX-source-correlate-method 'synctex)
	(setq TeX-auto-save t)
	(setq TeX-parse-self t)
	(setq-default TeX-master "paper.tex")
	(setq reftex-plug-into-AUCTeX t)
	(pdf-tools-install)
	(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
		  TeX-source-correlate-start-server t)
	;; Update PDF buffers after successful LaTeX runs
	(add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer)
	(add-hook 'LaTeX-mode-hook
		      (lambda ()
			    (reftex-mode t)
			    (flyspell-mode t)))
	    ))


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


;; (use-package lsp-latex
;;   :after tex
;;   :hook
;;   ((tex-mode . lsp)
;;    (latex-mode . lsp)
;;    (bibtex-mode . lsp)))



(provide 'sorend-paper-config)
