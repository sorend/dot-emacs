

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


(provide 'sorend-paper-config)
