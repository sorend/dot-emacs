;;
;; general configuration features 
;;

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
;; refresh whee
(global-set-key (kbd "<f5>") 'revert-buffer)

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
  :after lsp-mode
  :hook
  ((prog-mode . company-mode))
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
   :map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-to-list 'company-backends 'company-ansible)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package crontab-mode)

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
;; Multiple cursors
;;
(use-package multiple-cursors
  :bind
  (("C-c SPC" . mc/edit-lines)
   ("C-c C-<down>" . mc/mark-next-like-this)
   ("C-c C-<up>" . mc/mark-previous-like-this)
   ("C-c C-<right>" . mc/mark-all-like-this)))
 

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


(provide 'sorend-general-config)
