(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Performance optimization
(setq gc-cons-threshold (* 16 gc-cons-threshold))

(unless package-archive-contents
  (package-refresh-contents))
(if (package-installed-p 'use-package)
    nil
  (package-install 'use-package))

(set-face-attribute 'default nil
                    :family "IBM Plex Mono"
                    :height 100
                    :weight 'regular
                    :width 'condensed)

(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))
(setq-default indent-tabs-mode nil)

(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t
      initial-buffer-choice nil)

(blink-cursor-mode 0)
(setq column-number-mode t)
(setq make-backup-files nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode 1)
(global-auto-revert-mode t)

;;; Set default frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 100))

(defun prev-window()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "<s-left>") 'prev-window)
(global-set-key (kbd "<s-right>") 'other-window)

(defun scroll-down-5x()
  "Scrolls down 5 lines."
  (interactive)
  (scroll-up-line 5))

(defun scroll-up-5x()
  "Scrolls up 5 lines."
  (interactive)
  (scroll-down-line 5))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  :bind
  (:map evil-normal-state-map
        ("C-e" . 'scroll-down-5x)
        ("C-y" . 'scroll-up-5x))
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-easymotion
  :after evil
  :ensure t
  :config
  (evilem-default-keybindings "SPC"))

(use-package evil-escape
  :after evil
  :ensure t
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence ",,")
  (setq-default evil-escape-delay 0.2))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-lion
  :after evil
  :ensure t
  :config
  (evil-lion-mode))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (linum-relative-global-mode))

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package omnisharp
  :ensure t
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (electric-pair-local-mode 1)
  :bind
  (("C-; b" . omnisharp-go-to-definition)
   ("C-; B" . omnisharp-go-to-definition-other-window)
   ("C-; u" . omnisharp-find-usages-with-ido)
   ("C-; i" . omnisharp-find-implementations-with-ido)))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

(use-package popwin
  :ensure t
  :init (popwin-mode 1))

(use-package counsel
  :ensure t
  :init
  (ivy-mode 1)
  :bind
  ("s-b" . counsel-switch-buffer))

(use-package hl-todo
  :ensure t
  :init (global-hl-todo-mode 1)
  :config
  (setq hl-todo-keyword-faces
         '(("BUG" . "#ff9999")
           ("TODO" . "#00ace6")
           ("FIXME" . "#ff9900")
           ("NOTE" . "#66ff66"))))

(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config (setq which-key-popup-type 'minibuffer))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package zenburn-theme
  :ensure t)
;;  :init (load-theme 'zenburn t))

(use-package leuven-theme
  :ensure t
  :init (load-theme 'leuven t))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
