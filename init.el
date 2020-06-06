;;; package -- summary
;;; Commentary:
;;; Initialization section

;;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;; code:
(package-initialize)

;;; Install use-package
(unless package-archive-contents
  (package-refresh-contents))
(if (package-installed-p 'use-package)
    nil
  (package-install 'use-package))

;;; Font
(set-face-attribute 'default nil
		    :family "JetBrains Mono"
		    :height 120
		    :weight 'regular
		    :width 'condensed)

;;; Tab
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))
(setq-default indent-tabs-mode nil)

;;; Disable messaging and splash screen
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t
      initial-buffer-choice nil)

;;; Random customization
(blink-cursor-mode 0)
(setq column-number-mode t)
(setq make-backup-files nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Windows specific customization
(if (eq system-type 'windows-nt)
      (menu-bar-mode -1))

;;; Highlight line
(global-hl-line-mode 1)

;;; Autoreload changed files
(global-auto-revert-mode t)

;;; Username
(setq user-full-name "Saurabh Sangpal"
      user-mail-address "saurabh.s@juegostudio.net")

;;; Set default frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 100))

;;; Switch to previous window
(defun prev-window()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "<s-left>") 'prev-window)
(global-set-key (kbd "<s-right>") 'other-window)

;;; Package configuration---------------------------------------------
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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package omnisharp
  :ensure t
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'before-save-hook 'omnisharp-code-format-entire-file)
  (electric-pair-local-mode 1))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp 'company-yasnippet))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package popwin
  :ensure t
  :init (popwin-mode 1))

(use-package counsel
  :ensure t
  :init
  (ivy-mode 1)
  :bind
  ("s-b" . counsel-switch-buffer))

(use-package counsel-projectile
  :ensure t)

(use-package hl-todo
  :ensure t
  :init (global-hl-todo-mode 1)
  :config
  (setq hl-todo-keyword-faces
         '(("BUG" . "#ff9999")
           ("TODO" . "#00ace6")
           ("FIXME" . "#ff9900")
           ("NOTE" . "#66ff66"))))

(use-package indent-guide
  :ensure t
  :init (indent-guide-global-mode)
  :config
  (setq indent-guide-delay 0.)
  (setq indent-guide-char "●"))

(use-package smooth-scroll
  :ensure t
  :config (smooth-scroll-mode 1))

(use-package smartparens
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package magit
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode))

;;; Themes
(use-package ample-theme
  :ensure t
  :init (progn (load-theme 'ample-flat t t)
               (enable-theme 'ample-flat))
  :defer t)

;;; Vim mode helper functions
(defun scroll-down-5x()
  "Scrolls down 5 lines."
  (interactive)
  (scroll-up-line 5))

(defun scroll-up-5x()
  "Scrolls up 5 lines."
  (interactive)
  (scroll-down-line 5))

;; Vim mode
(use-package evil
  :ensure t
  :init (evil-mode)
  :config
  (evil-define-key 'normal 'global (kbd "C-e") 'scroll-down-5x)
  (evil-define-key 'normal 'global (kbd "C-y") 'scroll-up-5x)
  (evil-define-key 'normal 'global (kbd "C-o") 'counsel-projectile-find-file)
  (evil-define-key 'normal 'global (kbd "g h") 'omnisharp-current-type-documentation)

  (evil-set-leader 'nil (kbd ",")))

(use-package evil-escape
  :ensure t
  :init (evil-escape-mode)
  :config
  (setq-default evil-escape-key-sequence ",,")
  (setq-default evil-escape-delay 0.2))

(use-package linum-relative
  :ensure t
  :init (linum-relative-global-mode)
  :config (setq linum-relative-backend 'display-line-numbers-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config (setq which-key-popup-type 'minibuffer))

(use-package wakatime-mode
  :ensure t
  :init
  (global-wakatime-mode))

(provide 'init)
;;; Init complete
