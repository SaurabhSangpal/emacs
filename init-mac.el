;;; package -- summary
;;; Commentary:
;; Initialization section | Manage packages

;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;; code:
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package in PACKAGES is installed.
Ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."

(unless package-archive-contents
  (package-refresh-contents))

  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
           (package-install package)
         package)))
   packages))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'magit 'csharp-mode 'flycheck 'helm 'helm-projectile 'dash 'f 'use-package 'async 'popup
                          'popwin 'company 'yasnippet 'projectile 'smartparens 'powerline 'indent-guide 'omnisharp
                          'hl-todo 'ample-theme 'ace-window 'smooth-scroll)
;; Install treemacs later

;; activate installed packages
(package-initialize)

;;---------------------------------------------------------------------------------------------------------------------;
;; Basic setup | Font, theming and properties
;; Font
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 120
                    :weight 'regular
                    :width 'condensed)

;; Tab width
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))
(setq-default indent-tabs-mode nil)

;; Disable startup messages
(setq message-log-max nil)
(kill-buffer "*Messages*")
(kill-buffer "*scratch*")

;; Stop cursor blink
(blink-cursor-mode 0)

;; Disables completions buffer
(add-hook 'minibuffer-exit-hook
	  '(lambda ()
	     (let ((buffer "*Completions*"))
	       (and (get-buffer buffer)
		        (kill-buffer buffer)))))

;; Disable splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t
      initial-buffer-choice nil)

;; Smooth scroll
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Disable bell sounds
(setq visible-bell 1)

;; Reload changed files
(global-auto-revert-mode t)

;; Enable line and column numbering
;; (global-linum-mode t)
(setq column-number-mode t)

;; Disable continued line symbol
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil right-curly-arrow))

;; Disable backup
(setq make-backup-files nil)

;; Set username and email ID
(setq user-full-name "Saurabh Sangpal"
      user-mail-address "saurabh.s@juegostudio.net")

;; Theme
(load-theme 'ample t t)
(enable-theme 'ample)

;; Set frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 125))

;; Disable menubar, scrollbar and toolbar
(tool-bar-mode -1)

;; Disable case sensitive searching
(setq case-fold-search t)

;; Use S-left S-right S-up S-down to switch frames
(windmove-default-keybindings 'ctrl)

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;---------------------------------------------------------------------------------------------------------------------;
;; Functions

;; Rebinding moving between buffers to C-Right arrow and vice versa
(defun prev-window()
  "Previous and next window switching."
  (interactive)
  (other-window -1))

;; Bind C-return to jump to new line w/o breaking current line
(defun newline-without-breaking-line ()
  "Create new line without breaking current line."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

;; Move line up and down
(defun move-line-up()
  "Move the line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(defun move-line-down()
  "Move the line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun semicolon-and-new-line()
  "Add a semicolon to the end of the line, and create new line."
  (interactive)
  (end-of-line)
  (insert ";")
  (newline-and-indent))

;;---------------------------------------------------------------------------------------------------------------------;
;; Keybindings

;; Previous window and next window
(global-set-key (kbd "<s-left>") 'prev-window)
(global-set-key (kbd "<s-right>") 'other-window)

;; Bind paragraph keys
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Remap C-x C-f to C-o
(global-set-key (kbd "C-o") 'helm-projectile-find-file)

;; Bind format to C-M-F
(global-set-key (kbd "C-M-f") 'indent-region)

;; New line without breaking next line
(global-set-key (kbd "<C-return>") 'newline-without-breaking-line)

;; Move line up and down
(global-set-key (kbd "M-w") 'move-line-up)
(global-set-key (kbd "M-s") 'move-line-down)

;; Semicolon at end of line and new line
(global-set-key (kbd "C-;") 'semicolon-and-new-line)

;; Rebind redo
(global-set-key (kbd "C-c u") 'redo)

;; Helm
(require 'helm-config)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-b") 'helm-buffers-list)
(setq helm-M-x-fuzzy-match t)

;; Ace window keybinding
(global-set-key (kbd "M-o") 'ace-window)

;; Window split
(global-set-key (kbd "C-c s") 'split-window-right)
(global-set-key (kbd "C-c b") 'split-window-below)
(global-set-key (kbd "C-c m") 'make-frame-command)
(global-set-key (kbd "C-c k") 'delete-window)

;;---------------------------------------------------------------------------------------------------------------------;
;; Packages configuration

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Omnisharp
(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  "Start omnisharp, company and flycheck."
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)
  (hl-todo-mode)

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  (electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "<f2>") 'omnisharp-rename)
  (local-set-key (kbd "C-d") 'omnisharp-go-to-definition)
  (local-set-key (kbd "C-x u") 'omnisharp-find-usages)
  )

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

; Dash syntax highlighting
(eval-after-load 'dash '(dash-enable-font-lock))

; Async
(require 'dired-async)
(dired-async-mode 1)

; Popwin
(require 'popwin)
(popwin-mode 1)

; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Smartparens
(use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))
;;(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;;(add-hook 'csharp-mode 'turn-on-smartparens-strict-mode)

;; Indent guide
(require 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-delay 0.)
(setq indent-guide-char "●")

;; hl todo
(setq hl-todo-keyword-faces
      '(("BUG"   . "#ff9999")
        ("TODO"  . "#00ace6")
        ("FIXME" . "#ff9900")
        ("NOTE"  . "#66ff66")))
(hl-todo-mode 1)

;; Smooth scrolling
(use-package smooth-scroll
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 5)
  )

(provide 'init)
;;; init ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c684e64b79a2fa042fa912b70ba14cd8da0a7175c06ac6791efee57b3209da50" default)))
 '(package-selected-packages
   (quote
    (treemacs ace-window yasnippet use-package smartparens projectile powerline popwin omnisharp magit indent-guide helm company ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
