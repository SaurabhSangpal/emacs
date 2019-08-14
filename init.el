; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."

(unless package-archive-contents
  (package-refresh-contents))

  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'magit 'iedit 'magit 'csharp-mode 'flycheck 'helm 'dash 'f 'use-package 'async 'popup 'popwin 'helm-core 'company 'yasnippet 'smartparens 'auto-complete 'powerline 'ample-theme)

;; activate installed packages
(package-initialize)

;---------------------------------------------------------------------------------------------------------------------;
; Font
(add-to-list 'default-frame-alist
  '(font . "InputMono-11"))

; Tab width
(setq tab-width 4)
(setq-default c-basic-offset 4
              c-indent-level 4
              c-brace-imaginary-offset 0
              c-brace-offset -4
              c-argdecl-indent 4
              c-label-offset -4
              c-continued-statement-offset 4)

; Disable startup messages
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(kill-buffer "*scratch*")

; Disables completions buffer
(add-hook 'minibuffer-exit-hook
	  '(lambda ()
	     (let ((buffer "*Completions*"))
	       (and (get-buffer buffer)
		        (kill-buffer buffer)))))

; Disable splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t
      initial-buffer-choice nil)

; Disable bell sounds
(setq visible-bell 1)

; Enable line numbering
(global-linum-mode t)

; Disable continued line symbol
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil right-curly-arrow))

; Disable backup
(setq make-backup-files nil)

; Set username and email ID
(setq user-full-name "Saurabh Sangpal"
      user-mail-address "saurabhsangpal@gmail.com")

; Adding clang format support
(load "C:/Program Files/LLVM/share/clang/clang-format.el")
(global-set-key [C-tab] 'clang-format-buffer)

; Rebinding moving between buffers to C-Right arrow and vice versa
(defun prev-window()
  (interactive)
  (other-window -1))
(global-set-key "\033[5D" 'prev-window)
(global-set-key "\033[5C" 'other-window)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit))))

; Theme
(load-theme 'ample t t)
(enable-theme 'ample)

; Set frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 120))

; Disable menubar, scrollbar and toolbar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

; Bind C-return to jump to new line w/o breaking current line
(defun newline-without-breaking-line ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "<C-return>") 'newline-without-breaking-line)

; Move line up and down
(defun move-line-up()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(defun move-line-down()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(control up)] 'move-line-up)
(global-set-key [(control down)] 'move-line-down)

;---------------------------------------------------------------------------------------------------------------------;
; Enable autocomplete
(ac-config-default)

; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

; Helm
(require 'helm-config)

; Dash syntax highlighting
(eval-after-load 'dash '(dash-enable-font-lock))

; Async
(require 'dired-async)
(dired-async-mode 1)

; Popwin
(require 'popwin)
(popwin-mode 1)

; Company
(add-hook 'after-init-hook 'global-company-mode)

; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

; Smartparens
(require 'smartparens-config)

; Iedit
(require 'iedit)

;; Powerline
(require 'powerline)
(powerline-default-theme)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."

(unless package-archive-contents
  (package-refresh-contents))

  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'magit 'magit 'csharp-mode 'flycheck 'helm 'dash 'f 'use-package 'async 'popup 'popwin 'helm-core 'company 'yasnippet 'omnisharp 'projectile 'smartparens 'powerline 'indent-guide 'ample-theme)

;; activate installed packages
(package-initialize)

;---------------------------------------------------------------------------------------------------------------------;
; Font
(add-to-list 'default-frame-alist
  '(font . "InputMono-11"))

; Tab width
(setq-default tab-width 4)
(setq-default c-basic-offset 4
              c-indent-level 4
              c-brace-imaginary-offset 0
              c-brace-offset -4
              c-argdecl-indent 4
              c-label-offset -4
              c-continued-statement-offset 4)

; Disable startup messages
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(kill-buffer "*scratch*")

; Disables completions buffer
(add-hook 'minibuffer-exit-hook
	  '(lambda ()
	     (let ((buffer "*Completions*"))
	       (and (get-buffer buffer)
		        (kill-buffer buffer)))))

; Disable splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t
      initial-buffer-choice nil)

; Disable bell sounds
(setq visible-bell 1)

; Enable line numbering
(global-linum-mode t)

; Disable continued line symbol
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil right-curly-arrow))

; Disable backup
(setq make-backup-files nil)

; Set username and email ID
(setq user-full-name "Saurabh Sangpal"
      user-mail-address "saurabhsangpal@gmail.com")

; Rebinding moving between buffers to C-Right arrow and vice versa
(defun prev-window()
  (interactive)
  (other-window -1))
(global-set-key "\033[5D" 'prev-window)
(global-set-key "\033[5C" 'other-window)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit)))
 '(tab-stop-list (quote (4 8 12 16 20))))

; Theme
(load-theme 'ample t t)
(enable-theme 'ample)

; Set frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 125))

; Disable menubar, scrollbar and toolbar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Indent using spaces
(setq-default indent-tabs-mode t)

;; Disable case sensitive searching
(setq-default case-fold-search t)

;; Use S-left S-right S-up S-down to switch frames
(windmove-default-keybindings 'meta)

;; Bind format to C-M-F
(global-set-key (kbd "C-M-f") 'indent-region)

; Bind C-return to jump to new line w/o breaking current line
(defun newline-without-breaking-line ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "<C-return>") 'newline-without-breaking-line)

;; Move line up and down
(defun move-line-up()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(defun move-line-down()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(control up)] 'move-line-up)
(global-set-key [(control down)] 'move-line-down)

(fset 'n
      [end ?\; return])
(define-key global-map [f4] 'call-last-kbd-macro)

(defun semicolon-and-new-line()
  (interactive)
  (end-of-line)
  (insert ";")
  (newline-and-indent))
(global-set-key (kbd "C-;") 'semicolon-and-new-line)

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;---------------------------------------------------------------------------------------------------------------------;
;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Omnisharp
(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)


; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

; Helm
(require 'helm-config)
(global-set-key (kbd "<apps>") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(setq helm-M-x-fuzzy-match t)

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

; Company
(add-hook 'after-init-hook 'global-company-mode)

; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

; Smartparens
(use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'csharp-mode 'turn-on-smartparens-strict-mode)

; Indent guide
(require 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-delay 0.1)
(setq indent-guide-char "-")

;; Powerline
(require 'powerline)
(powerline-default-theme)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

