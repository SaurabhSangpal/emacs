(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;Font
(add-to-list 'default-frame-alist
	     '(font . "IBM Plex Mono-11"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes
   (quote
    ("59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "617341f1be9e584692e4f01821716a0b6326baaec1749e15d88f6cc11c288ec6" default)))
 '(package-selected-packages
   (quote
    (d-mode auto-complete-clang-async auto-complete-clang ac-racer company flycheck-rust yasnippet cargo cmake-ide cmake-mode racer flycheck-irony flycheck rust-playground atom-one-dark-theme rust-mode dracula-theme helm-make auto-complete magit dash helm))))

;; Add Rust Support
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq rust-format-on-save t)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(setq tab-width 4)
;; Setting indent
(setq-default c-basic-offset 4
	      c-indent-level 4
	      c-brace-imaginary-offset 0
	      c-brace-offset -4
	      c-argdecl-indent 4
	      c-label-offset -4
	      c-continued-statement-offset 4)
;; Disables the messages buffer on startup
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(kill-buffer "*scratch*")
;; Disables the completions buffer
(add-hook 'minibuffer-exit-hook
	  '(lambda ()
	     (let ((buffer "*Completions*"))
	       (and (get-buffer buffer)
		    (kill-buffer buffer)))))
;; Disable splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t
      initial-buffer-choice nil)
;; Disable bell sounds
(setq visible-bell 1)
;; Enable line numbering
(global-linum-mode t)
;; Disable continued line symbol
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil right-curly-arrow))
;; Disable backup
(setq make-backup-files nil)
;; Set username and email ID
(setq user-full-name "Saurabh Sangpal"
      user-mail-address "saurabhsangpal@gmail.com")
;; Change evil cursor
;;(setq evil-insert-state-cursor '(box "blue")
;;      evil-normal-state-cursor '(box "white"))
;; Remove Scroll bars (very ugly)
(scroll-bar-mode -1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Adding clang format support
(load "C:/Program Files/LLVM/share/clang/clang-format.el")
(global-set-key [C-tab] 'clang-format-buffer)
;; Rebinding moving between buffers to C-Right arrow and vice versa
(defun prev-window()
  (interactive)
  (other-window -1))
(global-set-key "\033[5D" 'prev-window)
(global-set-key "\033[5C" 'other-window)

;; Set Autocomplete
(ac-config-default)
