
;; https://blog.d46.us/advanced-emacs-startup/
;; Don't GC during startup
(setq gc-cons-threshold most-positive-fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MELPA

;; Add MELPA package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install packages

(defconst *pkgs*
  '(evil
    evil-collection
    use-package
    proof-general
    ))

;; Update with M-x list-packages U x
(defun install-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (pkg-name *pkgs*)
    (package-install pkg-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure packages

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'shell-mode evil-default-state)
  (define-key evil-motion-state-map " " 'evil-window-map)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package tramp
  :config
  ;; (setq tramp-default-method "sshx")
  ;; (setenv "SHELL" "/bin/sh")
  )

(use-package proof-general)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global configuration


(setq-default display-line-numbers-current-absolute nil)
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
;; Alternative if don't want this enabled globally
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; disable tool and menu bars
(tool-bar-mode -1) ;; Don't bother for tui?
(menu-bar-mode -1)

(scroll-bar-mode -1) ;; Don't bother for tui?

(setq inhibit-startup-message t)
(setq visible-bell t)
(global-font-lock-mode 1)

;; Mode line config
(column-number-mode)
(setq-default mode-line-end-spaces nil)

(setq-default whitespace-style '(face tab-mark tabs))
(setq-default custom-tab-width 4)
(setq-default indent-tabs-mode nil)
(global-whitespace-mode)

;; I couldn't figure out how else to set this on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (set-display-table-slot (or buffer-display-table standard-display-table) 'vertical-border ?│)))

;; Report startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.4f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Custom GC threshold
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 10000000))) ;; 1MB (default is 80KB)


(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(read-abbrev-file)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silent)

(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq vc-follow-symlinks t)

(defvar acl2-skip-shell nil)
(setq acl2-skip-shell t)
(load "${ACL2_ROOT}/books/emacs/emacs-acl2.el")
(load "${HOME}/.emacs.d/core/acl2-extensions.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto-generated

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(proof-general evil-collection use-package evil cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-variable-tag ((t (:foreground "#83A598" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#D3869B"))))
 '(font-lock-comment-face ((t (:foreground "#928874" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#83A598"))))
 '(font-lock-function-name-face ((t (:foreground "#8EC07C"))))
 '(font-lock-keyword-face ((t (:foreground "#FB4934"))))
 '(font-lock-string-face ((t (:foreground "#B8BB26"))))
 '(font-lock-variable-name-face ((t (:foreground "#FABD2F"))))
 '(line-number ((t (:inherit nil :foreground "#928874"))))
 '(line-number-current-line ((t (:inherit line-number))))
 '(minibuffer-prompt ((t (:foreground "#B8BB26"))))
 '(mode-line ((t (:background "#504945" :foreground "#EBDBB2" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:background "#3c3836" :foreground "#A89984" :box (:line-width -1 :style released-button)))))
 '(proof-locked-face ((t (:extend t :background "color-237"))))
 '(vertical-border ((t (:foreground "#928874"))))
 '(whitespace-indentation ((t (:inherit whitespace-space))))
 '(whitespace-newline ((t (:foreground "#928874" :weight normal))))
 '(whitespace-space ((t (:foreground "#928874"))))
 '(whitespace-tab ((t (:foreground "#928874"))))
 '(window-divider ((t (:foreground "#928874")))))
