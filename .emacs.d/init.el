
;; https://blog.d46.us/advanced-emacs-startup/
;; Don't GC during startup
(setq gc-cons-threshold most-positive-fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")

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
    autothemer
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

(use-package autothemer
  :ensure t
  :config
  (load "${HOME}/.emacs.d/gruvbox-theme.el")
  (load-theme 'gruvbox t))


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

(defun wload (file)
  (or (load file t)
      (progn (warn "Failed to load file: %s" file)
             nil)))

(defvar acl2-skip-shell nil)
(setq acl2-skip-shell t)
(if (wload "${ACL2_ROOT}/books/emacs/emacs-acl2.el")
    (wload "${HOME}/.emacs.d/core/acl2-extensions.el"))
