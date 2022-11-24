
;; https://blog.d46.us/advanced-emacs-startup/
;; Don't GC during startup
(setq gc-cons-threshold most-positive-fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
    evil-org
    use-package
    exec-path-from-shell
    autothemer
    smartparens
    rainbow-delimiters
    xclip
    proof-general
    company-coq
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
  ;; (evil-set-initial-state 'man-mode evil-default-state)
  (define-key evil-motion-state-map " " 'evil-window-map)
  )

;; (use-package man
;;   :config
;;   ;; (define-key man-mode-map " " nil)
;;   ;; (setq man-notify-method 'pushy)
;;   )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "ACL2_ROOT")
  (exec-path-from-shell-initialize))

(use-package tramp
  :config
  ;; (setq tramp-default-method "sshx")
  ;; (setenv "SHELL" "/bin/sh")
  )

(use-package smartparens)

(use-package autothemer
  :ensure t
  :config
  (load "${HOME}/.emacs.d/gruvbox-theme.el")
  (load-theme 'gruvbox t))

(use-package rainbow-delimiters
  :config
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package proof-general
  :config
  (setq proof-splash-enable nil))

(use-package company-coq
  :config
  (add-hook 'coq-mode-hook #'company-coq-mode))

(use-package org)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global configuration

(defun set-tui-vertical-border()
  ;; Thicker version:
  ;; (set-display-table-slot (or buffer-display-table standard-display-table) 'vertical-border ?┃)
  (set-display-table-slot buffer-display-table 'vertical-border ?│)
  (set-display-table-slot standard-display-table 'vertical-border ?│)
  )

(defun set-tui-style()
  (set-tui-vertical-border)
  (set-face-background 'default "unspecified-bg" (selected-frame)))

(defun set-gui-style()
  (set-frame-font "Fira Code 12" nil t))

(if (display-graphic-p (selected-frame))
    ;; Not quite working, still messes up sometimes
    (progn
      (add-hook 'emacs-startup-hook 'set-tui-style)
      (add-hook 'server-switch-hook 'set-tui-style)
      (add-hook 'window-setup-hook  'set-tui-style))
  (progn
    (add-hook 'emacs-startup-hook 'set-gui-style)
    (add-hook 'server-switch-hook 'set-gui-style)
    (add-hook 'window-setup-hook  'set-gui-style)))


(setq-default display-line-numbers-current-absolute nil)
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Disable tool and menu bars
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
(add-hook 'coq-mode-hook (lambda () (setq abbrev-mode nil)))
(setq save-abbrevs 'silent)

(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq vc-follow-symlinks t)

(defun wload (file)
  (or (load file t)
      (progn (warn "Failed to load file: %s" file)
             nil)))

;; TODO, make sure exec-path-from-shell is already loaded
(defvar acl2-skip-shell nil)
(setq acl2-skip-shell t)
(if (wload "${ACL2_ROOT}/books/emacs/emacs-acl2.el")
    (wload "${HOME}/.emacs.d/core/acl2-extensions.el"))

;; (set-display-table-slot (or buffer-display-table standard-display-table) 'vertical-border ?│)
;; (set-display-table-slot buffer-display-table 'vertical-border ?│)
;; (set-face-background 'default "unspecified-bg" (selected-frame))
