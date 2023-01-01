;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GC

;; https://blog.d46.us/advanced-emacs-startup/
;; Don't GC during startup
(setq gc-cons-threshold most-positive-fixnum)

(defun emacs-startup-gc-and-report ()
  (setq gc-cons-threshold 10000000) ;; 1MB (default is 80KB)
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.4f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook 'emacs-startup-gc-and-report)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Custom file

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Load utilities

(load "~/.emacs.d/core/utils.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MELPA

;; Add MELPA package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Manually update packages with M-x list-packages U x (should be done
;; automatically by auto-package-update).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Packages

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'shell-mode evil-default-state)
  (define-key evil-motion-state-map " " 'evil-window-map)
  (define-key evil-visual-state-map " " 'evil-window-map)
  (define-key evil-window-map (kbd "TAB") 'toggle-prev-buffer))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :init
  (defalias 'evil-org-link-open
    (read-kbd-macro "\\ C-c C-o"))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (define-key evil-normal-state-map (kbd "RET") 'evil-org-link-open))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)


(use-package tramp
  :config
  (setenv "SHELL" "/bin/bash"))


(use-package org
  :ensure t
  :init
  (setq org-image-actual-width nil))


(use-package abbrev-mode
  :init
  (setq-default abbrev-mode t)
  (add-hook 'coq-mode-hook (lambda () (setq abbrev-mode nil)))
  :config
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (read-abbrev-file "~/.emacs.d/abbrev_defs")
  (setq save-abbrevs 'silently))


(use-package autothemer
  :ensure t
  :config
  (load "${HOME}/.emacs.d/gruvbox-theme.el")
  (load-theme 'gruvbox t))


(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))


(use-package smartparens
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))


(use-package xclip
  :ensure t
  :config
  ;; https://stackoverflow.com/questions/37214940/require-package-only-if-available
  (when (executable-find "xclip")
    (xclip-mode 1)))


(use-package proof-general
  :ensure t
  :config
  (setq proof-splash-enable nil))

(use-package company-coq
  :ensure t
  :config
  (add-hook 'coq-mode-hook #'company-coq-mode)
  (add-hook 'coq-mode-hook (lambda () (prettify-symbols-mode))))


(use-package doom-modeline
  :ensure t
  :after all-the-icons
  :init
  (doom-modeline-mode 1)
  ;; Should have no effect on TUI
  (setq doom-modeline-icon t)
  (setq doom-modeline-modal-icon t))

;; May need to run `(all-the-icons-install-fonts)` on install?
(use-package all-the-icons
  :ensure t)


(use-package treemacs
  :ensure t
  :defer t)


(use-package flycheck
  :ensure t
  :after exec-path-from-shell
  :config
  ; (global-flycheck-mode)
  )


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gm-mode)
  :init (setq markdown-command "multimarkdown"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Global configuration

(defun set-tui-vertical-border ()
  ;; Thicker version:
  ;; (set-display-table-slot (or buffer-display-table standard-display-table) 'vertical-border ?┃)
  (set-display-table-slot buffer-display-table 'vertical-border ?│)
  (set-display-table-slot standard-display-table 'vertical-border ?│)
  )

(defun set-tui-style ()
  (set-tui-vertical-border)
  (set-face-background 'default "unspecified-bg" (selected-frame)))

(defun set-gui-style ()
  (set-frame-parameter (selected-frame) 'alpha '(97 . 95)))

(defun set-style ()
  (if (display-graphic-p (selected-frame))
      (set-gui-style)
    (set-tui-style)))

(if (display-graphic-p (selected-frame))
    (add-hook 'emacs-startup-hook 'set-gui-style)
  (add-hook 'emacs-startup-hook 'set-tui-style))

;; This is probably overkill, but harmless. Solves an issue where vertical bar
;; character was being unpredictably reset.
(add-hook 'server-switch-hook 'set-style)
(add-hook 'window-setup-hook  'set-style)


;; TODO: restrict to text modes?
(global-visual-line-mode t)

(add-to-list 'default-frame-alist '(font . "Fira Code 12"))

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

(setq initial-scratch-message nil)

(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq vc-follow-symlinks t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Loads

(load "~/.emacs.d/core/acl2.el")
