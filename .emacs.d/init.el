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
;; (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
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
  (define-key evil-window-map (kbd "TAB") 'toggle-prev-buffer)
  (define-key evil-visual-state-map "u" 'downcase-region-smart)
  (define-key evil-visual-state-map "U" 'upcase-region-smart)
  (evil-set-undo-system 'undo-redo)

  ;; "ns" = "named-shell"
  (evil-define-command evil-ns (name)
    (interactive "<a>")
    (fresh-shell nil name))

  (evil-ex-define-cmd "ns" 'evil-ns)
  )

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
  :config
  (define-skeleton org-header "" nil
    "#+title: " _ "\n#+author: Grant Jurgensen")
  :init
  (setq org-image-actual-width nil)
  (setq org-startup-folded t))


;; (use-package abbrev-mode
;;   :ensure nil
;;   :init
;;   (setq-default abbrev-mode t)
;;   (add-hook 'coq-mode-hook (lambda () (setq abbrev-mode nil)))
;;   :config
;;   (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
;;   (read-abbrev-file "~/.emacs.d/abbrev_defs")
;;   (setq save-abbrevs 'silently))


(use-package autothemer
  :ensure t
  :config
  (load "~/.emacs.d/gruvbox-theme.el")
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
  (if (executable-find "xclip")
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


(use-package haskell-mode
  :ensure t)


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


(use-package treemacs
  :ensure t
  :defer t)


(use-package flycheck
  :ensure t
  :after exec-path-from-shell
  ; :config (global-flycheck-mode)
  )


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package eyebrowse
  :ensure t
  :after evil
  :init
  (define-key evil-window-map (kbd "0") 'eyebrowse-switch-to-window-config-0)
  (define-key evil-window-map (kbd "1") 'eyebrowse-switch-to-window-config-1)
  (define-key evil-window-map (kbd "2") 'eyebrowse-switch-to-window-config-2)
  (define-key evil-window-map (kbd "3") 'eyebrowse-switch-to-window-config-3)
  (define-key evil-window-map (kbd "4") 'eyebrowse-switch-to-window-config-4)
  (define-key evil-window-map (kbd "5") 'eyebrowse-switch-to-window-config-5)
  (define-key evil-window-map (kbd "6") 'eyebrowse-switch-to-window-config-6)
  (define-key evil-window-map (kbd "7") 'eyebrowse-switch-to-window-config-7)
  (define-key evil-window-map (kbd "8") 'eyebrowse-switch-to-window-config-8)
  (define-key evil-window-map (kbd "9") 'eyebrowse-switch-to-window-config-9)
  :config (eyebrowse-mode t))


;; For copilot

;; (use-package editorconfig
;;   :ensure t)

;; (use-package jsonrpc
;;   :ensure t)

;; (use-package copilot
;;   :ensure t
;;   ;; :quelpa (copilot :fetcher github
;;   ;;                  :repo "copilot-emacs/copilot.el"
;;   ;;                  :branch "main"
;;   ;;                  :files ("*.el"))
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ;; ("<tab>" . 'copilot-accept-completion)
;;               ;; ("TAB" . 'copilot-accept-completion)
;;               ;; ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ;; ("C-<tab>" . 'copilot-accept-completion-by-word)
;;               ("<tab>" . 'copilot-accept-completion-by-word)
;;               ("TAB" . 'copilot-accept-completion-by-word)
;;               ("C-TAB" . 'copilot-accept-completion)
;;               ("C-<tab>" . 'copilot-accept-completion)
;;               )
;;   :init (add-to-list 'warning-suppress-types '(copilot)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Global configuration

;(set-display-table-slot buffer-display-table 'vertical-border ?│)
;(set-display-table-slot standard-display-table 'vertical-border ?│)

;; (defun set-tui-vertical-border ()
;;   ;; Thicker version:
;;   ;; (set-display-table-slot (or buffer-display-table standard-display-table) 'vertical-border ?┃)
;;   (set-display-table-slot buffer-display-table 'vertical-border ?│)
;;   ;(set-display-table-slot window-display-table 'vertical-border ?│)
;;   (set-display-table-slot standard-display-table 'vertical-border ?│)
;;   )

(defun set-tui-vertical-border ()
  (if (not (active-minibuffer-window))
      (progn
        (set-display-table-slot buffer-display-table 'vertical-border ?│)
        (set-display-table-slot standard-display-table 'vertical-border ?│))))

(defun set-tui-style ()
  (set-tui-vertical-border)
  (set-face-background 'default "unspecified-bg" (selected-frame)))

(defun set-gui-style ()
  (set-frame-parameter (selected-frame) 'alpha '(97 . 95)))

(defun set-style ()
  (interactive)
  (if (display-graphic-p (selected-frame))
      (set-gui-style)
    (set-tui-style)))

(defun set-style-foo ()
  (if (display-graphic-p (selected-frame))
      (set-gui-style)
    (set-tui-style)))

(if (display-graphic-p (selected-frame))
    (add-hook 'emacs-startup-hook 'set-gui-style)
  (add-hook 'emacs-startup-hook 'set-tui-style))

(add-hook 'server-switch-hook 'set-style)
;(add-hook 'window-setup-hook  'set-style)
;(add-hook 'window-configuration-change-hook 'set-tui-vertical-border)


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
(setq-default custom-tab-width 2)
(setq-default indent-tabs-mode nil)
(global-whitespace-mode)

(setq c-basic-offset 2)

(setq initial-scratch-message nil)

(setq-default show-trailing-whitespace t)
(add-hook 'shell-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq vc-follow-symlinks t)

(setq auto-mode-alist
      (append '(("Buildfile" . lisp-mode)
                (".*\\.camkes\\'" . c-mode)
                (".*\\.idl4\\'" . c-mode))
              auto-mode-alist))

;; term stuff
;; server-name doesn't seem to be set until startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (daemonp)
              (setenv "EMACS_SERVER" server-name))))


;; abbrev-mode config/hooks
(setq-default abbrev-mode t)
(add-hook 'coq-mode-hook (lambda () (setq abbrev-mode nil)))
(add-hook 'abbrev-mode-hook
          (lambda ()
            (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
            (read-abbrev-file "~/.emacs.d/abbrev_defs")
            (setq save-abbrevs 'silently)))

;; (custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; https://www.emacswiki.org/emacs/FullScreen#h5o-21
(defun fullscreen ()
  "Enable full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun disable-fullscreen ()
  "Disable full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter nil 'fullscreen nil)))

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (if (not (frame-parameter nil 'fullscreen))
         'fullboth
       nil))))

;; TODO: add to gui setup?
(add-hook 'emacs-startup-hook 'fullscreen)
(add-hook 'server-switch-hook 'fullscreen)
(add-hook 'window-setup-hook  'fullscreen)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Loads

(load "~/.emacs.d/core/acl2.el")
