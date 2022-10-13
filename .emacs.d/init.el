
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
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


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

;; Report startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.4f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; ;; Restore default GC threshold after startup
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold 800000))) ;; 80KB

;; Custom GC threshold
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 10000000))) ;; 1MB


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
 '(package-selected-packages '(evil-collection use-package evil cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-variable-tag ((t (:foreground "brightblue" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "brightmagenta"))))
 '(font-lock-comment-face ((t (:foreground "brightblack" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "brightblue"))))
 '(font-lock-function-name-face ((t (:foreground "brightcyan"))))
 '(font-lock-keyword-face ((t (:foreground "brightred"))))
 '(font-lock-string-face ((t (:foreground "brightgreen"))))
 '(font-lock-variable-name-face ((t (:foreground "brightyellow"))))
 '(line-number ((t (:inherit shadow))))
 '(line-number-current-line ((t (:inherit line-number))))
 '(minibuffer-prompt ((t (:foreground "brightgreen"))))
 '(mode-line ((t (:background "brightblack" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :weight light))))
 '(window-divider ((t (:foreground "brightblack")))))
