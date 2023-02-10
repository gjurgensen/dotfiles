;;; ACL2-related config, called by `acl2.el' after loading `emacs-acl2.el'.

(require 'smartparens)
(require 'utils "~/.emacs.d/core/utils.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Extensions and modifications to the standard Emacs file

(define-key ctl-t-keymap "c" 'set-acl2-shell-buffer)
(defun set-acl2-shell-buffer ()
  (interactive)
  ;; The original name is hard for me to remember since it doesn't mention acl2
  (set-shell-buffer))

(defun ashell ()
  (interactive)
  (save-window
   (shell (fresh-buffer-name "acl2%s"))
   (set-acl2-shell-buffer)
   (insert "$ACL2")
   (evil-newline)))

(defun acl2 ()
  (interactive)
  (ashell))

;; calls `ashell` if there is no acl2-shell
(defun ashell-if-none ()
  (interactive)
  (unless (get-buffer *acl2-shell*)
    (ashell)))

(defun tramp-filename-to-local (filename)
  (car (last (split-string filename ":"))))

(defun acl2-load-all-elsewhere ()
  (interactive)
  (save-window
   (ashell-if-none)
   (select-window (get-buffer-window (get-buffer *acl2-shell*)))
   (goto-char (point-max))
   (insert
    (format "(acl2::ld \"%s\")"
            ;; https://stackoverflow.com/a/455500/11126632
            (tramp-filename-to-local (buffer-file-name (nth 1 (buffer-list))))))
   (evil-newline)))


(defun submit-theorem-elsewhere ()
  (interactive)
  (save-window
   (enter-theorem-fn t)
   (evil-newline)))

(defun acl2-submit-undo-elsewhere ()
  (interactive)
  (save-window
   (select-window (get-buffer-window (get-buffer *acl2-shell*)))
   (goto-char (point-max))
   (insert
    (format ":ubt! %s"
            (acl2-event-name
             (car (read-from-string (acl2-current-form-string t)))
             t))
   (evil-newline))))

(defun acl2-submit-pe ()
  (interactive)
  (save-window
   (sp-copy-sexp)
   (select-window (get-buffer-window (get-buffer *acl2-shell*)))
   (goto-char (point-max))
   (insert (format ":pe %s" (car kill-ring-yank-pointer)))
   (evil-newline)))

(defun pe (str)
  (interactive "sEvent name: ")
  (save-window
   (ashell-if-none)
   (select-window (get-buffer-window (get-buffer *acl2-shell*)))
   (goto-char (point-max))
   (insert
    (format ":pe %s" str))
   (evil-newline)))

(evil-define-command evil-pe (str)
  (interactive "<a>")
  (pe (or str (read-string "Event name: "))))

(evil-ex-define-cmd "pe" 'evil-pe)

(defun acl2-submit-pr ()
  (interactive)
  (save-window
   (sp-copy-sexp)
   (select-window (get-buffer-window (get-buffer *acl2-shell*)))
   (goto-char (point-max))
   (insert (format ":pr %s" (car kill-ring-yank-pointer)))
   (evil-newline)))

(defun pr (str)
  (interactive "sRune: ")
  (save-window
   (ashell-if-none)
   (select-window (get-buffer-window (get-buffer *acl2-shell*)))
   (goto-char (point-max))
   (insert
    (format ":pr %s" str))
   (evil-newline)))

(evil-define-command evil-pr (str)
  (interactive "<a>")
  (pr (or str (read-string "Rune: "))))

(evil-ex-define-cmd "pr" 'evil-pr)

(defun acl2-submit-doc ()
  (interactive)
  (save-window
   (sp-copy-sexp)
   (select-window (get-buffer-window (get-buffer *acl2-shell*)))
   (goto-char (point-max))
   (insert (format ":doc %s" (car kill-ring-yank-pointer)))
   (evil-newline)))

(define-key ctl-t-keymap "\C-r" 'acl2-load-all-elsewhere)
(define-key ctl-t-keymap "\C-u" 'acl2-submit-undo-elsewhere)
(define-key ctl-t-keymap "\C-p" 'acl2-submit-pe)
(define-key ctl-t-keymap "\C-o" 'acl2-submit-pr)
(define-key ctl-t-keymap "\C-d" 'acl2-submit-doc)
;; Overwrites enter-theorem-elsewhere
(define-key ctl-t-keymap "\C-e" 'submit-theorem-elsewhere)
