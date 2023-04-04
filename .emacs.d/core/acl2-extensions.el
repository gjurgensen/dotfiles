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

(defun new-acl2-shell (name)
  (save-window
   (shell (fresh-buffer-name (concat shell-buf-name "%s")))
   (insert "$ACL2")
   (evil-newline)
   (buffer-name (current-buffer))))

(defun new-local-acl2-shell (shell-buf-name)
  (interactive "B")
  (set (make-local-variable 'local-acl2-shell)
       (new-acl2-shell (concat shell-buf-name "%s")))
  (message "Setting the local ACL2 shell to buffer %s" local-acl2-shell))

(defun local-acl2-shell (shell-buf-name)
  (interactive "B")
  (set (make-local-variable 'local-acl2-shell)
       (or (and (get-buffer shell-buf-name) shell-buf-name)
           (new-acl2-shell (concat shell-buf-name "%s"))))
  (message "Setting the local ACL2 shell to buffer %s" local-acl2-shell))

(defun ashell (&optional shell-buf-name)
  (interactive)
  (new-local-acl2-shell (or shell-buf-name "acl2")))

(defun acl2 ()
  (interactive)
  (ashell))

;; calls `ashell` if there is no acl2-shell
;; TODO: also check if there is a global shell?
(defun ashell-if-none ()
  (interactive)
  (unless (get-buffer local-acl2-shell)
    (ashell)))

(defun tramp-filename-to-local (filename)
  (car (last (split-string filename ":"))))

(defun acl2-load-all-elsewhere ()
  (interactive)
  (save-window
   (ashell-if-none)
   ;; Todo assign buffer to window if not in one
   (select-window (get-buffer-window (get-buffer local-acl2-shell)))
   (goto-char (point-max))
   (insert
    (format "(acl2::ld \"%s\")"
            ;; https://stackoverflow.com/a/455500/11126632
            (tramp-filename-to-local (buffer-file-name (nth 1 (buffer-list))))))
   (evil-newline)))


;; Modified from enter-theorem-fn in emacs-acl2.el
(defun enter-theorem-fn-local (elsewhere)
  (let* ((str (acl2-current-form-string))
         (buf (get-buffer local-acl2-shell))
         (win (if elsewhere
                  (get-buffer-window buf)
                (selected-window)))
         (patterns *acl2-insert-pats*))
    (unless buf
      (error "Nonexistent *acl2-shell* buffer: %s" *acl2-shell*))
    ;; Go to the *acl2-shell* buffer
    (push-mark)
    (if win
        (select-window win)
      (other-window 1))
    (switch-to-buffer buf)
    (goto-char (point-max))
    ;; Check that there is a process in the buffer
    (unless (get-buffer-process buf)
      (error "Error: This buffer has no process!"))
    ;; Check that we have a valid prompt at which to place the form.
    (save-excursion
      (forward-line 0)
      (cond
       ((null patterns))         ; nothing to check
       ((eq (car patterns) :not) ; prompt must not match any of the regexps
        (while (setq patterns (cdr patterns))
          (when (looking-at (car patterns))
            (error "Error: Detected non-ACL2 prompt, matching \"%s\"; see *acl2-insert-pats*"
                   (car patterns)))))
       (t                      ; prompt must match one of the regexeps
        (let ((flg nil))
          (while patterns
            (cond ((looking-at (car patterns))
                   (setq flg t)
                   (setq patterns nil))
                  (t (setq patterns (cdr patterns)))))
          (or flg
              (error "Error: Couldn't detect ACL2 prompt; see *acl2-insert-pats*"))))))
    ;; Insert the form
    (insert str)))

(defun submit-theorem-elsewhere ()
  (interactive)
  (save-window
   (enter-theorem-fn-local t)
   (evil-newline)))

(defun acl2-submit-undo-elsewhere ()
  (interactive)
  (save-window
   (select-window (get-buffer-window (get-buffer local-acl2-shell)))
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
   (select-window (get-buffer-window (get-buffer local-acl2-shell)))
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
   (select-window (get-buffer-window (get-buffer local-acl2-shell)))
   (goto-char (point-max))
   (insert (format ":pr %s" (car kill-ring-yank-pointer)))
   (evil-newline)))

(defun pr (str)
  (interactive "sRune: ")
  (save-window
   (ashell-if-none)
   (select-window (get-buffer-window (get-buffer local-acl2-shell)))
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
   (select-window (get-buffer-window (get-buffer local-acl2-shell)))
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
