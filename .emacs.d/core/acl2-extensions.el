;; Dependencies:
;; 1. smartparens (sp-copy-sexp)

;; courtesy of Eric Smith, modified for evil-mode compatibility
(defalias 'strip-encloser
  (read-kbd-macro
   "\\ C-M-k \\ C-M-u \\ C-y \\ C-M-k \\ C-M-b \\ C-M-q"))

(fset 'localize
      (kmacro-lambda-form [?% ?a ?\) escape ?h ?% ?i ?\( ?l ?o ?c ?a ?l ?  escape ?l ?\\] 0 "%d"))

(fset 'evil-newline
      (kmacro-lambda-form [?\\ ?\C-m] 0 "%d"))

;; Similar, perhaps, to `save-excursion`
(defmacro save-window (&rest body)
  `(let ((ret-window (get-buffer-window (buffer-file-name (nth 1 (buffer-list))))))
     ,@body
     (select-window ret-window)))

(defun ashell ()
  (interactive)
  (save-window
   (shell)
   (insert "$ACL2")
   (evil-newline)))

(defun new-ashell ()
  (interactive)
  (save-window
   (new-shell)
   (insert "$ACL2")
   (evil-newline)))

(defun tramp-filename-to-local (filename)
  (car (last (split-string filename ":"))))

(defun acl2-load-all-elsewhere ()
  (interactive)
  (save-window
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

(defun acl2-submit-pr ()
  (interactive)
  (save-window
   (sp-copy-sexp)
   (select-window (get-buffer-window (get-buffer *acl2-shell*)))
   (goto-char (point-max))
   (insert (format ":pr %s" (car kill-ring-yank-pointer)))
   (evil-newline)))

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
