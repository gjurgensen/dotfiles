(provide 'utils)

(defun toggle-prev-buffer ()
  "Switch to the previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; https://www.emacswiki.org/emacs/CommentingCode
;; (defun uncomment-region (beg end)
;;   "Like `comment-region' invoked with a C-u prefix arg."
;;   (interactive)
;;   (comment-region beg end -1))

;; newline that works in normal mode, visual mode, etc.
(fset 'evil-newline
      (kmacro-lambda-form [?\\ ?\C-m] 0 "%d"))

(defun wload (file)
  "Load file FILE or print a warning."
  (or (load file t)
      (progn (warn "Failed to load file: %s" file)
             nil)))

(defun eload (file)
  "Load file FILE or throw an error."
  (or (load file t)
      (progn (error "Failed to load file: %s" file)
             nil)))

(defmacro save-window (&rest body)
  "Save current window; execute BODY; restore window.

Similar to `save-excursion'."
  `(let ((ret-window (get-buffer-window (buffer-file-name (nth 1 (buffer-list))))))
     ,@body
     (select-window ret-window)))

;; https://emacs.stackexchange.com/questions/17306/upcase-whole-buffer-but-ignore-quoted-strings
(defun downcase-region-smart (beg end)
  "Downcase region, except for strings and similar case-sensitive subregions."
  (interactive "r")
  (let* ((old-syntax-table (syntax-table))
         (new-syntax-table (make-syntax-table old-syntax-table)))
    (modify-syntax-entry ?\" "\"   " new-syntax-table)
    (with-syntax-table new-syntax-table
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (let ((beg (point)))
            (parse-partial-sexp (point) end nil nil nil 'syntax-table)
            (downcase-region beg (point))
            (parse-partial-sexp (point) end nil nil nil 'syntax-table)))))))

(defun upcase-region-smart (beg end)
  "Upcase region, except for strings and similar case-sensitive subregions."
  (interactive "r")
  (let* ((old-syntax-table (syntax-table))
         (new-syntax-table (make-syntax-table old-syntax-table)))
    (modify-syntax-entry ?\" "\"   " new-syntax-table)
    (with-syntax-table new-syntax-table
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (let ((beg (point)))
            (parse-partial-sexp (point) end nil nil nil 'syntax-table)
            (upcase-region beg (point))
            (parse-partial-sexp (point) end nil nil nil 'syntax-table)))))))

(defun einit ()
  "Change directory to `~/.emacs.d' and open `init.el'"
  (interactive)
  (cd "~/.emacs.d")
  (find-file "init.el"))
