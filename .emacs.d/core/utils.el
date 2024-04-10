(provide 'utils)
(require 'cl-lib)


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
  `(let ((ret-window (get-buffer-window (buffer-file-name (nth 1 (buffer-list)))))
         (save-window-ret (progn ,@body)))
     (select-window ret-window)
     save-window-ret))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))

(defun fresh-buffer-name-aux (base-fmt number suffix-fmt$ blacklist)
  (let ((name (format base-fmt (format suffix-fmt$ number))))
    (if (member name blacklist)
        (fresh-buffer-name-aux base-fmt (+ 1 number) suffix-fmt$ blacklist)
      name)))

;; base-fmt should have one `%s` for the potential suffix.
;; suffix-fmt (if provided) should have one `%d` (or other number formatter) for
;; the number.
(defun fresh-buffer-name (base-fmt &optional suffix-fmt number-init)
  (let ((blacklist (buffer-name-list))
        (base (format base-fmt "")))
    (if (member base blacklist)
        (fresh-buffer-name-aux base-fmt
                               (or number-init 0)
                               (or suffix-fmt "-%d")
                               blacklist)
      base)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Shell stuff

;; https://emacs.stackexchange.com/questions/28909/how-i-can-open-shell-in-current-buffer/28924#28924

(require 'shell)

(defun shell-get-buffer-create (&optional buffer)
  "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or (if that is nil) from `shell-file-name'.
If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
it is given as initial input (but this may be lost, due to a timing
error, if the shell discards input when it starts up).
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
         (prog1
             (read-buffer "Shell buffer: "
                          ;; If the current buffer is an inactive
                          ;; shell buffer, use it as the default.
                          (if (and (eq major-mode 'shell-mode)
                                   (null (get-buffer-process (current-buffer))))
                              (buffer-name)
                            (generate-new-buffer-name "*shell*")))
           (if (file-remote-p default-directory)
               ;; It must be possible to declare a local default-directory.
               ;; FIXME: This can't be right: it changes the default-directory
               ;; of the current-buffer rather than of the *shell* buffer.
               (setq default-directory
                     (expand-file-name
                      (read-directory-name
                       "Default directory: " default-directory default-directory
                       t nil))))))))
  (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                       (comint-check-proc (current-buffer)))
                   (get-buffer-create (or buffer "*shell*"))
                 ;; If the current buffer is a dead shell buffer, use it.
                 (current-buffer)))

  ;; On remote hosts, the local `shell-file-name' might be useless.
  (if (and (called-interactively-p 'any)
           (file-remote-p default-directory)
           (null explicit-shell-file-name)
           (null (getenv "ESHELL")))
      (with-current-buffer buffer
        (set (make-local-variable 'explicit-shell-file-name)
             (file-remote-p
              (expand-file-name
               (read-file-name
                "Remote shell path: " default-directory shell-file-name
                t shell-file-name))
              'localname))))

  ;; The buffer's window must be correctly set when we call comint (so
  ;; that comint sets the COLUMNS env var properly).
  (with-current-buffer buffer
    (unless (comint-check-proc buffer)
      (let* ((prog (or explicit-shell-file-name
                       (getenv "ESHELL") shell-file-name))
             (name (file-name-nondirectory prog))
             (startfile (concat "~/.emacs_" name))
             (xargs-name (intern-soft (concat "explicit-" name "-args"))))
        (unless (file-exists-p startfile)
          (setq startfile (concat user-emacs-directory "init_" name ".sh")))
        (apply 'make-comint-in-buffer "shell" buffer prog
               (if (file-exists-p startfile) startfile)
               (if (and xargs-name (boundp xargs-name))
                   (symbol-value xargs-name)
                 '("-i")))
        (shell-mode))))
  buffer)

(defun my-display-buffer (buffer alist direction &optional size pixelwise)
  "BUFFER:  The buffer that will be displayed.
ALIST:  See the doc-string of `display-buffer' for more information.
DIRECTION:  Must use one of these symbols:  'left 'right 'below 'above
SIZE:  See the doc-string for `split-window'.
PIXELWISE:  See the doc-string for `split-window'.
There are three possibilities:
-  (1) If a window on the frame already displays the target buffer,
then just reuse the same window.
-  (2) If there is already a window in the specified direction in relation
to the selected window, then display the target buffer in said window.
-  (3) If there is no window in the specified direction, then create one
in that direction and display the target buffer in said window."
  (let ((window
         (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction direction))
          (t
           (split-window (selected-window) size direction pixelwise)))))
    (window--display-buffer buffer window 'window alist)
    window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shell-cmd-get-buffer-create (&optional cmd buffer)
  (interactive)
  (let ((buf (shell-get-buffer-create buffer)))
    (save-excursion
     (switch-to-buffer buf)
     (goto-char (point-max))
     (when (stringp cmd)
       (progn
         (insert cmd)
         (evil-newline)))
     buf)))

(defun fresh-shell (&optional cmd base-fmt suffix-fmt number-init)
  (interactive)
  (select-window
   (get-buffer-window
    (shell-cmd-get-buffer-create
     cmd
     (fresh-buffer-name (or base-fmt "*shell%s*") suffix-fmt number-init)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar copyright-header-fmt-str
;;       ""

(defvar copyright-line-fmt-str "Copyright (C) %s %s")

(defvar copyright-alist
  '((ki . ("Kestrel Institute (https://www.kestrel.edu)"
           "grant@kestrel.edu"))
    (kt . ("Kestrel Technology LLC (http://kestreltechnology.com)"
           "grant@kestreltechnology.com"))
    (gj . ("Grant Jurgensen"
           "grant@jurgensen.dev"))))

;; TODO: add comments using comment-start and comment-end vars
;; TODO: add authorhip
;; TODO: add license option
(defun copyright-header-lines (&optional entity)
  (let* ((year (format-time-string "%G"))
         (entity (or entity 'ki))
         (entities (if (consp entity) entity (list entity)))
         (copyright-lines
          (mapcar
           (lambda (ent) (format copyright-line-fmt-str year
                                 (cadr (assoc ent copyright-alist))))
           entities))
         (authorship-line
          (format "Author: Grant Jurgensen (%s)"
                  (mapconcat
                   (lambda (ent) (caddr (assoc ent copyright-alist)))
                   entities ", "))))
    (append copyright-lines (list "" authorship-line))))

(defun unlines-comment (lines)
  (mapconcat
   (lambda (line)
     (mapconcat
      'identity
      (cl-remove-if 'string-empty-p (list comment-start line comment-end))
      " "))
   lines "\n"))

(defun copyright-header-str (&optional entity)
  (unlines-comment (copyright-header-lines entity)))

(defun copyright-header (&optional entity)
  (interactive)
  (insert (copyright-header-str entity)))
