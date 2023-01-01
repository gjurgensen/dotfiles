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
