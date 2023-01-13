;;; ACL2-related config.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Load emacs-acl2.el and acl2-extensions.el

(use-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "ACL2_ROOT")
  (exec-path-from-shell-initialize))

(defvar acl2-skip-shell nil)
(setq acl2-skip-shell t)
(and (wload "${ACL2_ROOT}/books/emacs/emacs-acl2.el")
     (wload "~/.emacs.d/core/acl2-extensions.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Abbreviations

(define-skeleton skel-dx "" nil
  "(declare (xargs " _ "))")

(define-skeleton skel-dxg "" nil
  "(declare (xargs :guard " _ "))")

(define-skeleton skel-dxm "" nil
  "(declare (xargs :measure " _ "))")

(define-skeleton skel-hg "" nil
  ":hints ((\"Goal\" " _ "))")

(define-skeleton skel-hid "" nil
  ":hints ((\"Goal\" :in-theory (disable " _ ")))")

(define-skeleton skel-hie "" nil
  ":hints ((\"Goal\" :in-theory (enable " _ ")))")

(define-skeleton skel-hied "" nil
  ":hints ((\"Goal\" :in-theory (e/d " _ ")))")

(define-skeleton skel-hu "" nil
  ":hints ((\"Goal\" :use ((" _ "))))")

(define-skeleton skel-hui "" nil
  ":hints ((\"Goal\" :use ((:instance " _ "))))")

(define-skeleton skel-ghg "" nil
  ":guard-hints ((\"Goal\" " _ "))")

(define-skeleton skel-ghid "" nil
  ":guard-hints ((\"Goal\" :in-theory (disable " _ ")))")

(define-skeleton skel-ghie "" nil
  ":guard-hints ((\"Goal\" :in-theory (enable " _ ")))")

(define-skeleton skel-ghied "" nil
  ":guard-hints ((\"Goal\" :in-theory (e/d " _ ")))")

(define-skeleton skel-ghu "" nil
  ":guard-hints ((\"Goal\" :use ((" _ "))))")

(define-skeleton skel-ghui "" nil
  ":guard-hints ((\"Goal\" :use ((:instance " _ "))))")

(define-abbrev-table 'acl2-abbrev-table
  '(("dx"    "" skel-dx 0)
    ("dxg"   "" skel-dxg 0)
    ("dxm"   "" skel-dxm 0)
    ("hg"    "" skel-hg 0)
    ("hid"   "" skel-hdi 0)
    ("hie"   "" skel-hie 0)
    ("hied"  "" skel-hied 0)
    ("hu"    "" skel-hu 0)
    ("hui"   "" skel-hui 0)
    ("ghg"   "" skel-ghg 0)
    ("ghid"  "" skel-ghdi 0)
    ("ghie"  "" skel-ghie 0)
    ("ghied" "" skel-ghied 0)
    ("ghu"   "" skel-ghu 0)
    ("ghui"  "" skel-ghui 0)
    ("agj"   ";;; Author: Grant Jurgensen (grant@kestrel.edu)" nil 0)
    ("dxgt"  "(declare (xargs :guard t))" nil 0)
    ))

(add-hook 'lisp-mode-hook
            (lambda () (setq local-abbrev-table acl2-abbrev-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Misc

;; Courtesy of Eric Smith, modified for evil-mode compatibility
(defalias 'strip-encloser
  (read-kbd-macro
   "\\ C-M-k \\ C-M-u \\ C-y \\ C-M-k \\ C-M-b \\ C-M-q"))

(fset 'localize
      (kmacro-lambda-form [?% ?a ?\) escape ?h ?% ?i ?\( ?l ?o ?c ?a ?l ?  escape ?l ?\\] 0 "%d"))
