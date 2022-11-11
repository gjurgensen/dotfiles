(require 'autothemer)

(autothemer-deftheme gruvbox "Inspired by gruvbox for vim"
  ((((class color) (min-colors #xFFFFFF)))

   (gruv-red    "#FB4934")
   (gruv-green  "#B8BB26")
   (gruv-blue   "#83A598")
   (gruv-purple "#D3869B")
   (gruv-yellow "#FABD2F")
   (gruv-orange "#FE8019")
   (gruv-cyan   "#8EC07C")
   (gruv-fg     "#ebdbb2")
   (gruv-bg     "#282828")
   (gruv-bg1    "#3c3836")
   (gruv-bg2    "#504945")
   (gruv-bg4    "#a89984")
   (gruv-gray   "#928374"))

  ;; TODO: make background transparent in tui
  ((default                      (:foreground gruv-fg :background gruv-bg))
   (highlight                    (:foreground gruv-fg :background gruv-bg2))
   (cursor                       (:background gruv-fg))
   (region                       (:background gruv-bg2 :distant-foreground gruv-fg))
   (minibuffer-prompt            (:foreground gruv-green))
   (link                         (:foreground gruv-blue :underline t))
   (link-visited                 (:foreground gruv-purple :underline nil))
   (fringe                       (:background gruv-bg))
   (vertical-border              (:foreground gruv-gray))
   (border                       (:background gruv-gray))
   (window-divider               (:foreground gruv-gray))

   (mode-line                    (:foreground gruv-fg :background gruv-bg2))
   (mode-line-inactive           (:foreground gruv-bg4 :background gruv-bg1))
   (line-number                  (:foreground gruv-gray))
   (custom-variable-tag          (:foreground gruv-blue :bold t))
   (font-lock-builtin-face       (:foreground gruv-purple))
   (font-lock-comment-face       (:foreground gruv-gray :italic t))
   (font-lock-constant-face      (:foreground gruv-blue))
   (font-lock-function-name-face (:foreground gruv-cyan))
   (font-lock-keyword-face       (:foreground gruv-red))
   (font-lock-string-face        (:foreground gruv-green))
   (font-lock-variable-name-face (:foreground gruv-yellow))

   (proof-locked-face            (:background gruv-bg2 :distant-foreground gruv-fg))
   (whitespace-newline           (:foreground gruv-gray))
   (whitespace-space             (:foreground gruv-gray))
   (whitespace-tab               (:foreground gruv-gray))
   (whitespace-indentation       (:inherit 'whitespace-space))

   (compilation-info                          (:foreground gruv-green))
   (compilation-mode-line-fail                (:foreground gruv-red))
   (error                                     (:foreground gruv-orange :bold t))
   (success                                   (:foreground gruv-green :bold t))
   (warning                                   (:foreground gruv-red :bold t))

   ;; TODO: rainbow-delimiter-depth-n-face
   )

  (custom-theme-set-variables 'gruvbox
                              `(ansi-color-names-vector [,gruv-red
                                                         ,gruv-green
                                                         ,gruv-blue
                                                         ,gruv-purple
                                                         ,gruv-yellow
                                                         ,gruv-orange
                                                         ,gruv-cyan])))

(provide-theme 'gruvbox)
