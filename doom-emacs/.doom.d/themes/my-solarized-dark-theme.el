;;; ../Projekte/dotfiles/doom-emacs/.doom.d/my-solarized-theme.el -*- lexical-binding: t; -*-

(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(defvar my-solarized-theme
  '("My solarized child theme."
    (custom-theme-set-faces
     theme-name
     `(default ((,class (:foreground ,base0 :background ,base03))))
     ;; `(font-lock-builtin-face ((,class (:foreground ,base0 :weight ,s-maybe-bold))))
     ;; `(font-lock-comment-delimiter-face
     ;;   ((,class (:foreground ,base01 :slant ,s-maybe-italic))))
     ;; `(font-lock-comment-face ((,class (:foreground ,base01))))
     `(font-lock-constant-face ((,class (:foreground ,violet))))
     ;; `(font-lock-doc-face ((,class (:foreground ,base01 :slant ,s-maybe-italic))))
     `(font-lock-function-name-face ((,class (:foreground ,base0))))
     ;; `(font-lock-keyword-face ((,class (:foreground ,base01
     ;;                                    :weight ,s-maybe-bold
     ;;                                    ))))
     ;; `(font-lock-negation-char-face ((,class (:foreground ,yellow :weight bold))))
     ;; `(font-lock-preprocessor-face ((,class (:foreground ,base01
                                             ;; :weight bold
     ;;                                         :slant ,s-maybe-italic
     ;;                                         ))))
     ;; `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow :weight bold))))
     ;; `(font-lock-regexp-grouping-backslash ((,class (:foreground ,green :weight bold))))
     ;; `(font-lock-string-face ((,class (:foreground ,cyan))))
     `(font-lock-type-face ((,class (:foreground ,blue))))
     `(font-lock-variable-name-face ((,class (:foreground ,yellow))))
     ;; `(font-lock-warning-face ((,class (:inherit error :weight bold))))
     `(lsp-face-semhl-macro ((,class (:foreground ,base0 :weight ,s-maybe-bold))))
     `(lsp-face-semhl-rust-attribute-element ((,class (:slant ,s-maybe-italic))))
     `(lsp-face-semhl-punctuation ((,class (:foreground ,base01))))
     `(lsp-face-semhl-keyword-self ((,class (:foreground ,yellow :weight ,s-maybe-bold))))
     `(lsp-face-semhl-namespace ((,class (:foreground ,blue))))
     `(lsp-face-semhl-consuming ((,class (:weight ,s-maybe-bold))))
     `(lsp-face-semhl-property ((,class (:foreground ,base0))))
     ;; `(lsp-face-semhl-declaration ((,class ())))
     )))

(deftheme my-solarized-dark "The dark variant of the Solarized colour theme")
(solarized-with-color-variables 'dark 'my-solarized-dark
  solarized-dark-color-palette-alist my-solarized-theme)

(provide-theme 'my-solarized-dark)
