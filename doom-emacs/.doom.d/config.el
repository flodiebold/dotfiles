;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Florian Diebold"
      user-mail-address "flodiebold@gmail.com")

(setq uniquify-buffer-name-style 'forward)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :name "Input"
                           :size 23
                           :weight 'normal
                           :width 'normal
                           :powerline-scale 1.0))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'my-solarized-dark)
(setq solarized-use-variable-pitch nil)
(setq solarized-use-more-italic t)
(setq solarized-scale-org-headlines nil)
(setq evil-normal-state-cursor '(box "orange")
      evil-insert-state-cursor '(bar "medium sea green")
      evil-visual-state-cursor '(hollow "orange"))

;; After splitting windows, prompt to switch buffer
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)

;; Global keys
(setq doom-localleader-key ",")
(after! evil
  (evil-declare-ignore-repeat 'recenter-top-bottom)
  (evil-declare-ignore-repeat 'move-beginning-of-line)
  (evil-declare-ignore-repeat 'doom/backward-to-bol-or-indent))

;; Org config
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/roam")
;; (setq org-startup-indented nil) ;; don't indent stuff -- this doesn't seem to work, and I've gotten used to the indented mode

;; line numbers
(setq display-line-numbers-type 'relative)

;; disable visual-line-mode -- Doom enables this by default, but I find it super
;; annoying and it doesn't work very well with evil editing functions
(remove-hook 'text-mode-hook #'visual-line-mode)

;; hscroll by centering cursor instead of tiny steps
(setq hscroll-step 0)

;; General LSP stuff
(setq lsp-ui-sideline-enable nil)
;; (setq evil-goto-definition-functions '(evil-goto-definition-xref))
(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.2)
(setq lsp-file-watch-threshold 2000)

(setq lsp-semantic-tokens-enable nil)
(setq lsp-semantic-tokens-apply-modifiers t)
(setq lsp-auto-execute-action nil)

;;; remove yasnippet from company backends for LSP buffers. I don't want a bunch
;;; of snippets being suggested all the time
(setq +lsp-company-backends 'company-capf)

(after! smartparens
  (ad-disable-advice 'company--insert-candidate 'after 'sp-company--insert-candidate))

;; Rust config
(setq lsp-rust-server 'rust-analyzer)
(setenv "RUST_SRC_PATH") ;; just to make sure this isn't set
(setq lsp-rust-analyzer-server-command '("env" "RA_LOG=error,rust_analyzer::config" "RA_PROFILE=*>400" "RUST_BACKTRACE=1" "rust-analyzer"))
(setq lsp-rust-analyzer-completion-add-call-argument-snippets nil)
(setq lsp-rust-analyzer-call-info-full nil)
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(setq lsp-rust-analyzer-display-chaining-hints t)
(setq lsp-rust-analyzer-display-parameter-hints t)
(setq lsp-rust-analyzer-proc-macro-enable t)
(setq lsp-rust-analyzer-cargo-run-build-scripts t)
(setq lsp-rust-analyzer-max-inlay-hint-length 20)

(defadvice! lsp-rust-analyzer-additional-options (fun)
  :around 'lsp-rust-analyzer--make-init-options
  (let ((opts (funcall fun)))
    (append opts '(:experimental (:procAttrMacros t)))))

;; LSP semantic highlighting fixes / theme improvements
;; add missing method token type
(after! lsp-semantic-tokens
  (defface lsp-face-semhl-attribute
    '((t :inherit font-lock-comment-face))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-boolean
    '((t :inherit font-lock-constant-face))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-builtin-type
    '((t :inherit font-lock-type-face))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-lifetime
    '((t :inherit font-lock-type-face))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-keyword-self
    '((t :inherit font-lock-keyword-face))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-type-alias
    '((t :inherit font-lock-type-face))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-union
    '((t :inherit font-lock-type-face))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-unresolved-reference
    '((t :inherit font-lock-warning-face))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-rust-format-specifier
    '((t :inherit font-lock-variable-name-face))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-punctuation
    '((t))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-parenthesis
    '((t :inherit lsp-face-semhl-punctuation))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-bracket
    '((t :inherit lsp-face-semhl-punctuation))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-brace
    '((t :inherit lsp-face-semhl-punctuation))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-angle
    '((t :inherit lsp-face-semhl-punctuation))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-comma
    '((t :inherit lsp-face-semhl-punctuation))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-colon
    '((t :inherit lsp-face-semhl-punctuation))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-semicolon
    '((t :inherit lsp-face-semhl-punctuation))
    "." :group 'lsp-faces)
  (defface lsp-face-semhl-dot
    '((t :inherit lsp-face-semhl-punctuation))
    "." :group 'lsp-faces)
  (setq lsp-semantic-token-faces
        (append lsp-semantic-token-faces
                '(("method" . lsp-face-semhl-method)
                  ("attribute" . lsp-face-semhl-attribute)
                  ("boolean" . lsp-face-semhl-boolean)
                  ("builtinType" . lsp-face-semhl-builtin-type)
                  ("lifetime" . lsp-face-semhl-lifetime)
                  ("selfKeyword" . lsp-face-semhl-keyword-self)
                  ("typeAlias" . lsp-face-semhl-type-alias)
                  ("union" . lsp-face-semhl-union)
                  ("unresolvedReference" . lsp-face-semhl-unresolved-reference)
                  ("formatSpecifier" . lsp-face-semhl-rust-format-specifier)
                  ("punctuation" . lsp-face-semhl-punctuation)
                  ("parenthesis" . lsp-face-semhl-parenthesis)
                  ("bracket" . lsp-face-semhl-bracket)
                  ("brace" . lsp-face-semhl-brace)
                  ("angle" . lsp-face-semhl-angle)
                  ("comma" . lsp-face-semhl-comma)
                  ("colon" . lsp-face-semhl-colon)
                  ("semicolon" . lsp-face-semhl-semicolon)
                  ("dot" . lsp-face-semhl-dot))))
  ;; enum looking different from struct looks weird
  (face-spec-set 'lsp-face-semhl-enum '((t :inherit font-lock-type-face)))
  (defface lsp-face-semhl-declaration
    '((t)) "." :group 'lsp-faces)
  (defface lsp-face-semhl-mutable
    '((t :underline t)) "." :group 'lsp-faces)
  (defface lsp-face-semhl-rust-attribute-element
    '((t)) "." :group 'lsp-faces)
  (defface lsp-face-semhl-rust-constant
    '((t)) "." :group 'lsp-faces)
  (defface lsp-face-semhl-control-flow
    '((t)) "." :group 'lsp-faces)
  (defface lsp-face-semhl-unsafe
    '((t)) "." :group 'lsp-faces)
  (defface lsp-face-semhl-consuming
    '((t)) "." :group 'lsp-faces)
  (defface lsp-face-semhl-callable
    '((t)) "." :group 'lsp-faces)
  (setq lsp-semantic-token-modifier-faces
        '(("declaration" . lsp-face-semhl-declaration)
          ("deprecated" . lsp-face-semhl-deprecated)
          ("readonly" . lsp-face-semhl-constant)
          ("mutable" . lsp-face-semhl-mutable)
          ("attribute" . lsp-face-semhl-rust-attribute-element)
          ("constant" . lsp-face-semhl-rust-constant)
          ("controlFlow" . lsp-face-semhl-control-flow)
          ("unsafe" . lsp-face-semhl-unsafe)
          ("consuming" . lsp-face-semhl-consuming)
          ("callable" . lsp-face-semhl-callable))))

;; YAML
(use-package! yaml-mode
  :mode "\\.ya?ml\\'")

;; Protobuf
(use-package! protobuf-mode
  :mode "\\.proto\\'")

;; AsciiDoc
(use-package! adoc-mode
  :mode "\\.adoc\\'")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
