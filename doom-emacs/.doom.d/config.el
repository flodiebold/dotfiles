;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Florian Diebold"
      user-mail-address "flodiebold@gmail.com")

;; uniquify does not work in doom emacs, https://github.com/doomemacs/doomemacs/issues/4179
;; (setq uniquify-buffer-name-style 'forward)

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
(setq lsp-enable-on-type-formatting t)

;;; remove yasnippet from company backends for LSP buffers. I don't want a bunch
;;; of snippets being suggested all the time
(setq +lsp-company-backends 'company-capf)

(after! smartparens
  (ad-disable-advice 'company--insert-candidate 'after 'sp-company--insert-candidate))

;; Rust config
(setq lsp-rust-server 'rust-analyzer)
(setenv "RUST_SRC_PATH") ;; just to make sure this isn't set
(setq lsp-rust-analyzer-server-command '("env"
                                         "RA_LOG=error,rust_analyzer::config=info,project_model=info"
                                         "RA_PROFILE=*>400"
                                         "RUST_BACKTRACE=1"
                                         "rust-analyzer"))
(setq lsp-rust-analyzer-completion-add-call-argument-snippets nil)
(setq lsp-rust-analyzer-call-info-full nil)
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(setq lsp-rust-analyzer-display-chaining-hints t)
(setq lsp-rust-analyzer-display-parameter-hints t)
(setq lsp-rust-analyzer-max-inlay-hint-length 20)
(setq lsp-rust-analyzer-diagnostics-enable-experimental t)
(setq lsp-rust-analyzer-cargo-watch-enable nil)

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

;; WGSL
(use-package! wgsl-mode
  :mode "\\.wgsl\\'")

;; Treesitter / Combobulate
(use-package! treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
              '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
                (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                (toml "https://github.com/tree-sitter/tree-sitter-toml")
                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.20.4" "src"))
                (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (dolist (mapping
         '((python-mode . python-ts-mode)
           (css-mode . css-ts-mode)
           (typescript-mode . typescript-ts-mode)
           (js2-mode . js-ts-mode)
           (bash-mode . bash-ts-mode)
           (css-mode . css-ts-mode)
           (json-mode . json-ts-mode)
           (js-json-mode . json-ts-mode)
           (rust-mode . rust-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  )

(use-package! lsp-mode
  :hook ((rust-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)))

(use-package! combobulate
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)
   (rust-ts-mode . combobulate-mode)))

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
