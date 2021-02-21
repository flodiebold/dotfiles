;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Florian Diebold"
      user-mail-address "flodiebold@gmail.com")

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
(setq doom-theme 'solarized-dark)
(setq solarized-use-variable-pitch nil)
(setq solarized-scale-org-headlines nil)

;; Org config
(setq org-directory "~/org/")
(setq org-startup-indented nil) ;; don't indent stuff

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

;; General LSP stuff
(setq lsp-ui-sideline-enable nil)
;; (setq evil-goto-definition-functions '(evil-goto-definition-xref))

;; Rust config
(setq lsp-rust-server 'rust-analyzer)
(setenv "RUST_SRC_PATH")
(setq lsp-rust-analyzer-server-command '("env"))
(setq lsp-rust-analyzer-server-args '("RA_LOG=error,rust_analyzer::config" "RA_PROFILE=*>400" "RUST_BACKTRACE=1" "rust-analyzer"))
(setq lsp-rust-analyzer-completion-add-call-argument-snippets nil)
(setq lsp-rust-analyzer-call-info-full nil)
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(setq lsp-rust-analyzer-display-chaining-hints t)
(setq lsp-rust-analyzer-display-parameter-hints t)
(setq lsp-rust-analyzer-proc-macro-enable t)
(setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
(setq lsp-rust-analyzer-max-inlay-hint-length 20)

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
