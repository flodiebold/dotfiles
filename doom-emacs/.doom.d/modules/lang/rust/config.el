;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

(use-package! rust-mode
  :mode ("\\.rs$" . rust-mode)
  :init
  (setq rust-load-optional-libraries nil)
  :config
  (add-hook 'rust-mode-hook #'lsp!))
