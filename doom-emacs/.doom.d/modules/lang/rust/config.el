;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

;; work around bug with disabled optional libraries
(defun rust-before-save-hook ())
(defun rust-after-save-hook ())

(use-package! rust-mode
  :mode ("\\.rs$" . rust-mode)
  :init
  (setq rust-load-optional-libraries nil)
  :config
  (add-hook 'rust-mode-hook #'lsp!)
  (define-key rust-mode-map [remap evil-join] 'lsp-rust-analyzer-join-lines)
  (map! :map rust-mode-map
        :localleader
        (:prefix ("r" . "run")
         :desc "run" "r" #'lsp-rust-analyzer-run
         :desc "run again" "a" #'lsp-rust-analyzer-rerun)
        (:prefix ("m" . "macro")
         :desc "expand macro" "x" #'lsp-rust-analyzer-expand-macro)))
