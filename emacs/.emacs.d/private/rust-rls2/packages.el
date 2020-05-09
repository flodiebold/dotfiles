;;; packages.el --- Rust Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Chris Hoeppner <me@mkaito.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq rust-rls2-packages
  '(cargo
    rust-mode
    toml-mode))

;; TODO: look into disabling some expand-region expansions
;; TODO: look into using / stealing stuff from rustic-mode

(defun rust-rls2/init-cargo ()
  (use-package cargo
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'rust-mode "mc" "cargo")
      (spacemacs/declare-prefix-for-mode 'rust-mode "mt" "cargo test")
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "c." 'cargo-process-repeat
        "cC" 'cargo-process-clean
        "cX" 'cargo-process-run-example
        "cc" 'cargo-process-check
        "cd" 'cargo-process-doc
        "ce" 'cargo-process-bench
        "cf" 'cargo-process-fmt
        "ci" 'cargo-process-init
        "cn" 'cargo-process-new
        "cs" 'cargo-process-search
        "cu" 'cargo-process-update
        "cx" 'cargo-process-run
        "ta" 'cargo-process-test
        "tf" 'cargo-process-current-file-tests
        "tt" 'cargo-process-current-test))))

(defun rust-rls2/init-rust-mode ()
  (use-package rust-mode
    :defer t
    :init
    (progn
      (spacemacs/add-to-hook 'rust-mode-hook '(spacemacs//rust-rls2-setup-lsp))
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "==" 'rust-format-buffer
        "\r" 'lsp-rust-analyzer-run
        "," 'lsp-execute-code-action
        "q" 'spacemacs/rust-quick-run)
      (evil-define-key '(normal motion) rust-mode-map
        "N" 'lsp-rust-analyzer-join-lines))))

(defun rust-rls2/init-toml-mode ()
  (use-package toml-mode
    :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'"))

(defun rust-rls2/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; Don't pair lifetime specifiers
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

;; TODO: use xref-find-definitions directly in spacemacs-jump-handlers
