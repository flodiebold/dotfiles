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

(setq rust-rls-packages
  '(
    cargo
    ;; company
    ;; racer
    flycheck
    ;; (lsp-flycheck :toggle (configuration-layer/package-usedp 'flycheck))
    ;; ggtags
    ;; helm-gtags
    rust-mode
    toml-mode
    lsp-mode
    lsp-rust
    ;; (lsp-mode :location "/home/florian/Projekte/lsp-mode")
    ;; (lsp-rust :location "/home/florian/Projekte/lsp-rust")
    ))

(defun rust-rls/init-cargo ()
  (use-package cargo
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'rust-mode "mc" "cargo")
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "c." 'cargo-process-repeat
        "cC" 'cargo-process-clean
        "cX" 'cargo-process-run-example
        "cc" 'cargo-process-build
        "cd" 'cargo-process-doc
        "ce" 'cargo-process-bench
        "cf" 'cargo-process-current-test
        "cf" 'cargo-process-fmt
        "ci" 'cargo-process-init
        "cn" 'cargo-process-new
        "co" 'cargo-process-current-file-tests
        "cs" 'cargo-process-search
        "cu" 'cargo-process-update
        "cx" 'cargo-process-run
        "t" 'cargo-process-test))))

(defun rust-rls/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'rust-mode))

(defun rust-rls/init-rust-mode ()
  (use-package rust-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "=" 'rust-format-buffer
        "q" 'spacemacs/rust-quick-run)
      ;; (evil-define-key 'insert rust-mode-map
      ;;   (kbd ".") 'rustrls/completing-dot)
      )))

(defun rust-rls/init-toml-mode ()
  (use-package toml-mode
    :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'"))

;; (defun rust-rls/post-init-company ()
;;   (push 'company-capf company-backends-rust-mode)
;;   (spacemacs|add-company-hook rust-mode)
;;   (add-hook 'rust-mode-hook
;;             (lambda ()
;;               (setq-local company-tooltip-align-annotations t))))

(defun rust-rls/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; Don't pair lifetime specifiers
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

;; (defun rust-rls/init-racer ()
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-copy-env "RUST_SRC_PATH"))

;;   (use-package racer
;;     :defer t
;;     :init
;;     (progn
;;       (spacemacs/add-to-hook 'rust-mode-hook '(racer-mode eldoc-mode))
;;       (spacemacs/declare-prefix-for-mode 'rust-mode "mg" "goto")
;;       (add-to-list 'spacemacs-jump-handlers-rust-mode 'racer-find-definition)
;;       (spacemacs/declare-prefix-for-mode 'rust-mode "mh" "help")
;;       (spacemacs/set-leader-keys-for-major-mode 'rust-mode
;;         "hh" 'spacemacs/racer-describe))
;;     :config
;;     (progn
;;       (spacemacs|hide-lighter racer-mode)
;;       (evilified-state-evilify-map racer-help-mode-map
;;         :mode racer-help-mode))))

;; TODO: use xref-find-definitions directly in spacemacs-jump-handlers

(defun rust-rls/init-lsp-mode ()
  (use-package lsp-mode
    :init
    (add-hook 'rust-mode-hook 'lsp-mode)
    :config
    (use-package lsp-flycheck
      :ensure f ; comes with lsp-mode
      :after flycheck)))

(defun rust-rls/init-lsp-rust ()
  (use-package lsp-rust
    :after lsp-mode))
