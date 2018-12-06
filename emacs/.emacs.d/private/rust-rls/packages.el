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
    company
    ;; racer
    flycheck
    (helm-xref :requires helm)
    rust-mode
    toml-mode
    lsp-rust
    ))

;; TODO: format via rls

(defun rust-rls/init-helm-xref ()
  (use-package helm-xref
    :defer t
    :init
    (progn
      ;; This is required to make xref-find-references not give a prompt.
      ;; xref-find-references asks the identifier (which has no text property) and then passes it to lsp-mode, which requires the text property at point to locate the references.
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29619
      (setq xref-prompt-for-identifier
            '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references spacemacs/jump-to-definition))

      ;; Use helm-xref to display xref.el results.
      (setq xref-show-xrefs-function #'helm-xref-show-xrefs)
      )))

(defun rust-rls/init-cargo ()
  (use-package cargo
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'rust-mode "mc" "cargo")
      (spacemacs/declare-prefix-for-mode 'rust-mode "mt" "cargo test")
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "," 'lsp-ui-sideline-apply-code-actions
        "c." 'cargo-process-repeat
        "cC" 'cargo-process-clean
        "cX" 'cargo-process-run-example
        "cc" 'cargo-process-build
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

(defun rust-rls/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'rust-mode))

(defun rust-rls/init-rust-mode ()
  (use-package rust-mode
    :defer t
    :init
    (progn
      (add-hook 'rust-mode-hook #'lsp)
      (spacemacs/lsp-bind-keys-for-mode 'jsx-flow-mode)
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "=b" 'rust-format-buffer
        "q" 'spacemacs/rust-quick-run)
      ;; (evil-define-key 'insert rust-mode-map
      ;;   (kbd ".") 'rustrls/completing-dot)
      )))

(defun rust-rls/init-toml-mode ()
  (use-package toml-mode
    :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'"))

(defun rust-rls/post-init-company ()
  (push 'company-lsp company-backends-rust-mode)
  (spacemacs|add-company-hook rust-mode)
  ;; (add-hook 'rust-mode-hook
  ;;           (lambda ()
  ;;             (setq-local company-tooltip-align-annotations t)))
  )

(defun rust-rls/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; Don't pair lifetime specifiers
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

;; TODO: use xref-find-definitions directly in spacemacs-jump-handlers

(defun rust-rls/init-lsp-rust ()
  (use-package lsp-rust
    :after lsp-mode))
