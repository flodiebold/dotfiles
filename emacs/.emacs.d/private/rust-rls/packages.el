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
    ;; (lsp-flycheck :toggle (configuration-layer/package-usedp 'flycheck))
    (company-lsp :requires company)
    (helm-xref :requires helm)
    rust-mode
    toml-mode
    lsp-mode
    lsp-ui
    lsp-rust
    ;; (lsp-mode :location "/home/florian/Projekte/lsp-mode")
    ;; (lsp-rust :location "/home/florian/Projekte/lsp-rust")
    ))

;; TODO: get code actions to work?
;; TODO: format via rls

(defun rust-rls/init-company-lsp ()
  (use-package company-lsp
    :defer t
    :init
    ;; Language servers have better idea filtering and sorting,
    ;; don't filter results on the client side.
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil)
    ;; (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common)
    ;; (spacemacs|add-company-backends :backends company-lsp :modes rust-mode)
    ))

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
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "=" 'rust-format-buffer
        "q" 'spacemacs/rust-quick-run)
      (add-hook 'rust-mode-hook #'lsp-rust-enable)
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
    (progn
      (add-hook 'lsp-mode-hook #'lsp-ui-mode)

      ;; Disable lsp-flycheck.el in favor of lsp-ui-flycheck.el
      (setq lsp-enable-flycheck nil)

      ;; (spacemacs|diminish lsp-mode " Ⓛ" " L")
      )))

(defun rust-rls/init-lsp-ui ()
  (use-package lsp-ui
    :config
    (progn
      ;; (lsp//sync-peek-face)
      ;; (add-hook 'spacemacs-post-theme-change-hook #'lsp//sync-peek-face)
      )))

(defun rust-rls/init-lsp-rust ()
  (use-package lsp-rust
    :after lsp-mode))
