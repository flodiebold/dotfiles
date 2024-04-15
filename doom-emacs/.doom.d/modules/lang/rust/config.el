;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

(after! combobulate
  (load! "combobulate-rust")
  (add-to-list 'combobulate-setup-functions-alist '(rust . combobulate-rust-setup)))

;; work around bug with disabled optional libraries
(defun rust-before-save-hook ())
(defun rust-after-save-hook ())

;; TODO: doesn't update after LSP code action
;; TODO: add navigation between holes
;; TODO: overwriting of holes
;; TODO: cleanup

(defvar-local rust-holes--cursor nil)

(defvar rust-hole-query nil)

(defun rust-holes--update-node (node)
  (while (eq (tsc-node-type node) 'token_tree)
    (setq node (tsc-get-parent node)))
  (remove-overlays (tsc-node-start-position node)
                   (tsc-node-end-position node)
                   'rust-hole t)
  (let ((matches (tsc-query-captures rust-hole-query
                                      node
                                      #'ts--buffer-substring-no-properties
                                      rust-holes--cursor)))
      (seq-do (lambda (match)
                (when (and (eq (car match) 'hole)
                           (not (tsc-node-has-error-p (cdr match))))
                  (let ((o (make-overlay (tsc-node-start-position (cdr match))
                                (tsc-node-end-position (cdr match)))))
                    ;; (overlay-put o 'invisible t)
                    (overlay-put o 'display "•")
                    (overlay-put o 'evaporate t)
                    (overlay-put o 'rust-hole t))))
              matches)
      nil))

(defun rust-holes-update (&optional old-tree)
  (unless rust-holes--cursor
    (setq rust-holes--cursor (tsc-make-query-cursor)))
  (if old-tree
      (seq-doseq (range (tsc-changed-ranges old-tree tree-sitter-tree))
        (pcase-let* ((`[,beg-byte ,end-byte] range)
                     (node (tsc-get-descendant-for-byte-range (tsc-root-node tree-sitter-tree) beg-byte end-byte)))
          (rust-holes--update-node node)))
    ;; First parse.
    (rust-holes--update-node (tsc-root-node tree-sitter-tree))))

(defun enable-tree-sitter ()
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (tree-sitter-mode)
  (tree-sitter-hl-mode)
  ;; (add-hook 'tree-sitter-after-change-functions #'rust-holes-update nil :local)
  ;; (add-hook 'tree-sitter-after-change-functions #'rust-holes-update nil :local)
  ;; (unless rust-holes--cursor
  ;;   (setq rust-holes--cursor (tsc-make-query-cursor))
  ;;   ;; Invalidate the buffer, only if we were actually disabled previously.
  ;;   (when tree-sitter-tree (rust-holes-update)))
  ;; (unless rust-hole-query
  ;;   (setq rust-hole-query
  ;;         (tsc-make-query (tree-sitter-require 'rust)
  ;;                         [((macro_invocation
  ;;                            macro: (identifier) @id
  ;;                            "!"
  ;;                            (.eq? @id "todo")) @hole)])))
  )

(use-package! rust-mode
  :mode ("\\.rs$" . rust-mode)
  :init
  (setq rust-load-optional-libraries nil)
  :config
  (add-hook 'rust-mode-hook #'lsp!)
  ;; (add-hook 'rust-mode-hook #'enable-tree-sitter)
  ;; (add-hook 'rust-mode-hook #'rust-mode-setup-prettify-symbols)
  (define-key rust-mode-map [remap evil-join] 'lsp-rust-analyzer-join-lines)
  (map! :map rust-mode-map
        :localleader
        (:prefix ("r" . "run")
         :desc "run" "r" #'lsp-rust-analyzer-run
         :desc "run again" "a" #'lsp-rust-analyzer-rerun)
        (:prefix ("m" . "macro")
         :desc "expand macro" "x" #'lsp-rust-analyzer-expand-macro)))

(use-package! tree-sitter
  :defer t)
(use-package! tree-sitter-langs
  :defer t)
