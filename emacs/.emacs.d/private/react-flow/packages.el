;;; packages.el --- React-flow layer packages file for spacemacs

(defconst react-flow-packages
  '((jsx-flow-mode :location
                   (recipe
                    :fetcher github
                    :branch "lsp"
                    :repo "flodiebold/jsx-flow-mode"))
    company
    (prettier-js :location local)))

(defvar-local react-flow--prettier-enabled t)

(defun react-flow/toggle-prettier-on-save ()
  (interactive)
  (setq react-flow--prettier-enabled (not react-flow--prettier-enabled)))

(defun react-flow/init-prettier-js ()
  (use-package prettier-js
    :config
    (progn
      (add-hook 'jsx-flow-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook
                            (lambda ()
                              (when react-flow--prettier-enabled
                                (prettier))) nil t))))))

(defun react-flow/init-jsx-flow-mode ()
  (use-package jsx-flow-mode
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . jsx-flow-mode))
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-flow-mode)))))

(defun react-flow/post-init-company ()
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends (company-lsp company-jsx-flow-import-backend)
          :modes jsx-flow-mode)
        (add-hook 'jsx-flow-mode-hook
                  (lambda ()
                    (setq-local company-tooltip-align-annotations t))))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
