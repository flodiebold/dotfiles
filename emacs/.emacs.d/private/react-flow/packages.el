;;; packages.el --- React-flow layer packages file for spacemacs

(defconst react-flow-packages
  '((jsx-flow-mode :location
                   (recipe
                    :fetcher github
                    :repo "flodiebold/jsx-flow-mode"))))

(defun react-flow/init-jsx-flow-mode ()
  (use-package jsx-flow-mode
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . jsx-flow-mode))
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-flow-mode))
      (evil-leader/set-key-for-mode 'jsx-flow-mode "gg" 'jsx-flow/get-def-at-point))))
