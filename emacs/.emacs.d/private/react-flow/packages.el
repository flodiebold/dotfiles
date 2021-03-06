;;; packages.el --- React-flow layer packages file for spacemacs

(defconst react-flow-packages
  '((jsx-flow-mode :location
                   (recipe
                    :fetcher github
                    :repo "flodiebold/jsx-flow-mode"))
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
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-flow-mode))
      (evil-leader/set-key-for-mode 'jsx-flow-mode "gg" 'jsx-flow/get-def-at-point))))
