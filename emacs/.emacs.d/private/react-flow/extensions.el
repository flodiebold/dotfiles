;;; extensions.el --- React-flow Layer Extensions File for Spacemacs

(setq react-flow-post-extensions '(flowtype-mode))

(defun react-flow/init-flowtype-mode ()
  (use-package flowtype-mode
    :config
    (progn
      (evil-leader/set-key-for-mode 'flowtype-mode "gg" 'flowtype/get-def-at-point))))
