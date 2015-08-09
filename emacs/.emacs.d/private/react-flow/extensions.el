;;; extensions.el --- React-flow Layer Extensions File for Spacemacs

(setq react-flow-post-extensions '(flowtype-mode))

(defun react-flow/init-flowtype-mode ()
  (use-package flowtype-mode))
