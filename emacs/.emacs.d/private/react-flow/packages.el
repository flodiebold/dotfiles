;;; packages.el --- React-flow layer packages file for spacemacs

(setq react-flow-packages '(
                            web-mode
                            flycheck
                            ))

(defun react-flow/init-web-mode ()
  (use-package web-mode))
