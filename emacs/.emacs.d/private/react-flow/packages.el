;;; packages.el --- React-flow layer packages file for spacemacs

(setq react-flow-packages '(
                            json
                            web-mode
                            flycheck
                            dash
                            ))

(defun react-flow/init-web-mode ()
  (use-package web-mode))
