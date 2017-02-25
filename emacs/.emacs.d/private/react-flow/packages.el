;;; packages.el --- React-flow layer packages file for spacemacs

(setq react-flow-packages '(web-mode
                            (flowtype-mode :location local)))

(defun react-flow/init-web-mode ()
  (use-package web-mode))

(defun react-flow/init-flowtype-mode ()
  (use-package flowtype-mode
    :config
    (progn
      (evil-leader/set-key-for-mode 'flowtype-mode "gg" 'flowtype/get-def-at-point))))
