;;; packages.el --- prettier layer packages file for spacemacs

(setq prettier-packages '((prettier-js :location local)))

(defun prettier/init-prettier-js ()
  (use-package prettier-js
    ;; :config
    ;; (add-hook 'flowtype-mode-hook
    ;;           (lambda () (add-hook 'before-save-hook
    ;;                                (lambda () (prettier)))))
    ))
