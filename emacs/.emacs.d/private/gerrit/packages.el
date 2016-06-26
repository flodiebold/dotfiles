
(setq gerrit-packages
      '(magit-gerrit))

(defun gerrit/init-magit-gerrit ()
  (with-eval-after-load 'magit
    (require 'magit-gerrit)
    (define-key magit-mode-map
      "P" 'magit-gerrit-popup)))
