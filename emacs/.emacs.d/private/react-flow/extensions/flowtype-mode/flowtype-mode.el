;;; flowtype-mode.el --- Derived mode for JSX with flow types

(define-derived-mode flowtype-mode
  web-mode "FlowJSX"
  "Major mode for JSX with flow types."
  (setq web-mode-content-type "jsx")
  (setq web-mode-markup-indent-offset 2)
  (flycheck-mode 1))

(add-to-list 'magic-mode-alist '("/\\* @flow" . flowtype-mode))

(flycheck-define-command-checker 'javascript-flowtype
  "A JavaScript syntax and style checker using Flow."
  :command '("flow" source-original)
  :error-patterns
  '((error line-start
           (file-name)
           ":"
           line
           ":"
           (minimal-match (one-or-more not-newline))
           ": "
           (message (minimal-match (and (one-or-more anything) "\n")))
           line-end))
  :modes '(flowtype-mode))

(add-to-list 'flycheck-checkers 'javascript-flowtype)

(provide 'flowtype-mode)
