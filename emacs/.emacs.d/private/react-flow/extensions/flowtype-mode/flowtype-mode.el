;;; flowtype-mode.el --- Derived mode for JSX with flow types

(require 'json)

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

(defun flowtype//call-flow-on-current-buffer (&rest args)
  "Calls flow with args on the current buffer, returns the result."
  (let ((buf (generate-new-buffer "flow")))
    (message "calling %s..." args)
    (unwind-protect
        (let ((result (apply 'call-process-region (point-min) (point-max) "flow" nil buf nil args))
              (output (with-current-buffer buf (buffer-string))))
          (message output)
          (when (= result 0)
              output))
      (kill-buffer buf))))

(defun flowtype//json-flow-call (&rest args)
  "Calls flow on the current buffer passing --json, parses the result."
  (let* ((args (append args '("--json")))
         (output (apply 'flowtype//call-flow-on-current-buffer args)))
    (when output
      (json-read-from-string output))))

(defun flowtype//pos-to-flow-location (pos)
  "Returns a list of (line col) for pos in the current buffer."
  (let ((line (line-number-at-pos pos))
        (col (1+ (column-number-at-pos pos))))
    (list (number-to-string line) (number-to-string col))))

(defun flowtype//get-def (pos)
  "Calls flow to get the definition location of the thing at pos, returns the result."
  (let* ((loc (flowtype//pos-to-flow-location pos))
         (filename (buffer-file-name))
         (answer (apply 'flowtype//json-flow-call "get-def" filename loc)))
    answer))

(defun flowtype//show-flow-loc (loc)
  "Takes a flow location info and shows it."
  (let* ((filename (cdr (assq 'path loc)))
         (line (cdr (assq 'line loc)))
         (col (cdr (assq 'start loc))))
    (when (not (eq filename ""))
      (find-file filename)
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char (1- col)))))

(defun flowtype/get-def-at-point ()
  "Show the definition of the thing at point using flow."
  (interactive)
  (let ((loc (flowtype//get-def (point))))
    (flowtype//show-flow-loc loc)))

(provide 'flowtype-mode)
