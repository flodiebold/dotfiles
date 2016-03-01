;;; flowtype-mode.el --- Derived mode for JSX with flow types -*- lexical-binding: t -*-

(require 'json)

(add-to-list 'magic-mode-alist '("/\\* @flow" . flowtype-mode))

(defun flowtype//column-number-at-pos (pos)
  "column number at pos"
  (save-excursion (goto-char pos) (current-column)))

(with-eval-after-load 'flycheck
  (flycheck-define-command-checker 'javascript-flowtype
    "A JavaScript syntax and style checker using Flow."
    :command '("flow" "status" "--old-output-format")
    :error-patterns
    '((error line-start
             (file-name)
             ":"
             line
             ":"
             column;(minimal-match (one-or-more not-newline))
             ","
             (minimal-match (one-or-more not-newline))
             ": "
             (message (minimal-match (and (one-or-more anything) "\n")))
             line-end))
    :modes '(flowtype-mode))

  (add-to-list 'flycheck-checkers 'javascript-flowtype))

(defmacro flowtype|measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let* ((time (current-time))
          (result ,@body))
     (message "%.06f" (float-time (time-since time)))
     result))

(defun flowtype//call-flow-on-current-buffer (&rest args)
  "Calls flow with args on the current buffer, returns the result."
  (flowtype|measure-time
   (let ((buf (generate-new-buffer "*flow*")))
     (unwind-protect
         (let ((result (apply 'call-process-region (point-min) (point-max) "flow" nil buf nil args))
               (output (with-current-buffer buf (buffer-string))))
           (when (= result 0)
             output))
       (kill-buffer buf)))))

(defun flowtype//call-flow-on-current-buffer-async (result-handler &rest args)
  "Calls flow with args on the current buffer asynchronously; passes the result to result-handler."
  (let* ((buf (generate-new-buffer "*flow*"))
         (process (apply #'start-process "flow" buf "flow" args)))
    (set-process-sentinel process
                          (lambda (process event)
                            (when (equal 'exit (process-status process))
                              (let ((output (with-current-buffer (process-buffer process) (buffer-string))))
                                (kill-buffer (process-buffer process))
                                (funcall result-handler output)))))
    (process-send-region process (point-min) (point-max))
    (process-send-eof process)))

(defun flowtype//json-flow-call (&rest args)
  "Calls flow on the current buffer passing --json, parses the result."
  (let* ((args (append args '("--json")))
         (output (apply #'flowtype//call-flow-on-current-buffer args)))
    (when output
      (json-read-from-string output))))

(defun flowtype//json-flow-call-async (result-handler &rest args)
  "Calls flow on the current buffer passing --json asynchronously; parses the result and gives it to result-handler."
  (let ((args (append args '("--json")))
        (handler (lambda (output) (funcall result-handler (json-read-from-string output)))))
    (apply #'flowtype//call-flow-on-current-buffer-async handler args)))

(defun flowtype//pos-to-flow-location (pos)
  "Returns a list of (line col) for pos in the current buffer."
  (let ((line (line-number-at-pos pos))
        (col (1+ (flowtype//column-number-at-pos pos))))
    (list (number-to-string line) (number-to-string col))))

(defun flowtype//get-def (pos)
  "Calls flow to get the definition location of the thing at pos, returns the result."
  (let* ((loc (flowtype//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'flowtype//json-flow-call "get-def" filename loc)))

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

(defun flowtype//type-at-pos-async (result-handler pos)
  "Calls flow to get the type at pos asynchronously; passes the result to result-handler."
  (let* ((loc (flowtype//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'flowtype//json-flow-call-async result-handler "type-at-pos" filename loc)))

(defun flowtype//eldoc-show-type-info (data)
  "Shows the passed type info using eldoc."
  (let ((type (cdr (assq 'type data))))
    (when (not (equal "(unknown)" type))
      (eldoc-message (cdr (assq 'type data))))))

(defun flowtype/eldoc-show-type-at-point ()
  "Shows type at point."
  (interactive)
  (flowtype//type-at-pos-async #'flowtype//eldoc-show-type-info (point))
  nil)

(define-derived-mode flowtype-mode
  web-mode "FlowJSX"
  "Major mode for JSX with flow types."
  (setq web-mode-content-type "jsx")
  (setq web-mode-markup-indent-offset 2)
  (set (make-local-variable 'eldoc-documentation-function) #'flowtype/eldoc-show-type-at-point)
  (make-local-variable 'flowtype--ast)
  (turn-on-eldoc-mode)
  (flycheck-mode 1))

(provide 'flowtype-mode)
