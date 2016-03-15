;;; flowtype-mode.el --- Derived mode for JSX with flow types -*- lexical-binding: t -*-

(require 'json)
(require 'company)          ; for autocomplete

(defcustom flowtype:uncovered-type-background-color "#ff9999"
  "background-color for undefined types."
  :type 'string
  :group 'flowtype)

(add-to-list 'magic-mode-alist '("/\\* @flow" . flowtype-mode))

(defun flowtype//column-number-at-pos (pos)
  "column number at pos"
  (save-excursion (goto-char pos) (current-column)))

(defmacro flowtype|measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let* ((time (current-time))
          (result ,@body))
     (message "%.06f" (float-time (time-since time)))
     result))

(defun flowtype//call-flow-into-buffer (&rest args)
  "Calls flow with args on the current buffer, returns the result."
  (flowtype|measure-time
   (let ((buf (generate-new-buffer flowtype:buffer-name)))
     (apply 'call-process-region (point-min) (point-max) "flow" nil buf nil args)
     buf)))

(defun flowtype//call-flow-on-current-buffer (&rest args)
  "Calls flow with args on the current buffer, returns the result."
  (flowtype|measure-time
   (let* ((buf (generate-new-buffer "*flow*")))
     (unwind-protect
         (let* ((result (apply 'call-process-region (point-min) (point-max) "flow" nil buf nil args))
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
    (when (process-live-p process)
      (with-demoted-errors "flowtype: error calling flow: %s"
        (process-send-region process (point-min) (point-max))
        (process-send-eof process)))))

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

(defun flowtype/suggest-into-buffer ()
  "Calls flow suggest and then runs ediff with the result."
  (interactive)
  (let* ((filename (buffer-file-name))
         (diff-buffer (flowtype//call-flow-into-buffer "suggest" filename)))
    (ediff-patch-file 2 diff-buffer)))

(defun flowtype//type-at-pos-async (result-handler pos)
  "Calls flow to get the type at pos asynchronously; passes the result to result-handler."
  (let* ((loc (flowtype//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'flowtype//json-flow-call-async result-handler "type-at-pos" filename loc)))

(defun flowtype//eldoc-show-type-info (data)
  "Shows the passed type info using eldoc."
  (let ((type (cdr (assq 'type data))))
    (when (not (equal "(unknown)" type))
      (eldoc-message type))))

(defun flowtype/eldoc-show-type-at-point ()
  "Shows type at point."
  (interactive)
  (flowtype//type-at-pos-async #'flowtype//eldoc-show-type-info (point))
  nil)


;; company provider

(defun flowtype//fetch-completions (&rest _)
  (interactive "P")
  (let* ((loc (flowtype//pos-to-flow-location (point)))
         (filename (buffer-file-name))
         (response (flowtype//json-flow-call "autocomplete" (car loc) (cadr loc)))
         (result (cdr (assoc 'result response)))
         (names (mapcar (lambda (res) (cdr (assoc 'name res))) result)))
    names))

(defun company-flowtype-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-flowtype-backend))
    (prefix (and (eq major-mode 'flowtype-mode)
                 (company-grab-symbol)))
    (candidates
     (progn
       (message "candidates %s" arg)
       (let* ((completes (flowtype//fetch-completions))
              (_ (message "names %s" completes))
              (list (remove-if-not
                    (lambda (c) (string-prefix-p arg c))
                    completes)))
         (message "list %s" list)
         list)))))

(add-to-list 'company-backends 'company-flowtype-backend)

;; flycheck

(defun flowtype//fc-convert-part (error-part checker counter)
  (message "part %s" error-part)
  (let* ((desc (cdr (assoc 'descr error-part)))
         (line (cdr (assoc 'line error-part)))
         (col  (cdr (assoc 'start error-part))))
    (flycheck-error-new-at line col 'error desc :checker checker :id counter)))

(defun flowtype//fc-convert-error (error checker counter)
  "Return a list of errors from ERROR."
  (let* ((msg-parts (cdr (assoc 'message error))))
    (mapcar (lambda (part)
              (flowtype//fc-convert-part part checker counter))
            msg-parts)))

(defun flowtype//parse-status-errors (output checker buffer)
  "Parse flow status errors in OUTPUT."
  (let* ((json (json-read-from-string output))
         (errors (cdr (assoc 'errors json)))
         (counter 0)
         (converted-errs (mapcar (lambda (err)
                                   (setq counter (1+ counter))
                                   (flowtype//fc-convert-error err checker (number-to-string counter)))
                                 errors))
         (errs (apply #'append converted-errs)))
    ;; (message "done: %s" errs)
    errs))

(with-eval-after-load 'flycheck
  (flycheck-define-command-checker 'javascript-flowtype
    "A JavaScript syntax and style checker using Flow."
    :command '("flow" "status" "--json")
    :error-parser #'flowtype//parse-status-errors
    :modes '(flowtype-mode))

  (add-to-list 'flycheck-checkers 'javascript-flowtype))


;; coverage overlays

(defun flowtype//clear-cov-overlays ()
  "Clear all flowtype overlays in current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'flowtype t))

(defun flowtype//make-overlay (tuple)
  "Make overlay for values in TUPLE."
  (let* ((linepos (point-at-bol (car tuple))))
    (make-overlay (- (+ linepos (nth 1 tuple)) 1) (+ linepos (nth 3 tuple)))))

(defun flowtype//overlay-put (ovl color)
  "Record actual overlay in OVL with COLOR."
  (overlay-put ovl 'face (cons 'background-color color))
  (overlay-put ovl 'flowtype t))

(defun flowtype//overlay-current-buffer-with-list (tuple-list)
  "Overlay current buffer acording to given TUPLE-LIST."
  (save-excursion
    (goto-char (point-min))
    (flowtype//clear-cov-overlays)
    (dolist (ovl (mapcar #'flowtype//make-overlay tuple-list))
      (flowtype//overlay-put ovl flowtype:uncovered-type-background-color))))

(defun flowtype//parse-raw-type (type)
  "Parse raw TYPE into tuple."
  (message "type: %s" type)
  (list (cdr (assoc 'line type))
        (cdr (assoc 'start type))
        (cdr (assoc 'endline type))
        (cdr (assoc 'end type))
        (cdr (assoc 'type type))))

(defun my-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun flowtype//untyped? (type)
  "True if type of TYPE is \"\" or any."
  (let* ((typename (cdr (assoc 'type type))))
    (message "type: %s" typename)
    (or (string= "any" typename)
        (string= "" typename))))

(defun flowtype//parse-raw-types (types)
  "Parse raw TYPES into tuples."
  (let* ((untyped (my-filter #'flowtype//untyped? types))
         (parsed (mapcar #'flowtype//parse-raw-type untyped)))
    (message "parsed: %s" parsed)
    parsed))

(defun flowtype//fetch-coverage (filename)
  "Fetch coverage data for FILENAME from flow."
  (let* ((data (flowtype//json-flow-call "dump-types" filename)))
    (flowtype//parse-raw-types data)))

(defun flowtype//file-load-callback ()
  "Initialize overlays in buffer after loading."
  (interactive)
  (let* ((filename (buffer-file-name))
         (buffer-coverage-data (flowtype//fetch-coverage filename)))
    (when buffer-coverage-data
      (message (format "flowtype: coverage for file: %s" filename))
      (flowtype//overlay-current-buffer-with-list buffer-coverage-data))))

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
