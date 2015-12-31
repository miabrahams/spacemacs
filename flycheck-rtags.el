
;;; Code:

;; (require 'rtags "rtags.el")


(defun rtd--file (diagnostic)
  (nth 0 diagnostic))

(defun rtd--line (diagnostic)
  (nth 1 diagnostic))

(defun rtd--column (diagnostic)
  (nth 2 diagnostic))

(defun rtd--severity (diagnostic)
  (nth 4 diagnostic))

(defun rtd--message (diagnostic)
  (nth 5 diagnostic))

(defun rtd--request-handler (diagnostics callback buffer)
  (with-current-buffer buffer
    (cond
     ((rtags--buffer-parsed-p)
      (funcall callback 'success diagnostics))
     (t
      ;; buffer has become out-of-date
      (funcall callback 'cancelled "diagnostics obsolete, buffer has changed")))))



(defun rtd--async-example (callback &optional force)
  "Perform an asynchronous diagnostic request for the current
buffer.

Use FORCE to force the reparsing of the buffer.

CALLBACK is called with at least one argument, a symbol
representing the status of the request. Depending on the status
more argument are provided. Possible values are explained below:

- success

  When quering the diagnostics work, the additional argument is a
  list of diagnostic object, diagnostics fields can be queried
  with the functions `rtd--<xxx>'.

- error

  Retrieving the diagnostics wasn't possible. A string explaining
  the reason is passed as a second argument.

- cancelled

  Retrieving the diagnostics was cancelled, e.g: because the
  buffer has changed since the beginning of the request, and as
  such the diagnostics are considered no longer relevant. A
  reason string is passed as a second argument."
  (lexical-let ((cb callback))
    (rtags--parse-buffer-async
     #'(lambda (parse-status)
         (cond
          ((eq parse-status 'success)
           (rtags--send-request "diagnostics"
                                (list 'rtd--request-handler
                                      cb
                                      (current-buffer))))
          ((eq parse-status 'cancelled)
           (funcall cb 'cancelled "parsing was cancelled"))
          ((eq parse-status 'failed)
           (funcall cb 'error "parsing failed"))
          (t
           (funcall cb 'error "internal-error: unexpected parse status"))))
     force)))


(defun rtd--async (&optional restart nodirty)
  (interactive "P")
  (when restart
    (rtags-stop-diagnostics))
  (let ((buf (rtags-get-buffer-create-no-undo rtags-diagnostics-buffer-name)))
    (when (cond ((not rtags-diagnostics-process) t)
                ((eq (process-status rtags-diagnostics-process) 'exit) t)
                ((eq (process-status rtags-diagnostics-process) 'signal) t)
                (t nil))
      (with-current-buffer buf
        (rtags-diagnostics-mode))
      (unless nodirty
        (rtags-reparse-file))
      (let ((process-connection-type (not rtags-diagnostics-use-pipe))) ;; use a pipe if rtags-diagnostics-use-pipe is t
        (let ((rawbuf (get-buffer rtags-diagnostics-raw-buffer-name)))
          (when rawbuf
            (kill-buffer rawbuf)))
        (setq rtags-diagnostics-process (start-process "RTags Diagnostics" buf (rtags-executable-find "rc") "-m" "--elisp-list"))
        (set-process-filter rtags-diagnostics-process (function rtags-diagnostics-process-filter))
        (set-process-sentinel rtags-diagnostics-process 'rtags-diagnostics-sentinel)
        (setq rtags-last-completions nil)
        (setq rtags-last-completion-position nil)
        (rtags-clear-diagnostics)))))

;; Modified functions from irony-diagnostics above

;; Todo
;; replace rtd--async
;; Implement rtags--buffer-parsed-p
;; Double check rtags-enabled (what is definition of irony-mode as a variable?)
;; look into flycheck's :predicate... also rtags-diagnostics-process
;;

(require 'flycheck)

(eval-when-compile
  (require 'pcase))


(defun flycheck-rtags--build-error (checker buffer diagnostic)
  (let ((severity (rtd--severity diagnostic)))
    (if (memq severity '(note warning error fatal))
        (flycheck-error-new-at
         (rtd--line diagnostic)
         (rtd--column diagnostic)
         (pcase severity
           (`note 'info)
           (`warning 'warning)
           ((or `error `fatal) 'error))
         (rtd--message diagnostic)
         :checker checker
         :buffer buffer
         :filename (rtd--file diagnostic)))))

(defun flycheck-rtags--start (checker callback)
  (let ((buffer (current-buffer)))
    (rtd--async
     #'(lambda (status &rest args) ;; closure, lexically bound
         (pcase status
           (`error (funcall callback 'errored (car args)))
           (`cancelled (funcall callback 'finished nil))
           (`success
            (let* ((diagnostics (car args))
                   (errors (mapcar #'(lambda (diagnostic)
                                       (flycheck-rtags--build-error checker buffer diagnostic))
                                   diagnostics)))
              (funcall callback 'finished (delq nil errors)))))))))

(defun flycheck-rtags--verify (checker)
  "Verify the Flycheck Rtags syntax checker."
  (list
   (flycheck-verification-result-new
    :label "Rtags Mode"
    :message (if rtags-enabled "enabled" "disabled")
    :face (if rtags-enabled 'success '(bold warning)))

   ;; FIXME: the logic of `rtags--locate-server-executable' could be extracted
   ;; into something very useful for this verification
   (let* ((client-path (rtags-executable-find "rc"))
          (client-found (file-exists-p client-path)))
     (flycheck-verification-result-new
      :label "rtags-client"
      :message (if client-found (format "Found at %s" client-path) "Not found")
      :face (if client-found 'success '(bold error))))))

(flycheck-define-generic-checker 'rtags
  "A syntax checker for C, C++ and Objective-C, using RTags Mode."
  :start #'flycheck-rtags--start
  :verify #'flycheck-rtags--verify
  :modes '(c-mode c++-mode objc-mode)
  :error-filter #'identity
  ;; :predicate #'(lambda () rtags-mode)
  )

;;;###autoload
(defun flycheck-rtags-setup ()
  "Setup Flycheck Rtags.
Add `rtags' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'rtags))

(provide 'flycheck-rtags)
