;;; test-resize.el --- Test PTY resize -*- lexical-binding: t; -*-

(declare-function tramp-rpc--call "tramp-rpc")

(defun test-resize (&optional cols rows)
  (interactive)
  (let* ((proc (get-buffer-process "*vterm*"))
         (vec (process-get proc :tramp-rpc-vec))
         (pid (process-get proc :tramp-rpc-pid))
         (cols (or cols 80))
         (rows (or rows 24)))
    (message "Testing resize: pid=%s cols=%s rows=%s" pid cols rows)
    (let ((result (tramp-rpc--call vec "process.resize_pty"
                                   `((pid . ,pid)
                                     (cols . ,cols)
                                     (rows . ,rows)))))
      (message "Resize result: %s" result)
      result)))

(defun test-resize-120x40 ()
  (interactive)
  (test-resize 120 40))

(defun test-check-size ()
  (interactive)
  (let ((proc (get-buffer-process "*vterm*")))
    (process-send-string proc "stty size > /tmp/stty-size.txt && cat /tmp/stty-size.txt\n")))

(provide 'test-resize)
;;; test-resize.el ends here
