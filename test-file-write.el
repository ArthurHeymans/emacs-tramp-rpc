;;; test-file-write.el --- Test TRAMP RPC file write operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;;; Commentary:

;; Test file for verifying TRAMP RPC file write operations.
;; Run with: emacs -Q -l test-file-write.el -f tramp-rpc-test-file-write-run
;;
;; Or interactively:
;;   M-x tramp-rpc-test-file-write-run

;;; Code:

(require 'tramp)

;; Load tramp-rpc
(let ((lisp-dir (expand-file-name "lisp" (file-name-directory load-file-name))))
  (add-to-list 'load-path lisp-dir)
  (require 'tramp-rpc nil t))

;;; Configuration

(defvar tramp-rpc-test-host "x220-nixos"
  "Host to use for testing.")

(defvar tramp-rpc-test-user nil
  "User for remote host. If nil, uses default.")

(defvar tramp-rpc-test-dir "/tmp/tramp-rpc-test"
  "Remote directory to use for tests.")

;;; Test utilities

(defvar tramp-rpc-test--passed 0 "Count of passed tests.")
(defvar tramp-rpc-test--failed 0 "Count of failed tests.")
(defvar tramp-rpc-test--errors nil "List of error messages.")

(defun tramp-rpc-test--make-path (file)
  "Make a TRAMP RPC path for FILE."
  (let ((user-part (if tramp-rpc-test-user
                       (concat tramp-rpc-test-user "@")
                     "")))
    (format "/rpc:%s%s:%s/%s" user-part tramp-rpc-test-host tramp-rpc-test-dir file)))

(defun tramp-rpc-test--assert (name condition &optional message)
  "Assert that CONDITION is true for test NAME."
  (if condition
      (progn
        (message "  PASS: %s" name)
        (cl-incf tramp-rpc-test--passed))
    (message "  FAIL: %s%s" name (if message (format " - %s" message) ""))
    (push (format "%s%s" name (if message (format ": %s" message) ""))
          tramp-rpc-test--errors)
    (cl-incf tramp-rpc-test--failed)))

(defun tramp-rpc-test--assert-equal (name expected actual)
  "Assert that EXPECTED equals ACTUAL for test NAME."
  (if (equal expected actual)
      (progn
        (message "  PASS: %s" name)
        (cl-incf tramp-rpc-test--passed))
    (message "  FAIL: %s - expected %S, got %S" name expected actual)
    (push (format "%s: expected %S, got %S" name expected actual)
          tramp-rpc-test--errors)
    (cl-incf tramp-rpc-test--failed)))

(defun tramp-rpc-test--setup ()
  "Set up test environment."
  (message "Setting up test directory...")
  (let ((dir (format "/rpc:%s%s:%s"
                     (if tramp-rpc-test-user
                         (concat tramp-rpc-test-user "@")
                       "")
                     tramp-rpc-test-host
                     tramp-rpc-test-dir)))
    (ignore-errors (delete-directory dir t))
    (make-directory dir t)))

(defun tramp-rpc-test--teardown ()
  "Clean up test environment."
  (message "Cleaning up test directory...")
  (let ((dir (format "/rpc:%s%s:%s"
                     (if tramp-rpc-test-user
                         (concat tramp-rpc-test-user "@")
                       "")
                     tramp-rpc-test-host
                     tramp-rpc-test-dir)))
    (ignore-errors (delete-directory dir t))
    (tramp-cleanup-all-connections)))

;;; File write tests

(defun tramp-rpc-test--write-simple ()
  "Test writing a simple text file."
  (message "\n=== Test: Simple file write ===")
  (let* ((file (tramp-rpc-test--make-path "simple.txt"))
         (content "Hello, World!"))
    ;; Write the file
    (condition-case err
        (with-temp-buffer
          (insert content)
          (write-region (point-min) (point-max) file))
      (error
       (tramp-rpc-test--assert "write-simple" nil (format "Write error: %S" err))
       (cl-return-from tramp-rpc-test--write-simple)))
    
    ;; Verify file exists
    (tramp-rpc-test--assert "write-simple-exists" (file-exists-p file))
    
    ;; Read back and verify content
    (let ((read-content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
      (tramp-rpc-test--assert-equal "write-simple-content" content read-content))))

(defun tramp-rpc-test--write-multiline ()
  "Test writing a multiline text file."
  (message "\n=== Test: Multiline file write ===")
  (let* ((file (tramp-rpc-test--make-path "multiline.txt"))
         (content "Line 1\nLine 2\nLine 3\n"))
    ;; Write the file
    (condition-case err
        (with-temp-buffer
          (insert content)
          (write-region (point-min) (point-max) file))
      (error
       (tramp-rpc-test--assert "write-multiline" nil (format "Write error: %S" err))
       (cl-return-from tramp-rpc-test--write-multiline)))
    
    ;; Read back and verify content
    (let ((read-content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
      (tramp-rpc-test--assert-equal "write-multiline-content" content read-content))))

(defun tramp-rpc-test--write-binary ()
  "Test writing binary content (bytes 0-255)."
  (message "\n=== Test: Binary file write ===")
  (let* ((file (tramp-rpc-test--make-path "binary.bin"))
         ;; Create all bytes 0-255
         (content (apply #'unibyte-string (number-sequence 0 255))))
    ;; Write the file
    (condition-case err
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert content)
          (write-region (point-min) (point-max) file))
      (error
       (tramp-rpc-test--assert "write-binary" nil (format "Write error: %S" err))
       (cl-return-from tramp-rpc-test--write-binary)))
    
    ;; Read back and verify content
    (let ((read-content (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents file)
                          (buffer-string))))
      (tramp-rpc-test--assert-equal "write-binary-length" 256 (length read-content))
      (tramp-rpc-test--assert-equal "write-binary-content" content read-content))))

(defun tramp-rpc-test--write-utf8 ()
  "Test writing UTF-8 content with non-ASCII characters."
  (message "\n=== Test: UTF-8 file write ===")
  (let* ((file (tramp-rpc-test--make-path "utf8.txt"))
         (content "Hello, World!\nBonjour le monde!\nHallo Welt!\nPrzyklad UTF-8: zolw"))
    ;; Write the file
    (condition-case err
        (with-temp-buffer
          (insert content)
          (write-region (point-min) (point-max) file))
      (error
       (tramp-rpc-test--assert "write-utf8" nil (format "Write error: %S" err))
       (cl-return-from tramp-rpc-test--write-utf8)))
    
    ;; Read back and verify content
    (let ((read-content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
      (tramp-rpc-test--assert-equal "write-utf8-content" content read-content))))

(defun tramp-rpc-test--write-large ()
  "Test writing a larger file (100KB)."
  (message "\n=== Test: Large file write (100KB) ===")
  (let* ((file (tramp-rpc-test--make-path "large.txt"))
         (content (make-string 102400 ?x)))
    ;; Write the file
    (condition-case err
        (with-temp-buffer
          (insert content)
          (write-region (point-min) (point-max) file))
      (error
       (tramp-rpc-test--assert "write-large" nil (format "Write error: %S" err))
       (cl-return-from tramp-rpc-test--write-large)))
    
    ;; Verify file size
    (let ((attrs (file-attributes file)))
      (tramp-rpc-test--assert-equal "write-large-size" 102400 (file-attribute-size attrs)))
    
    ;; Read back and verify content
    (let ((read-content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
      (tramp-rpc-test--assert-equal "write-large-content" content read-content))))

(defun tramp-rpc-test--write-overwrite ()
  "Test overwriting an existing file."
  (message "\n=== Test: Overwrite existing file ===")
  (let* ((file (tramp-rpc-test--make-path "overwrite.txt"))
         (content1 "Original content")
         (content2 "New content"))
    ;; Write initial file
    (with-temp-buffer
      (insert content1)
      (write-region (point-min) (point-max) file))
    
    ;; Verify initial content
    (let ((read-content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
      (tramp-rpc-test--assert-equal "write-overwrite-initial" content1 read-content))
    
    ;; Overwrite with new content
    (with-temp-buffer
      (insert content2)
      (write-region (point-min) (point-max) file))
    
    ;; Verify new content
    (let ((read-content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
      (tramp-rpc-test--assert-equal "write-overwrite-final" content2 read-content))))

(defun tramp-rpc-test--write-append ()
  "Test appending to an existing file."
  (message "\n=== Test: Append to file ===")
  (let* ((file (tramp-rpc-test--make-path "append.txt"))
         (content1 "First part\n")
         (content2 "Second part\n")
         (expected (concat content1 content2)))
    ;; Write initial file
    (with-temp-buffer
      (insert content1)
      (write-region (point-min) (point-max) file))
    
    ;; Append second part
    (with-temp-buffer
      (insert content2)
      (write-region (point-min) (point-max) file t))
    
    ;; Verify combined content
    (let ((read-content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
      (tramp-rpc-test--assert-equal "write-append-content" expected read-content))))

(defun tramp-rpc-test--write-string ()
  "Test writing using write-region with a string argument."
  (message "\n=== Test: Write-region with string ===")
  (let* ((file (tramp-rpc-test--make-path "string.txt"))
         (content "String content test"))
    ;; Write using string as first argument
    (condition-case err
        (write-region content nil file)
      (error
       (tramp-rpc-test--assert "write-string" nil (format "Write error: %S" err))
       (cl-return-from tramp-rpc-test--write-string)))
    
    ;; Read back and verify
    (let ((read-content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
      (tramp-rpc-test--assert-equal "write-string-content" content read-content))))

(defun tramp-rpc-test--write-empty ()
  "Test writing an empty file."
  (message "\n=== Test: Empty file write ===")
  (let* ((file (tramp-rpc-test--make-path "empty.txt"))
         (content ""))
    ;; Write empty file
    (condition-case err
        (write-region content nil file)
      (error
       (tramp-rpc-test--assert "write-empty" nil (format "Write error: %S" err))
       (cl-return-from tramp-rpc-test--write-empty)))
    
    ;; Verify file exists
    (tramp-rpc-test--assert "write-empty-exists" (file-exists-p file))
    
    ;; Verify file is empty
    (let ((attrs (file-attributes file)))
      (tramp-rpc-test--assert-equal "write-empty-size" 0 (file-attribute-size attrs)))))

(defun tramp-rpc-test--write-subdirectory ()
  "Test writing to a file in a subdirectory."
  (message "\n=== Test: Write to subdirectory ===")
  (let* ((subdir (tramp-rpc-test--make-path "subdir"))
         (file (concat subdir "/nested.txt"))
         (content "Nested file content"))
    ;; Create subdirectory
    (make-directory subdir t)
    
    ;; Write file in subdirectory
    (condition-case err
        (write-region content nil file)
      (error
       (tramp-rpc-test--assert "write-subdirectory" nil (format "Write error: %S" err))
       (cl-return-from tramp-rpc-test--write-subdirectory)))
    
    ;; Read back and verify
    (let ((read-content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
      (tramp-rpc-test--assert-equal "write-subdirectory-content" content read-content))))

(defun tramp-rpc-test--write-nil-region ()
  "Test writing with nil START and END (like save-buffer does)."
  (message "\n=== Test: Write with nil START/END (save-buffer style) ===")
  (let* ((file (tramp-rpc-test--make-path "nil-region.txt"))
         (content "Content written with nil region args"))
    ;; Write using nil for START and END - this is how save-buffer calls write-region
    (condition-case err
        (with-temp-buffer
          (insert content)
          (write-region nil nil file))
      (error
       (tramp-rpc-test--assert "write-nil-region" nil (format "Write error: %S" err))
       (cl-return-from tramp-rpc-test--write-nil-region)))
    
    ;; Verify file exists
    (tramp-rpc-test--assert "write-nil-region-exists" (file-exists-p file))
    
    ;; Read back and verify content
    (let ((read-content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
      (tramp-rpc-test--assert-equal "write-nil-region-content" content read-content))))

;;; Main test runner

;;;###autoload
(defun tramp-rpc-test-file-write-run ()
  "Run all file write tests."
  (interactive)
  (setq tramp-rpc-test--passed 0
        tramp-rpc-test--failed 0
        tramp-rpc-test--errors nil)
  
  (message "\n")
  (message "====================================")
  (message "TRAMP RPC File Write Tests")
  (message "====================================")
  (message "Host: %s" tramp-rpc-test-host)
  (message "====================================\n")
  
  (condition-case err
      (progn
        (tramp-rpc-test--setup)
        
        ;; Run all tests
        (tramp-rpc-test--write-simple)
        (tramp-rpc-test--write-multiline)
        (tramp-rpc-test--write-binary)
        (tramp-rpc-test--write-utf8)
        (tramp-rpc-test--write-large)
        (tramp-rpc-test--write-overwrite)
        (tramp-rpc-test--write-append)
        (tramp-rpc-test--write-string)
        (tramp-rpc-test--write-empty)
        (tramp-rpc-test--write-subdirectory)
        (tramp-rpc-test--write-nil-region)
        
        (tramp-rpc-test--teardown))
    (error
     (message "\nFATAL ERROR: %S" err)
     (tramp-rpc-test--teardown)))
  
  ;; Print summary
  (message "\n====================================")
  (message "Test Summary")
  (message "====================================")
  (message "Passed: %d" tramp-rpc-test--passed)
  (message "Failed: %d" tramp-rpc-test--failed)
  (when tramp-rpc-test--errors
    (message "\nFailures:")
    (dolist (err (nreverse tramp-rpc-test--errors))
      (message "  - %s" err)))
  (message "====================================\n")
  
  ;; Return success/failure
  (zerop tramp-rpc-test--failed))

(provide 'test-file-write)
;;; test-file-write.el ends here
