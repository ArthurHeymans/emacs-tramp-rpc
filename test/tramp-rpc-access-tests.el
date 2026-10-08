;;; tramp-rpc-access-tests.el --- Filesystem access checks -*- lexical-binding: t -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Loaded by tramp-rpc-mock-tests.el.  Mock tests check RPC dispatch and cache
;; policy; local-server tests compare accessibility with native Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defun tramp-rpc-access-test--decision (&optional errno)
  "Return an access decision with ERRNO, or success when omitted."
  (if (or (null errno) (zerop errno)) '((errno . 0))
    `((errno . ,errno) (message . "Access denied"))))

(ert-deftest tramp-rpc-mock-test-access-predicates-use-kernel ()
  "Predicates ask the kernel, never consulting cached permissions or groups."
  :tags '(:permissions)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((remote-file-name-inhibit-cache t)
        (tramp-cache-data (make-hash-table :test 'equal))
        (filename "/rpc:mockhost:/testdir")
        calls)
    (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
              ((symbol-function 'tramp-rpc-magit--file-exists-p)
               (lambda (_) (ert-fail "Full cache inhibition must bypass markers")))
              ((symbol-function 'tramp-rpc--call-file-stat)
               (lambda (&rest _) (ert-fail "Must not stat for permissions")))
              ((symbol-function 'tramp-get-remote-groups)
               (lambda (&rest _) (ert-fail "Must not simulate groups")))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vec method params &optional _connection)
                 (should (equal method "file.access"))
                 (push (list (tramp-rpc--binary-bytes (alist-get 'path params))
                             (alist-get 'mode params)) calls)
                 (tramp-rpc-access-test--decision))))
      (dolist (handler '(tramp-rpc-handle-file-readable-p
                         tramp-rpc-handle-file-writable-p
                         tramp-rpc-handle-file-executable-p
                         tramp-rpc-handle-file-accessible-directory-p))
        (should (eq t (funcall handler filename))))
      (should (equal (nreverse calls)
                     '(("/testdir" "r") ("/testdir" "w")
                       ("/testdir" "x") ("/testdir/./" "")))))))

(ert-deftest tramp-rpc-mock-test-access-errors-and-writable-parent ()
  "Denials are decisions; only ENOENT checks a writable parent."
  :tags '(:permissions)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((remote-file-name-inhibit-cache t)
        (tramp-cache-data (make-hash-table :test 'equal))
        (errno 2)
        (filename "/rpc:mockhost:/parent/missing")
        calls)
    (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
              ((symbol-function 'tramp-rpc-magit--file-exists-p)
               (lambda (_) 'not-cached))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vec _method params &optional _connection)
                 (let ((mode (alist-get 'mode params)))
                   (push (list (tramp-rpc--binary-bytes (alist-get 'path params))
                               mode) calls)
                   (tramp-rpc-access-test--decision
                    (unless (equal mode "wx") errno))))))
      (should (eq t (tramp-rpc-handle-file-writable-p filename)))
      (should (equal (nreverse calls)
                     '(("/parent/missing" "w") ("/parent/" "wx"))))
      (dolist (denial '(1 13 20)) ; EPERM, EACCES, ENOTDIR are not missing files.
        (setq errno denial calls nil)
        (should-not (tramp-rpc-handle-file-writable-p filename))
        (should (= 1 (length calls)))
        (should-not (tramp-rpc-handle-file-readable-p filename))))
    (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
              ((symbol-function 'tramp-rpc--call)
               (lambda (&rest _)
                 (signal 'remote-file-error '("Transport disconnected")))))
      (should-error (tramp-rpc-handle-file-executable-p filename)
                    :type 'remote-file-error))))

(ert-deftest tramp-rpc-mock-test-access-response-validation ()
  "Malformed decisions and RPC errors are never cached as permission denials."
  :tags '(:permissions :cache)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((tramp-cache-data (make-hash-table :test 'equal))
        (remote-file-name-inhibit-cache nil)
        (filename "/rpc:mockhost:/invalid-decision")
        (response t))
    (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
              ((symbol-function 'tramp-rpc--call) (lambda (&rest _) response)))
      (dolist (bad '(t nil ((errno . -1)) ((errno . "0")) ((errno . 13))
                    ((errno . 13) (message . t))))
        (setq response bad)
        (should-error (tramp-rpc-handle-file-executable-p filename)))
      (setq response (tramp-rpc-access-test--decision))
      (should (eq t (tramp-rpc-handle-file-executable-p filename))))))

(ert-deftest tramp-rpc-mock-test-access-unsupported-check-is-not-cached ()
  "Unresolved kernel checks propagate; only resolved decisions are cached."
  :tags '(:permissions :cache)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((tramp-cache-data (make-hash-table :test 'equal))
        (remote-file-name-inhibit-cache nil)
        (filename "/rpc:mockhost:/unsupported-check"))
    (dolist (handler '(tramp-rpc-handle-file-readable-p
                       tramp-rpc-handle-file-writable-p
                       tramp-rpc-handle-file-executable-p
                       tramp-rpc-handle-file-accessible-directory-p))
      (let ((calls 0))
        (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
                  ((symbol-function 'tramp-rpc-magit--file-exists-p)
                   (lambda (_) 'not-cached))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (&rest _)
                     (cl-incf calls)
                     (if (<= calls 2)
                         (signal 'file-error '("Access check unsupported"))
                       (tramp-rpc-access-test--decision)))))
          (dotimes (_ 2)
            (should-error (funcall handler filename) :type 'file-error))
          (should (eq t (funcall handler filename)))
          (should (eq t (funcall handler filename)))
          (should (= calls 3)))))))

(ert-deftest tramp-rpc-mock-test-access-predicate-cache-policy ()
  "Positive and negative decisions obey expiry, events and connection cleanup."
  :tags '(:permissions :cache)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((filename "/rpc:mockhost:/access-cache")
         (vec (tramp-dissect-file-name filename))
         (tramp-cache-data (make-hash-table :test 'equal))
         (remote-file-name-inhibit-cache 10)
         (allowed t)
         (calls 0))
    (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
              ((symbol-function 'tramp-rpc-magit--file-exists-p)
               (lambda (_) 'not-cached))
              ((symbol-function 'tramp-rpc--call)
               (lambda (&rest _)
                 (cl-incf calls)
                 (tramp-rpc-access-test--decision (unless allowed 13)))))
      (should (eq t (tramp-rpc-handle-file-readable-p filename)))
      (setq allowed nil)
      (should (eq t (tramp-rpc-handle-file-readable-p filename)))
      (should (= calls 1))
      (tramp-rpc--invalidate-event-path filename)
      (should-not (tramp-rpc-handle-file-readable-p filename))
      (setq allowed t)
      (should-not (tramp-rpc-handle-file-readable-p filename))
      (should (= calls 2))
      (tramp-rpc--clear-file-caches-for-connection vec)
      (should (eq t (tramp-rpc-handle-file-readable-p filename)))
      (let ((remote-file-name-inhibit-cache 0))
        (should (eq t (tramp-rpc-handle-file-readable-p filename))))
      (let ((remote-file-name-inhibit-cache (time-add nil 1)))
        (should (eq t (tramp-rpc-handle-file-readable-p filename))))
      (let ((remote-file-name-inhibit-cache t))
        (should (eq t (tramp-rpc-handle-file-readable-p filename))))
      (should (= calls 6)))))

(ert-deftest tramp-rpc-mock-test-access-cache-partitions-routes ()
  "Explicit and hidden hop routes never share access decisions."
  :tags '(:permissions :cache :multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (dolist (show-hops '(nil t))
    (let ((tramp-cache-data (make-hash-table :test 'equal))
          (tramp-show-ad-hoc-proxies show-hops)
          (remote-file-name-inhibit-cache nil)
          ;; Debugger printing settings must not truncate a route's identity.
          (print-level 2)
          (print-length 3)
          (calls 0))
      (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
                ((symbol-function 'tramp-rpc-magit--file-exists-p)
                 (lambda (_) 'not-cached))
                ((symbol-function 'tramp-rpc--call)
                 (lambda (vec _method _params &optional _connection)
                   (cl-incf calls)
                   (tramp-rpc-access-test--decision
                    (unless (cl-some
                             (lambda (hop) (equal (nth 3 hop) "gateway-a"))
                             (plist-get (tramp-rpc--connection-key vec) :route))
                      13)))))
        (cl-labels
            ((check (handler gateway allowed)
               ;; Hidden spellings refer to the currently configured proxy.
               ;; Keep both routes' file properties warm across that change.
               (let ((tramp-default-proxies-alist
                      (unless show-hops
                        `(("^target$" nil ,(format "/rpc:%s:" gateway)))))
                     (filename (if show-hops
                                   (format "/rpc:%s|rpc:target:/access-route"
                                           gateway)
                                 "/rpc:target:/access-route")))
                 (should (eq allowed (funcall handler filename))))))
          (dolist (handler '(tramp-rpc-handle-file-readable-p
                             tramp-rpc-handle-file-writable-p
                             tramp-rpc-handle-file-executable-p
                             tramp-rpc-handle-file-accessible-directory-p))
            (check handler "gateway-a" t)
            (check handler "gateway-b" nil)
            (check handler "gateway-a" t)
            (check handler "gateway-b" nil)))
        (should (= calls 8))))))

(ert-deftest tramp-rpc-mock-test-access-cache-rejects-invalid-routes ()
  "An invalid proxy route must not become a direct connection or cached answer."
  :tags '(:permissions :cache :multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((tramp-cache-data (make-hash-table :test 'equal))
        (tramp-default-proxies-alist '(("^target$" nil "/rpc:target:"))))
    (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
              ((symbol-function 'tramp-rpc--call)
               (lambda (&rest _) (ert-fail "Invalid route must not dispatch"))))
      (should-error
       (tramp-rpc-handle-file-executable-p "/rpc:target:/access-route")
       :type 'user-error))))

(ert-deftest tramp-rpc-mock-test-access-cache-preserves-trailing-slash ()
  "A regular file and its directory-only spelling have independent answers."
  :tags '(:permissions :cache)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((tramp-cache-data (make-hash-table :test 'equal))
        (remote-file-name-inhibit-cache nil)
        (filename "/rpc:mockhost:/regular")
        (calls 0))
    (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
              ((symbol-function 'tramp-rpc-magit--file-exists-p)
               (lambda (_) 'not-cached))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vec _method params &optional _connection)
                 (cl-incf calls)
                 (tramp-rpc-access-test--decision
                  (when (string-suffix-p
                         "/" (tramp-rpc--binary-bytes (alist-get 'path params)))
                    20)))))
      (dolist (handler '(tramp-rpc-handle-file-readable-p
                         tramp-rpc-handle-file-writable-p
                         tramp-rpc-handle-file-executable-p))
        (should (eq t (funcall handler filename)))
        (should-not (funcall handler (concat filename "/")))
        (should (eq t (funcall handler filename)))
        (should-not (funcall handler (concat filename "/"))))
      (should (= calls 6)))))

(ert-deftest tramp-rpc-mock-test-access-file-is-fresh ()
  "`access-file' bypasses cached decisions and reports native error classes."
  :tags '(:permissions :cache)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((filename "/rpc:mockhost:/access-fresh")
         (vec (tramp-dissect-file-name filename))
         (tramp-cache-data (make-hash-table :test 'equal))
         (remote-file-name-inhibit-cache nil)
         (errno 0)
         (calls 0))
    (tramp-set-file-property
     vec "/access-fresh" (tramp-rpc--file-property-key vec "/access-fresh" "r") nil)
    (tramp-set-file-property vec "/access-fresh" "file-exists-p" nil)
    (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vec method params &optional _connection)
                 (should (equal method "file.access"))
                 (should (equal (alist-get 'mode params) "r"))
                 (cl-incf calls)
                 (tramp-rpc-access-test--decision errno))))
      (should-not (tramp-rpc-handle-access-file filename "Cannot open"))
      (dolist (entry '((2 . file-missing) (13 . permission-denied)
                       (17 . file-already-exists) (1 . file-error)
                       (20 . file-error) (40 . file-error)))
        (setq errno (car entry))
        ;; Exercise TRAMP tracing rather than suppressing it to preserve data.
        (dolist (signal-hook-function '(nil tramp-signal-hook-function))
          (let ((err (should-error
                      (tramp-rpc-handle-access-file filename "Cannot open")
                      :type (cdr entry))))
            (should (eq (car err) (cdr entry)))
            (should (string-match-p "Cannot open" (error-message-string err)))
            (should (string-match-p filename (error-message-string err)))
            (should-not (string-match-p "errno" (error-message-string err))))))
      (should (= calls 13)))))

(ert-deftest tramp-rpc-mock-test-access-non-essential-and-timeout ()
  "Completion never connects; `access-file' keeps its timeout wrapper."
  :tags '(:permissions :non-essential)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((remote-file-name-inhibit-cache t)
        (non-essential t))
    (cl-letf (((symbol-function 'tramp-connectable-p) #'ignore)
              ((symbol-function 'tramp-rpc--call)
               (lambda (&rest _) (ert-fail "Completion must not connect"))))
      (should-not (tramp-rpc-handle-file-executable-p "/rpc:mockhost:/file"))
      (should (eq t (tramp-rpc-handle-file-accessible-directory-p
                     "/rpc:mockhost:/")))))
  (let ((remote-file-name-access-timeout 1))
    (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
              ((symbol-function 'tramp-rpc--call)
               (lambda (&rest _) (sleep-for 2) (tramp-rpc-access-test--decision))))
      (let ((err (should-error
                  (tramp-rpc-handle-access-file "/rpc:mockhost:/file" "Open")
                  :type 'file-error)))
        (should (string-match-p "Timeout" (error-message-string err)))))))

(ert-deftest tramp-rpc-mock-test-access-file-transport-timeout ()
  "The access timeout interrupts a real RPC wait and releases its pending ID."
  :tags '(:permissions :transport)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((remote-file-name-access-timeout 1))
      (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_) t))
                ((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_) connection)))
        ;; This live pipe accepts the request frame but never supplies a reply.
        (let ((err (should-error
                    (tramp-rpc-handle-access-file
                     "/rpc:request-test:/tmp/file" "Open")
                    :type 'file-error)))
          (should (string-match-p "Open: Timeout" (error-message-string err))))
        (should (process-live-p process))
        (should-not (tramp-rpc-connection-pending-ids connection))
        (should (zerop (hash-table-count
                        (tramp-rpc-connection-pending-responses connection))))))))

(defun tramp-rpc-access-test--local-call (vec method params &optional _connection)
  "Send METHOD/PARAMS for VEC to the local test server, retaining RPC errors."
  (let ((result (tramp-rpc-mock-test--rpc-call method params)))
    (if (and (listp result) (plist-get result :error))
        (tramp-rpc--signal-rpc-error
         "RPC" (plist-get result :error) (plist-get result :code)
         (alist-get 'os_errno (plist-get result :data))
         (tramp-make-tramp-file-name vec
                                    (tramp-rpc--binary-bytes
                                     (alist-get 'path params)))
         (plist-get result :data))
      result)))

(defun tramp-rpc-access-test--compare (path)
  "Compare native predicates and `access-file' against RPC for local PATH."
  (let ((remote (concat "/rpc:mockhost:" path)))
    (dolist (predicate '(file-readable-p file-writable-p file-executable-p
                        file-accessible-directory-p))
      (let ((native (funcall predicate path))
            (rpc (funcall predicate remote)))
        (should (memq rpc '(nil t)))
        (should (eq native rpc))))
    (let ((native-error (condition-case err (access-file path "Native")
                          (file-error (car err))))
          (rpc-error (condition-case err (access-file remote "RPC")
                       (file-error (car err)))))
      (should (eq native-error rpc-error)))))

(ert-deftest tramp-rpc-mock-test-server-access-matches-native-emacs ()
  "Kernel-backed access matches native Emacs, including links and directories."
  :tags '(:server :permissions)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let* ((root tramp-rpc-mock-test-temp-dir)
               (file (expand-file-name "file" root))
               (dir (expand-file-name "directory" root))
               (child (expand-file-name "child" dir))
               (tramp-cache-data (make-hash-table :test 'equal))
               (remote-file-name-inhibit-cache t))
          (write-region "contents" nil file nil 'silent)
          (make-directory dir)
          (write-region "contents" nil child nil 'silent)
          (set-file-modes child #o777)
          (cl-letf (((symbol-function 'tramp-rpc--call)
                     #'tramp-rpc-access-test--local-call)
                    ((symbol-function 'tramp-connectable-p) (lambda (_) t))
                    ((symbol-function 'tramp-rpc-magit--file-exists-p)
                     (lambda (_) 'not-cached)))
            (unwind-protect
                (progn
                  (dolist (mode '(#o000 #o004 #o100 #o200 #o400 #o700))
                    (set-file-modes file mode)
                    (tramp-rpc-access-test--compare file)
                    (tramp-rpc-access-test--compare (concat file "/")))
                  (dolist (mode '(#o000 #o004 #o111 #o200 #o444 #o700))
                    (set-file-modes dir mode)
                    (tramp-rpc-access-test--compare dir)
                    (tramp-rpc-access-test--compare child)
                    (tramp-rpc-access-test--compare
                     (expand-file-name "missing" dir)))
                  (set-file-modes dir #o700)
                  (set-file-modes file #o600)
                  (dolist (entry '(("link" . "file") ("dangling" . "missing")
                                   ("cycle" . "cycle")))
                    (let ((path (expand-file-name (car entry) root)))
                      (make-symbolic-link (cdr entry) path)
                      (tramp-rpc-access-test--compare path)))
                  (tramp-rpc-access-test--compare
                   (concat (file-name-as-directory file) "child"))
                  (let* ((remote (concat "/rpc:mockhost:" file))
                         (vec (tramp-dissect-file-name remote)))
                    ;; Warm stale metadata with no filesystem watch installed.
                    (set-file-modes file #o000)
                    (let ((remote-file-name-inhibit-cache nil))
                      (tramp-rpc--call-file-stat vec file))
                    (set-file-modes file #o600)
                    (should (eq t (file-readable-p remote)))
                    (unless (zerop (user-uid))
                      (let ((remote-file-name-inhibit-cache 10))
                        (should (eq t (file-readable-p remote)))
                        (set-file-modes file #o000)
                        (should (eq t (file-readable-p remote)))
                        (should-error (access-file remote "Fresh check")
                                      :type 'permission-denied)
                        (let ((remote-file-name-inhibit-cache 0))
                          (should-not (file-readable-p remote)))))
                    (tramp-flush-file-properties vec file)))
              (set-file-modes dir #o700)))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-access-posix-acl ()
  "Non-owner ACL grants, masks and traversal match native Emacs and real I/O."
  :tags '(:server :permissions :acl)
  ;; The runner supplies subordinate IDs and removes both DAC capabilities.
  (skip-unless (getenv "TRAMP_RPC_ACCESS_TEST_ACL"))
  (should tramp-rpc-mock-test--tramp-rpc-loaded)
  (should (tramp-rpc-mock-test--find-server))
  (should (executable-find "setfacl"))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let* ((root tramp-rpc-mock-test-temp-dir)
               (file (expand-file-name "acl-file" root))
               (dir (expand-file-name "acl-directory" root))
               (child (expand-file-name "child" dir))
               (missing (expand-file-name "missing" dir))
               (remote (concat "/rpc:mockhost:" file))
               (vec (tramp-dissect-file-name remote))
               (uid (user-uid))
               (gid (group-gid))
               (tramp-cache-data (make-hash-table :test 'equal))
               (remote-file-name-inhibit-cache t))
          (write-region "ACL contents" nil file nil 'silent)
          (make-directory dir)
          (write-region "child contents" nil child nil 'silent)
          (set-file-modes file #o000)
          (should (= 0 (call-process "chown" nil nil nil "1:1" file dir)))
          (should-not (= uid (file-attribute-user-id (file-attributes file))))
          (should-not (= gid (file-attribute-group-id (file-attributes file))))
          (cl-labels
              ((acl (path entry mask)
                 (should
                  (= 0 (call-process
                        "setfacl" nil nil nil "--no-mask" "--set"
                        (format "u::---,%s,g::---,m::%s,o::---" entry mask)
                        path))))
               (read-file ()
                 (tramp-rpc-access-test--local-call
                  vec "file.read" (tramp-rpc--encode-path file))))
            (cl-letf (((symbol-function 'tramp-rpc--call)
                       #'tramp-rpc-access-test--local-call)
                      ((symbol-function 'tramp-connectable-p) (lambda (_) t))
                      ((symbol-function 'tramp-rpc-magit--file-exists-p)
                       (lambda (_) 'not-cached)))
              (unwind-protect
                  (progn
                    (dolist (entry (list (format "u:%d:rwx" uid)
                                        (format "g:%d:rwx" gid)))
                      (dolist (case '(("---" nil nil nil) ("rwx" t t t)
                                      ("r--" t nil nil) ("-wx" nil t t)))
                        (acl file entry (car case))
                        (tramp-rpc-access-test--compare file)
                        (cl-mapc
                         (lambda (predicate allowed)
                           (should (eq allowed (funcall predicate remote))))
                         '(file-readable-p file-writable-p file-executable-p)
                         (cdr case))
                        (if (cadr case)
                            (should (equal (tramp-rpc--binary-bytes
                                            (alist-get 'content (read-file)))
                                           "ACL contents"))
                          (should-error (read-file) :type 'permission-denied))))
                    ;; Search without directory read permission is sufficient;
                    ;; creating a missing child additionally requires write.
                    (dolist (case '(("---" nil nil) ("--x" t nil) ("-wx" t t)))
                      (acl dir (format "u:%d:rwx" uid) (car case))
                      (dolist (path (list dir child missing))
                        (tramp-rpc-access-test--compare path))
                      (should (eq (cadr case)
                                  (file-accessible-directory-p
                                   (concat "/rpc:mockhost:" dir))))
                      (should (eq (caddr case)
                                  (file-writable-p
                                   (concat "/rpc:mockhost:" missing)))))
                    ;; Check both cached directions against actual ACL changes.
                    (acl file (format "u:%d:rwx" uid) "r--")
                    (tramp-flush-file-properties vec file)
                    (let ((remote-file-name-inhibit-cache nil))
                      (should (eq t (file-readable-p remote)))
                      (acl file (format "u:%d:rwx" uid) "---")
                      (should (eq t (file-readable-p remote)))
                      (should-error (access-file remote "Fresh ACL check")
                                    :type 'permission-denied)
                      (should-error (read-file) :type 'permission-denied)
                      ;; Consume the same invalidation used for watch events.
                      (tramp-rpc--invalidate-event-path remote)
                      (should-not (file-readable-p remote))
                      (acl file (format "u:%d:rwx" uid) "r--")
                      (should-not (file-readable-p remote))
                      (tramp-rpc--invalidate-event-path remote)
                      (should (eq t (file-readable-p remote)))))
                ;; Restore search access even if an assertion failed while denied.
                (acl dir (format "u:%d:rwx" uid) "rwx"))))))
    (tramp-rpc-mock-test--stop-server)))

(provide 'tramp-rpc-access-tests)
;;; tramp-rpc-access-tests.el ends here
