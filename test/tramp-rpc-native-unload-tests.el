;;; tramp-rpc-native-unload-tests.el --- Native-comp unload tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Assisted-by: various LLMs

;; This file is part of tramp-rpc.

;;; Commentary:

;; Regression coverage for unloading native-compiled TRAMP-RPC code.

;;; Code:

(require 'ert)
(require 'comp)

(defconst tramp-rpc-native-unload-test--project-root
  (expand-file-name "../" (file-name-directory
                           (or load-file-name buffer-file-name)))
  "Project root directory.")

(ert-deftest tramp-rpc-native-unload-test-unload-tramp ()
  "Native-compiled TRAMP-RPC unloads with TRAMP despite Emacs bug#80446."
  (should (native-comp-available-p))
  (let* ((directory (make-temp-file "tramp-rpc-native-unload" t))
         (source (expand-file-name "lisp/tramp-rpc.el"
                                   tramp-rpc-native-unload-test--project-root))
         (output (expand-file-name "tramp-rpc-native-test.eln" directory)))
    (unwind-protect
        (progn
          (native-compile source output)
          (load output nil nil t)
          (should (featurep 'tramp-rpc))
          ;; Confirm the native object owns a load-history entry.  Emacs
          ;; versions where bug#80446 is still present add malformed anonymous
          ;; defuns there; versions with an upstream fix may no longer do so.
          (should (assoc output load-history))
          ;; This follows upstream `tramp-test52-unload': unloading TRAMP runs
          ;; `tramp-unload-hook', which in turn unloads TRAMP-RPC.
          (unload-feature 'tramp 'force)
          (should-not (featurep 'tramp-rpc))
          (should-not (featurep 'tramp)))
      (when (featurep 'tramp-rpc)
        (unload-feature 'tramp-rpc 'force))
      (delete-directory directory t))))

(provide 'tramp-rpc-native-unload-tests)
;;; tramp-rpc-native-unload-tests.el ends here
