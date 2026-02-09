;;; tramp-rpc-magit.el --- Magit/Projectile integration for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: comm, processes, vc
;; Package-Requires: ((emacs "30.1") (msgpack "0"))

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides magit and projectile integration for tramp-rpc.
;; Currently a stub - magit prefetch and projectile optimizations will
;; be added in a future branch.

;;; Code:

(require 'cl-lib)
(require 'tramp)

(provide 'tramp-rpc-magit)
;;; tramp-rpc-magit.el ends here
