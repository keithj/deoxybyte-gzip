;;;
;;; Copyright (c) 2009-2013 Keith James. All rights reserved.
;;;
;;; This file is part of deoxybyte-gzip.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(in-package :cl-user)

(asdf:load-system :deoxybyte-systems)

(in-package :uk.co.deoxybyte-systems)

(defsystem deoxybyte-gzip
    :name "deoxybyte-gzip"
    :author "Keith James"
    :version "0.6.1"
    :licence "GPL v3"
    :in-order-to ((test-op (load-op :deoxybyte-gzip :deoxybyte-gzip-test))
                  (doc-op (load-op :deoxybyte-gzip :cldoc)))
    :depends-on ((:version :deoxybyte-systems "1.0.0")
                 (:version :deoxybyte-io "0.15.0")
                 (:version :deoxybyte-unix "0.8.0"))
    :components ((:module :deoxybyte-gzip
                          :serial t
                          :pathname "src/"
                          :components ((:file "package")
                                       (:file "zlib-ffi")
                                       (:file "rfc1952")
                                       (:file "conditions")
                                       (:file "deoxybyte-gzip")
                                       (:file "gzip-stream")
                                       (:file "line-stream")
                                       #+:sbcl (:file "sbcl")
                                       #+:ccl (:file "ccl"))))
    :perform (test-op :after (op c)
                      (maybe-run-lift-tests :deoxybyte-gzip
                                            "deoxybyte-gzip-test.config"))
    :perform (doc-op :after (op c)
                     (maybe-build-cldoc-docs :deoxybyte-gzip "doc/html")))
