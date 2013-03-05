;;;
;;; Copyright (c) 2009-2012 Keith James. All rights reserved.
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
    :version "0.5.1"
    :licence "GPL v3"
    :in-order-to ((test-op (load-op :deoxybyte-gzip :deoxybyte-gzip-test)))
    :depends-on ((:version :deoxybyte-io "0.9.0")
                 (:version :deoxybyte-unix "0.7.2"))
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
                                       #+:ccl (:file "ccl")))
                 (:lift-test-config :lift-tests
                                    :pathname "deoxybyte-gzip-test"
                                    :target-system :deoxybyte-gzip)
                 (:cldoc-config :cldoc-documentation
                                :pathname "doc/html/"
                                :target-system :deoxybyte-gzip)))
