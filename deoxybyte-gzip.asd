;;;
;;; Copyright (C) 2009 Keith James. All rights reserved.
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (asdf:find-system :deoxybyte-systems nil)
    (asdf:operate 'asdf:load-op :deoxybyte-systems)))

(defpackage :uk.co.deoxybyte-gzip-system
  (:use :common-lisp :asdf)
  (:import-from :deoxybyte-systems :lift-test-config :cldoc-config))

(in-package :uk.co.deoxybyte-gzip-system)

(defsystem deoxybyte-gzip
    :name "deoxybyte-gzip"
    :author "Keith James"
    :version "0.1.5"
    :licence "GPL v3"
    :in-order-to ((test-op (load-op :deoxybyte-gzip :deoxybyte-gzip-test)))
    :depends-on ((:version :cffi "0.10.3")
                 (:version :deoxybyte-io "0.5.3"))
    :components ((:module :deoxybyte-gzip
                          :serial t
                          :pathname "src/"
                          :components ((:file "package")
                                       (:file "zlib-ffi")
                                       (:file "rfc1952")
                                       (:file "conditions")
                                       (:file "deoxybyte-gzip")
                                       (:file "gzip-stream")))
                 (:lift-test-config :lift-tests
                                    :pathname "deoxybyte-gzip-test.config"
                                    :target-system :deoxybyte-gzip)
                 (:cldoc-config :cldoc-documentation
                                :pathname "doc/html/"
                                :target-system :deoxybyte-gzip)))
