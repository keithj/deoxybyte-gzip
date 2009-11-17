;;;
;;; Copyright (C) 2007-2009 Keith James. All rights reserved.
;;;
;;; This file is part of cl-gzip.
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

(in-package :uk.co.deoxybyte-gzip)

(define-condition zlib-io-error (io-error)
  ((text :initform nil
         :initarg :text
         :reader text-of
         :documentation "Error message text.")
   (errno :initform nil
          :initarg :errno
          :reader errno-of
          :documentation "The C error number."))
  (:report (lambda (condition stream)
             (format stream "Zlib error~@[: ~a~]."
                     (text-of condition))))
  (:documentation "A condition raised when an error occurs reading
  from or writing to a gzipped stream."))
