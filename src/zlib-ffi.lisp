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

(in-package :zlib-ffi)

(define-foreign-library zlib
  (t (:default "libz")))

(use-foreign-library zlib)

(defconstant +z-ok+ 0 "No error.")
(defconstant +z-null+ 0)
(defconstant +z-stream-end+ 1)
(defconstant +z-need-dict+ 2)
(defconstant +z-errno+ -1)
(defconstant +z-stream-error+ -2)
(defconstant +z-data-error+ -3)
(defconstant +z-mem-error+ -4)
(defconstant +z-buf-error+ -5)
(defconstant +z-version-error+ -6)

(defconstant +z-no-flush+ 0)
(defconstant +z-sync-flush+ 2)
(defconstant +z-full-flush+ 3)
(defconstant +z-finish+ 4)

(defconstant +z-no-compression+ 0)
(defconstant +z-best-speed+ 1)
(defconstant +z-best-compression+ 9)
(defconstant +z-default-compression+ -1)

(defconstant +z-null+ 0)

(defcvar ("errno" *c-error-number*) :int
  "Number of last error.")

(defcfun ("gzerror" gzerror) :string
  (gz :pointer)
  (errnum :pointer))

(defcfun ("gzopen" gzopen) :pointer
  (path :string)
  (mode :string))

(defcfun ("gzdopen" gzdopen) :pointer
  (fd :int)
  (mode :string))

(defcfun ("gzeof" gzeof) :boolean
  (gz :pointer))

(defcfun ("gzflush" gzflush) :int
  (gz :pointer)
  (flush :int))

(defcfun ("gzclose" gzclose) :int
  (gz :pointer))

(defcfun ("gzread" gzread) :int
  (gz :pointer)
  (buf :pointer)
  (len :int))

(defcfun ("gzwrite" gzwrite) :int
  (gz :pointer)
  (buf :pointer)
  (len :int))

(defcfun ("gzgets" gzgets) :string
  (gz :pointer)
  (buf :string)
  (len :int))

(defcfun ("gzputs" gzputs) :int
  (gz :pointer)
  (str :string))

(defcfun ("gzgetc" gzgetc) :int
  (gz :pointer))

(defcfun ("gzputc" gzputc) :int
  (gz :pointer)
  (char :uint))


