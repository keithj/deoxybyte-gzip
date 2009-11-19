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

(defpackage :zlib-ffi
  (:use #:common-lisp #:cffi)
  (:export
   #:+z-ok+
   #:+z-null+
   #:+z-stream-end+
   #:+z-need-dict+
   #:+z-errno+
   #:+z-stream-error+
   #:+z-data-error+
   #:+z-mem-error+
   #:+z-buf-error+
   #:+z-version-error+
   #:+z-no-flush+
   #:+z-sync-flush+
   #:+z-full-flush+
   #:+z-finish+
   #:gzerror
   #:gzopen
   #:gzclose
   #:gzdopen
   #:gztell
   #:gzseek
   #:gzeof
   #:gzflush
   #:gzread
   #:gzwrite
   #:gzgetc
   #:gzputc
   #:gzgets
   #:gzputs

   #:*c-error-number*
   #:seek-directive))

(defpackage :uk.co.deoxybyte-gzip
  (:use #:common-lisp #:cffi #:zlib-ffi #:deoxybyte-io)
  (:nicknames #:gz)
  (:import-from #:deoxybyte-utilities #:concat-strings #:txt)
  (:export
   ;; Constants

   ;; Conditions
   #:zlib-io-error

   ;; Classes
   #:gz

   ;; Macros
   #:with-gz-file

   ;; Functions
   #:gz-open
   #:gz-close
   #:gz-eof-p
   #:gz-read
   #:gz-write
   #:gz-read-string
   #:gz-write-string
   #:gz-read-byte
   #:gz-write-byte

   #:gzip-pathname
   #:gunzip-pathname
   #:gzip
   #:gunzip))
