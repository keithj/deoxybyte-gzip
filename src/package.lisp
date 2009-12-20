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
   #:+z-no-flush+
   #:+z-full-flush+
   #:+z-finish+
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
   #:+z-default-compression+
   #:+z-filtered+
   #:+z-huffman-only+
   #:+z-default-strategy+
   #:+z-deflated+

   #:z-stream
   #:next-in
   #:avail-in
   #:total-in
   #:next-out
   #:avail-out
   #:total-out
   #:msg
   #:zalloc
   #:zfree
   #:opaque
   #:data-type
   #:adler
   #:reserved

   #:zlib-version
   #:deflate-init
   #:%deflate-init
   #:deflate-init2
   #:%deflate-init2
   #:%deflate
   #:deflate-end
   #:inflate-init
   #:%inflate-init
   #:inflate-init2
   #:%inflate-init2
   #:%inflate
   #:inflate-end
 
   #:%compress
   #:%compress2
   #:%uncompress
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
   #:gzerror

   #:*c-error-number*)
  (:documentation "The zlib-ffi package provides a FFI for most of
  Zlib. It may be used directly, but is intended for use via the
  uk.co.deoxybyte-gzip package which wraps Zlib in a Lisp-style
  interface."))

(defpackage :uk.co.deoxybyte-gzip
  (:use #:common-lisp #:cffi #:zlib-ffi #:deoxybyte-io)
  (:nicknames #:deoxybyte-gzip #:gz)
  (:import-from #:deoxybyte-utilities #:concat-strings #:txt)
  (:export
   ;; Constants

   ;; Conditions
   #:zlib-error
   #:gz-io-error

   ;; Macros
   #:with-gz-file

   ;; Classes
   #:gz

   #:gzip-stream
   #:gzip-input-stream
   #:gzip-output-stream

   ;; Functions
   #:make-gzip-stream
   #:compress
   #:uncompress
   #:deflate-stream
   #:inflate-stream
   #:deflate-vector
   #:inflate-vector

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
   #:gunzip)
  (:documentation "The deoxybyte-gzip system provides a Lisp interface
to Zlib including a regular function interface to gzipped files, a
Gray-streams interface to gzipped files and utility gzip/gunzip
functions built on the former.

Also provided are functions for inflating and deflating to and from
Lisp octet vectors and Lisp octet streams, which may be tuned using
the Zlib tuning parameters described in the Zlib C function
deflateInit2."))
