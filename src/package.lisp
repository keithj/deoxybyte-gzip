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
   #:%adler32
   #:%crc32
   #:reserved

   #:zlib-version
   #:deflate-init
   #:%deflate-init
   #:deflate-init2
   #:%deflate-init2
   #:%deflate
   #:deflate-end
   #:deflate-reset
   #:inflate-init
   #:%inflate-init
   #:inflate-init2
   #:%inflate-init2
   #:%inflate
   #:inflate-end
   #:inflate-reset

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
  (:use #:common-lisp #:cffi #:zlib-ffi #:deoxybyte-utilities #:deoxybyte-io)
  (:import-from #:deoxybyte-unix #:file-descriptor #:maybe-standard-stream)
  (:nicknames #:deoxybyte-gzip #:gz)
  (:export
   ;; Zlib API
   #:zlib-error
   #:gz-io-error

   #:gz
   #:gz-open
   #:gz-close
   #:gz-eof-p
   #:gz-read
   #:gz-write
   #:gz-read-string
   #:gz-write-string
   #:gz-read-byte
   #:gz-write-byte
   #:with-gz-file

   #:compress
   #:uncompress
   #:deflate-stream
   #:inflate-stream
   #:deflate-vector
   #:inflate-vector
   #:adler32
   #:crc32

   #:gzip-pathname
   #:gunzip-pathname
   #:gzip
   #:gunzip

   ;; Gray streams API
   #:gzip-stream
   #:gzip-input-stream
   #:gzip-output-stream
   #:gzip-line-input-stream
   #:gzip-open
   #:with-open-gzip
   
   ;; RFC1952 
   #:+id1+
   #:+id2+
   #:+cm-deflate+
   #:+flag-text+
   #:+flag-fhcrc+
   #:+flag-extra+
   #:+flag-name+
   #:+flag-comment+
   #:+xfl-slowest+
   #:+xfl-fastest+

   #:+os-fat-filesystem+
   #:+os-amiga+
   #:+os-vms+
   #:+os-unix+
   #:+os-vm/cms+
   #:+os-atari-tos+
   #:+os-hpfs-filesystem+
   #:+os-macintosh+
   #:+os-z-system+
   #:+os-cp/m+
   #:+os-tops-20+
   #:+os-ntfs-filesystem+
   #:+os-qdos+
   #:+os-acorn-riscos+
   #:+os-unknown+

   #:gz-member
   #:make-gz-member
   #:gz-member-id1
   #:gz-member-id2
   #:gz-member-cm
   #:gz-member-flg
   #:gz-member-mtime
   #:gz-member-xfl
   #:gz-member-os
   #:gz-member-xlen
   #:gz-member-isize
   #:gz-member-crc32
   #:gz-member-cdata
   #:gz-member-cend)                                   
  (:documentation "The deoxybyte-gzip system provides a Lisp interface
to Zlib including a regular function interface to gzipped files, a
Gray-streams interface to gzipped files and utility gzip/gunzip
functions built on the former.

Functions are provided for inflating and deflating to and from Lisp
octet vectors and Lisp octet streams, which may be tuned using the
Zlib tuning parameters described in the Zlib C function deflateInit2.

In addition, a basic implementation of the data structure described in
RFC1952 is included, allowing a hybrid approach to reading gzip data,
using native Lisp streams and Zlib inflate/deflate."))
