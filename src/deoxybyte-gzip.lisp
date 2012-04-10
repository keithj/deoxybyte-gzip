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

(in-package :uk.co.deoxybyte-gzip)

(defconstant +default-zlib-buffer-size+ 4096)

(defstruct gz
  "A gzip handle.

- ptr: A foreign pointer to a zlib struct.
- open-p: A flag that, while T, indicates that the foreign pointer may
  be freed."
  (ptr nil :type t)
  (open-p nil :type t))

(defmacro with-gz-file ((var filespec &key (direction :input) compression)
                        &body body)
  "Executes BODY with VAR bound to a GZ handle structure created by
opening the file denoted by FILESPEC.

Arguments:

- var (symbol): The symbol to be bound.
- filespec (pathname designator): The file to open.

Key:

- direction (keyword): The direction, one of either :input or :output ,
  defaulting to :input .
- compression (integer): The zlib compression level, if compressing.
  An integer between 0 and 9, inclusive."
  `(let ((,var (gz-open ,filespec :direction ,direction
                        :compression (or ,compression
                                         +z-default-compression+))))
     (unwind-protect
          (progn
            ,@body)
       (when ,var
         (gz-close ,var)))))

(defun compress (source dest &key (source-start 0) source-end
                 (dest-start 0) (compression +z-default-compression+))
  "Compresses bytes in array SOURCE to array DEST, returning DEST and
the compressed size in bytes."
  (check-type source (vector octet))
  (check-type dest (vector octet))
  (let ((source-end (or source-end (length source))))
    (check-arguments (<= 0 source-start source-end (length source))
                     (source-start source-end)
                     "must satisfy (<= 0 source-start source-end ~d)"
                     (length source))
    (check-arguments (<= dest-start (length dest)) (dest-start)
                     "must be <= ~d" (length dest))
    (check-arguments (and (integerp compression)
                          (or (= +z-default-compression+ compression)
                              (<= 0 compression 9)))
                     (compression)
                     "must be an integer between 0 and 9, inclusive")
    (let ((source-len (- source-end source-start))
          (dest-len (- (length dest) dest-start)))
      (with-foreign-objects ((sbytes :uint8 source-len)
                             (dbytes :uint8 dest-len)
                             (dlen :long))
        (setf (mem-ref dlen :long) dest-len)
        (loop
           for i from source-start below source-end
           for j = 0 then (1+ j)
           do (setf (mem-aref sbytes :uint8 j) (aref source i)))
        (let ((val (%compress2 dbytes dlen sbytes source-len compression)))
          (if (minusp val)
              (z-error val)
              (loop
                 with compressed-len = (mem-ref dlen :long)
                 for i from dest-start below compressed-len
                 do (setf (aref dest i) (mem-aref dbytes :uint8 i))
                 finally (return (values dest compressed-len)))))))))

(defun uncompress (source dest &key (source-start 0) source-end (dest-start 0))
  "Uncompresses bytes in array SOURCE to array DEST, returning DEST
and the compressed size in bytes."
  (check-type source (vector octet))
  (check-type dest (vector octet))
  (let ((source-end (or source-end (length source))))
    (check-arguments (<= 0 source-start source-end (length source))
                     (source-start source-end)
                     "must satisfy (<= 0 source-start source-end ~d)"
                     (length source))
    (check-arguments (<= dest-start (length dest)) (dest-start)
                     "must be <= ~d" (length dest))
    (let* ((source-len (- source-end source-start))
           (dest-len (- (length dest) dest-start)))
      (with-foreign-objects ((sbytes :uint8 source-len)
                             (dbytes :uint8 dest-len)
                             (dlen :long))
        (setf (mem-ref dlen :long) dest-len)
        (loop
           for i from source-start below source-end
           for j from 0 below source-len
           do (setf (mem-aref sbytes :uint8 j) (aref source i)))
        (let ((val (%uncompress dbytes dlen sbytes source-len)))
          (if (minusp val)
              (z-error val)
              (loop
                 with uncompressed-len = (mem-ref dlen :long)
                 for i from dest-start below uncompressed-len
                 do (setf (aref dest i) (mem-aref dbytes :uint8 i))
                 finally (return (values dest uncompressed-len)))))))))

(defun gz-open (filespec &key (direction :input)
                (compression +z-default-compression+))
  "Opens FILESPEC for compression or decompression.

Arguments:

- filespec (pathname designator): The file to open.

Key:

- direction (keyword): The direction, one of either :input or :output ,
  defaulting to :input .
- compression (integer): The zlib compression level, if
  compressing. An integer between 0 and 9, inclusive."
  (check-arguments (and (integerp compression)
                        (or (= +z-default-compression+ compression)
                            (<= 0 compression 9)))
                   (compression)
                   "must be an integer between 0 and 9, inclusive")
  (let* ((mode (format nil "~c~@[~d~]" (ecase direction
                                         (:input #\r)
                                         (:output #\w)) compression))
         (designator (maybe-standard-stream filespec))
         (ptr (if (streamp designator)
                  (gzdopen (file-descriptor designator) mode)
                  (gzopen (pathstring filespec) mode)))
         (gz (make-gz :ptr ptr :open-p t)))
    (if (null-pointer-p (gz-ptr gz))
        (gz-error gz t (format nil "failed to open ~a (~a)"
                               filespec (gz-error-message gz)))
        gz)))

(defun gz-close (gz)
  "Closes GZ, if open."
  (when (gz-open-p gz)
    (setf (gz-open-p gz) nil)
    (or (= +z-ok+ (gzclose (gz-ptr gz)))
        (gz-error gz t (format nil "failed to close cleanly (~a)"
                               (gz-error-message gz))))))

(defun gz-eof-p (gz)
  "Returns T if GZ has reached EOF, or NIL otherwise."
  (gzeof (gz-ptr gz)))

(defun gz-tell (gz)
  (gztell (gz-ptr gz)))

(defun gz-seek (gz offset)
  (gzseek (gz-ptr gz) offset :seek-set))

(defun gz-flush (gz flush-mode)
  (gzflush (gz-ptr gz) flush-mode))

(defun gz-read (gz buffer n)
  "Reads up to N bytes from GZ into octet vector BUFFER. Returns the
number of bytes read, which may be 0."
  (declare (optimize (speed 3)))
  (declare (type simple-octet-vector buffer))
  (check-arguments (gz-open-p gz) (gz) "attempted to read from a closed stream")
  (if (gz-eof-p gz)
      0
      (with-foreign-pointer (buf (length buffer))
        (let ((x (gzread (gz-ptr gz) buf n)))
          (declare (type fixnum x))
          (cond ((zerop x)
                 0)
                ((= -1 x)
                 (gz-error gz t t))
                (t
                 (loop
                    for i from 0 below x
                    do (setf (aref buffer i) (mem-aref buf :uint8 i))
                    finally (return x))))))))

(defun gz-write (gz buffer n)
  "Writes up to N bytes in octet vector BUFFER to GZ. Returns the
number of bytes written. BUFFER must be a simple-array of
unsigned-byte 8."
  (declare (optimize (speed 3)))
  (declare (type simple-octet-vector buffer)
           (type vector-index n))
  (check-arguments (gz-open-p gz) (gz) "attempted to write to a closed stream")
  (with-foreign-pointer (buf (length buffer))
    (loop
       for i from 0 below n
       do (setf (mem-aref buf :uint8 i) (aref buffer i)))
    (let ((x (gzwrite (gz-ptr gz) buf n)))
      (if (zerop x)
          (gz-error gz t t)
          x))))

(defun gz-read-string (gz str n)
  "Reads up to N characters from GZ into string STR. Returns a string
or :eof . Relies on the CFFI conversion terminating the returned
string, so the result may be shorter than the STR argument."
  (check-arguments (gz-open-p gz) (gz) "attempted to read from a closed stream")
  (if (gz-eof-p gz)
      :eof
      (let ((x (gzgets (gz-ptr gz) str (1+ n))))
        (cond ((null x)
               (cond ((gz-eof-p x)
                      :eof)
                     (t
                      (gz-error gz t t))))
              (t
               x)))))

(defun gz-write-string (gz buffer)
  "Writes up to N characters in octet vector BUFFER to GZ. Returns the
number of characters written."
  (check-arguments (gz-open-p gz) (gz) "attempted to write to a closed stream")
  (let ((n (gzputs (gz-ptr gz) buffer)))
    (if (= -1 n)
        (gz-error gz t t)
        n)))

(defun gz-read-byte (gz)
  "Returns a byte read from GZ, or :eof ."
  (check-arguments (gz-open-p gz) (gz) "attempted to read from a closed stream")
  (if (gz-eof-p gz)
      :eof
      (let ((b (gzgetc (gz-ptr gz))))
        (if (= -1 b)
            (gz-error gz t t)
            b))))

(defun gz-write-byte (gz byte)
  "Writes BYTE to GZ and returns BYTE."
  (check-arguments (gz-open-p gz) (gz) "attempted to read from a closed stream")
  (let ((b (gzputc (gz-ptr gz) byte)))
    (if (= -1 b)
        (gz-error gz t t)
        b)))

(defun gz-error (gz &optional errno message)
  "Raises a {define-condition gz-io-error} . An ERRNO integer and
MESSAGE string may be supplied. If ERRNO or MESSAGE are T, they are
retrieved using *C-ERROR-NUMBER* and {defun gz-error-message}
respectively."
  (check-arguments (gz-p gz) (gz) "must be a gz instance")
  (error 'gz-io-error :errno (if (eql t errno)
                                 *c-error-number*
                                 errno)
         :text (if (eql t message)
                   (gz-error-message gz)
                   message)))

(defun gz-error-message (gz)
  "Returns a zlib error message string relevant to GZ."
  (let ((msg (with-foreign-pointer (ptr (foreign-type-size :int))
               (gzerror (gz-ptr gz) ptr))))
    (if (string= "" msg)
        "no error message available"
        msg)))

(defun gunzip (in-filespec &optional out-filespec)
  "Decompresses IN-FILESPEC to OUT-FILESPEC using the default
compression level. Returns two values, OUT-FILESPEC and the number of
bytes decompressed."
  (check-arguments (probe-file in-filespec) (in-filespec) "file does not exist")
  (let ((out-filespec (or out-filespec (gunzip-pathname in-filespec))))
    (assert (not (equalp in-filespec out-filespec)) (in-filespec)
            (txt "Unable to make implicit output filename from ~s,"
                 "please specify OUT-FILESPEC explicitly.")
            in-filespec)
    (with-open-file (stream out-filespec :direction :output
                            :element-type 'octet
                            :if-exists :overwrite :if-does-not-exist :create)
      (with-gz-file (gz in-filespec)
        (let ((x (1- (expt 2 16))))
          (loop
             with buffer = (make-array x :element-type 'octet
                                       :initial-element 0)
             for n of-type vector-index = (gz-read gz buffer x)
             sum n into num-bytes
             do (write-sequence buffer stream :end n)
             until (gz-eof-p gz)
             finally (return (values out-filespec num-bytes))))))))

(defun gzip (in-filespec &optional out-filespec)
  "Compresses IN-FILESPEC to OUT-FILESPEC using the default
compression level. Returns two values, OUT-FILESPEC and the number of
bytes compressed."
  (check-arguments (probe-file in-filespec) (in-filespec) "file does not exist")
  (let ((out-filespec (or out-filespec (gzip-pathname in-filespec))))
    (assert (not (equalp in-filespec out-filespec)) (in-filespec)
            (txt "Unable to make implicit output filename from ~s,"
                 "please specify OUT-FILESPEC explicitly.")
            in-filespec)
    (with-open-file (stream in-filespec :element-type 'octet)
      (with-gz-file (gz out-filespec :direction :output :compression 6)
        (let ((x (1- (expt 2 16))))
          (loop
             with buffer = (make-array x :element-type 'octet
                                       :initial-element 0)
             for n = (read-sequence buffer stream)
             sum n into num-bytes
             while (plusp n)
             do (gz-write gz buffer n)
             finally (return (values out-filespec num-bytes))))))))

(defun gzip-pathname (pathname)
  "Returns a copy of PATHNAME. A GZ type component is added, unless
already present. Any existing type component becomes part of the name
component.

For example:

foo.gz  -> foo.gz
foo.tar -> foo.tar.gz"
  (if (string= "gz" (pathname-type pathname))
      (pathname pathname)
      (merge-pathnames
       (make-pathname :host (pathname-host pathname)
                      :device (pathname-device pathname)
                      :directory (pathname-directory pathname)
                      :name (concatenate 'string
                                         (pathname-name pathname) "."
                                         (pathname-type pathname)))
       (make-pathname :type "gz"))))

(defun gunzip-pathname (pathname)
  "Returns a copy of PATHNAME. Any GZ type component is removed and
the name component parsed to supply the new type, if possible.

For example:

foo.tar    -> foo.tar
foo.tar.gz -> foo.tar"
  (if (string= "gz" (pathname-type pathname))
      (let ((host (pathname-host pathname))
            (device (pathname-device pathname))
            (directory (pathname-directory pathname))
            (dot (position #\. (pathname-name pathname) :from-end t)))
        (if dot
            (make-pathname :host host
                      :device device
                      :directory directory
                      :name (subseq (pathname-name pathname) 0 dot)
                      :type (subseq (pathname-name pathname) (1+ dot)))
            (make-pathname :host host
                           :device device
                           :directory directory
                           :name (pathname-name pathname))))
      (pathname pathname)))

(defun deflate-stream (source dest
                       &key (buffer-size +default-zlib-buffer-size+)
                       (compression +z-default-compression+)
                       suppress-header (window-bits 15) (mem-level 8)
                       (strategy :default-strategy))
  "Deflates stream SOURCE to stream DEST.

Arguments:

- source (stream): A binary input stream.
- dest (stream): A binary output stream.

Key:

- buffer-size (fixnum): The size of the internal buffer used by Zlib.
- compression (fixnum): The Zlib compression factor (0-9, inclusive).
- suppress-header (boolean): Exposes a feature of Zlib whereby the
  compressed data may be produced without a Zlib header, trailer or
  checksum.
- window-bits (fixnum): The Zlib window-bits argument (9-15, inclusive).
- mem-level (fixnum): The Zlib mem-level argument (1-9, inclusive).
- strategy (fixnum): The Zlib strategy argument (one
  of :default-strategy , :filtered or :huffman-only ).

Returns:
- Number of bytes read.
- Number of bytes written."
  (check-arguments (input-stream-p source) (source) "must bean input-stream")
  (check-arguments (output-stream-p dest) (dest) "must be an output-stream")
  (z-stream-operation :deflate source dest
                      #'fill-from-stream #'empty-to-stream buffer-size
                      :compression compression
                      :suppress-header suppress-header
                      :window-bits window-bits :mem-level mem-level
                      :strategy strategy))

(defun inflate-stream (source dest
                       &key (buffer-size +default-zlib-buffer-size+)
                       suppress-header (window-bits 15))
  "Inflates stream SOURCE to stream DEST.

Arguments:

- source (stream): A binary input stream.
- dest (stream): A binary output stream.

Key:

- buffer-size (fixnum): The size of the internal buffer used by Zlib.
- suppress-header (boolean): Exposes a feature of Zlib whereby the
  compressed data may be produced without a Zlib header, trailer or
  checksum. This must be set T if the data were compressed with the
  header suppressed.
- window-bits (fixnum): The Zlib window-bits argument (9-15, inclusive).

Returns:
- Number of bytes read.
- Number of bytes written."
  (check-arguments (input-stream-p source) (source) "must be an input-stream")
  (check-arguments (output-stream-p dest) (dest) "must be output-stream")
  (z-stream-operation :inflate source dest
                      #'fill-from-stream #'empty-to-stream buffer-size
                      :suppress-header suppress-header
                      :window-bits window-bits))

(defun deflate-vector (source dest
                       &key (compression +z-default-compression+)
                       suppress-header (window-bits 15) (mem-level 8)
                       (strategy :default-strategy) (backoff 0))
  "Deflates vector SOURCE to vector DEST.

Arguments:

- source (vector): An octet vector.
- dest (vector): An octet vector.

Key:

- compression (fixnum): The Zlib compression factor (0-9, inclusive).
- suppress-header (boolean): Exposes an undocumented feature of Zlib
  whereby the compressed data may be produced without a Zlib header or
  checksum.
- window-bits (fixnum): The Zlib window-bits argument (9-15, inclusive).
- mem-level (fixnum): The Zlib mem-level argument (1-9, inclusive).
- strategy (fixnum): The Zlib strategy argument (one
  of :default-strategy , :filtered or :huffman-only ).

Returns:
- The DEST vector, containing compressed data.
- Number of bytes read.
- Number of bytes written (consequently the end position of the
  compressed data in DEST)."
  (check-type source (vector octet))
  (check-type dest (vector octet))
  (z-vector-operation :deflate source dest backoff
                      :compression compression
                      :suppress-header suppress-header
                      :window-bits window-bits :mem-level mem-level
                      :strategy strategy))

(defun inflate-vector (source dest &key suppress-header (window-bits 15)
                       (backoff 0))
  "Inflates vector SOURCE to vector DEST.

Arguments:

- source (vector): An octet vector.
- dest (vector): An octet vector.

Key:

- compression (fixnum): The Zlib compression factor (0-9, inclusive).
- suppress-header (boolean): Exposes an undocumented feature of Zlib
  whereby the compressed data may be produced without a Zlib header or
  checksum. This must be set T if the data were compressed with the
  header suppressed.
- window-bits (fixnum): The Zlib window-bits argument (9-15, inclusive).

Returns:
- The DEST vector, containing decompressed data.
- Number of bytes read.
- Number of bytes written (consequently the end position of the
  decompressed data in DEST)."
  (check-type source (vector octet))
  (check-type dest (vector octet))
  (z-vector-operation :inflate source dest backoff
                      :suppress-header suppress-header
                      :window-bits window-bits))

(let ((init (%adler32 0 (null-pointer) 0)))
  (defun adler32 (buffer &key (start 0) end adler32)
     (declare (optimize (speed 3)))
     (declare (type simple-octet-vector buffer))
     (let ((end (or end (length buffer))))
       (declare (type vector-index start end))
       (let ((len (- end start)))
         (with-foreign-pointer (buf len)
           (loop
              for i from start below end
              for j from 0 below len
              do (setf (mem-aref buf :uint8 j) (aref buffer i)))
           (%adler32 (or adler32 init) buf len))))))

(let ((init (%crc32 0 (null-pointer) 0)))
  (defun crc32 (buffer &key (start 0) end crc32)
    (declare (optimize (speed 3)))
    (declare (type simple-octet-vector buffer))
    (let ((end (or end (length buffer))))
       (declare (type vector-index start end))
       (let ((len (- end start)))
         (with-foreign-pointer (buf len)
           (loop
              for i from start below end
              for j from 0 below len
              do (setf (mem-aref buf :uint8 j) (aref buffer i)))
           (%crc32 (or crc32 init) buf len))))))

(defun z-stream-open (operation &key (compression +z-default-compression+)
                      suppress-header (window-bits 15) (mem-level 8)
                      (strategy :default-strategy))
  "Returns a new Z-STREAM initialised for OPERATION (:inflate or :deflate)."
  (check-arguments (and (integerp compression)
                        (or (= +z-default-compression+ compression)
                            (<= 0 compression 9))) (compression)
                            "must be an integer between 0 and 9, inclusive")
  (check-arguments (and (integerp window-bits)
                        (<= 9 window-bits 15)) (window-bits)
                        "must be an integer between 9 and 15, inclusive")
  (check-arguments (and (integerp mem-level)
                        (<= 1 mem-level 9)) (mem-level)
                        "must be an integer between 1 and 9, inclusive")
  (let ((wbits (if suppress-header
                   (- window-bits)
                   window-bits))
        (strat (ecase strategy
                 (:filtered +z-filtered+)
                 (:huffman-only +z-huffman-only+)
                 (:default-strategy +z-default-strategy+))))
    (let* ((z-stream (make-z-stream))
           (val (ecase operation
                  (:deflate (deflate-init2 z-stream compression +z-deflated+
                                           wbits mem-level strat))
                  (:inflate (inflate-init2 z-stream wbits)))))
      (if (minusp val)
          (z-error val)
          z-stream))))

(defun make-z-stream ()
  "Makes an returns a new Z-STREAM with the ZALLOC, ZFREE and OPAQUE
slots filled with null-pointers."
  (let ((zs (foreign-alloc 'z-stream)))
    (with-foreign-slots ((avail-in avail-out zalloc zfree opaque) zs z-stream)
      (setf avail-in 0
            avail-out 0
            zalloc (null-pointer)  ; zlib casts +z-null+ to fn pointer
            zfree (null-pointer)   ; so we emulate that behaviour
            opaque (null-pointer)))
    zs))

(defun z-stream-close (z-stream operation)
  "Signals the end of OPERATION (:inflate or :deflate) on
Z-STREAM and frees the Z-STREAM memory."
  (unwind-protect
       (let ((val (ecase operation
                    (:deflate (deflate-end z-stream))
                    (:inflate (inflate-end z-stream)))))
         (if (minusp val)
             (z-error val)
             t))
    (foreign-free z-stream)))

(defun z-vector-operation (operation source dest backoff &rest z-stream-args)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-octet-vector source dest)
           (type vector-index backoff))
  (check-arguments (or (zerop backoff)
                       (and (plusp backoff) (< backoff (length dest))))
                   (backoff)
                   "must be a positive value < ~d" (length dest))
  (let ((zs (apply #'z-stream-open operation z-stream-args))
        (op-fn (ecase operation
                 (:inflate #'%inflate)
                 (:deflate #'%deflate)))
        (reset-fn (ecase operation
                    (:inflate #'inflate-reset)
                    (:deflate #'deflate-reset))))
    (let ((source-buffer (replace (make-shareable-byte-vector (length source))
                                  source))
          (dest-buffer (replace (make-shareable-byte-vector (length dest))
                                dest)))
      (unwind-protect
          (with-foreign-slots ((avail-in next-in avail-out next-out
                                total-in total-out) zs z-stream)
            (declare (type vector-index total-in total-out))
            (with-pointer-to-vector-data (in source-buffer)
              (with-pointer-to-vector-data (out dest-buffer)
                (loop
                  with avail of-type vector-index = (length source)
                  do (cond ((plusp avail)
                            (setf next-in in
                                  avail-in avail
                                  next-out out
                                  avail-out (length dest))
                            (let ((x (funcall op-fn zs +z-finish+)))
                              (declare (type fixnum backoff x))
                              (cond ((= +z-stream-error+ x)
                                     (z-error x))
                                    ((and (plusp backoff)
                                          (= +z-ok+ x)) ; Did not fit in dest
                                     (decf avail backoff)
                                     (funcall reset-fn zs))
                                    ((= +z-ok+ x)
                                     (z-error +z-buf-error+
                                              (txt "insufficient space in DEST"
                                                   "for compressed data")))
                                    (t
                                     (return (values (replace dest dest-buffer)
                                                     total-in
                                                     total-out))))))
                           (t
                            (z-error +z-buf-error+
                                     (txt "insufficient space in DEST"
                                          "for compressed data"
                                          "after backoff"))))))))
        (z-stream-close zs operation)))))

(defun z-stream-operation (operation source dest input-fn output-fn buffer-size
                           &rest z-stream-args)
  "Implements Zlib compression/decompression using inflate/deflate on
Lisp streams, as described in the Zlib Usage Example.

Arguments:

- operation (symbol): The operation type, either :inflate or :deflate .
- source (stream): A Lisp octet input stream.
- dest (stream): A Lisp octet output stream.
- input-fn (function): A Lisp function that accepts 3 arguments, an
  input stream, a buffer and an integer n. The function must read up
  to n bytes from the input stream into the buffer and return the
  number of bytes read.
- output-fn (function): A Lisp function that accepts 3 arguments, an
  output stream, a buffer and an integer n. The function must write up
  to n bytes to the output stream into the buffer and return the
  number of bytes written.
- buffer-size (fixnum): The size of the buffer used in the
  compression/decompression step(s).

Rest:
- Keyword arguments passed to {defun z-stream-open} .
  See {defun z-stream-open}

Returns:
- Number of bytes read.
- Number of bytes written."
  (let ((zs (apply #'z-stream-open operation z-stream-args))
        (op-fn (ecase operation
                 (:inflate #'%inflate)
                 (:deflate #'%deflate)))
        (in-buffer (make-shareable-byte-vector buffer-size))
        (out-buffer (make-shareable-byte-vector buffer-size)))
    (unwind-protect
         (with-foreign-slots ((avail-in next-in avail-out next-out
                               total-in total-out) zs z-stream)
           (with-pointer-to-vector-data (in in-buffer)
             (with-pointer-to-vector-data (out out-buffer)
               (flet ((read-from-zs ()
                        (when (zerop avail-in)
                          (let ((num-read (funcall input-fn in-buffer
                                                   source buffer-size)))
                            (declare (type vector-index num-read))
                            (when (plusp num-read)
                              (setf next-in in
                                    avail-in num-read))))
                        avail-in)
                      (write-from-zs ()
                        (let ((fullp (zerop avail-out))
                              (output-bytes (- buffer-size avail-out)))
                          (unless (zerop output-bytes)
                            (funcall output-fn out-buffer dest output-bytes)
                            (setf next-out out
                                  avail-out buffer-size))
                          fullp))) ; Was the output buffer full on writing?
                 (setf next-out out
                       avail-out buffer-size)
                 (loop
                    for num-read = (read-from-zs)
                    while (plusp num-read)
                    do (let ((flush (if (= buffer-size num-read)
                                        +z-no-flush+
                                        +z-finish+)))
                         (loop
                            with out-full = t
                            while out-full
                            do (let ((x (funcall op-fn zs flush)))
                                 (when (= +z-stream-error+ x)
                                   (z-error x))
                                 (setf out-full (write-from-zs)))))
                    finally (progn
                              (return (values total-in total-out))))))))
      (z-stream-close zs operation))))

(defun z-error (errno &optional message)
  "Raises a {define-condition zlib-error} . A MESSAGE string may be
supplied, otherwise it will be determined from ERRNO."
  (error 'zlib-error :errno errno
         :text (or message
                   (cond ((= +z-stream-error+ errno)
                          "zlib stream error")
                         ((= +z-buf-error+ errno)
                          "there was not enough space in the output buffer")
                         ((= +z-mem-error+ errno)
                          "the was not enough memory to perform the operation")
                         ((= +z-data-error+ errno)
                          "the input data were corrupted or incomplete")
                         ((= +z-version-error+ errno)
                          "zlib versions are incompatible")
                         (t
                          (format nil "zlib error ~d" errno))))))

(declaim (inline fill-from-stream))
(defun fill-from-stream (buffer stream n)
  (read-sequence buffer stream :end n))

(declaim (inline empty-to-stream))
(defun empty-to-stream (buffer stream n)
  (write-sequence buffer stream :end n))
