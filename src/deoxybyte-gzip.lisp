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

(in-package :uk.co.deoxybyte-gzip)

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
- compression (integer): The zlib compression level, if
  compressing. An integer between 0 and 9, inclusive."
  `(let ((,var (gz-open ,filespec :direction ,direction
                        :compression ,compression)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var
         (gz-close ,var)))))

(defun gz-open (filespec &key (direction :input)
                (compression nil compression-supplied-p))
  "Opens FILESPEC for compression or decompression.

Arguments:

- filespec (pathname designator): The file to open.

Key:

- direction (keyword): The direction, one of either :input or :output ,
  defaulting to :input .
- compression (integer): The zlib compression level, if
  compressing. An integer between 0 and 9, inclusive."
  (when (and compression-supplied-p compression)
    (assert (and (integerp compression) (<= 0 compression 9)) (compression)
            (txt "Invalid compression factor ~a:"
                 "expected an integer between 0 and 9, inclusive.")
            compression))
  (let ((gz (make-gz :ptr (gzopen (pathstring filespec)
                                  (format nil "~c~@[~d~]"
                                          (ecase direction
                                            (:input #\r)
                                            (:output #\w)) compression))
                     :open-p t)))
    (if (null-pointer-p (gz-ptr gz))
        (error 'zlib-io-error :errno *c-error-number*
               :text (format nil "failed to open ~a (~a)"
                             filespec (zlib-error-message gz)))
      gz)))

(defun gz-close (gz)
  "Closes GZ, if open."
  (when (gz-open-p gz)
    (setf (gz-open-p gz) nil)
    (or (= +z-ok+ (gzclose (gz-ptr gz)))
        (error 'zlib-io-error :errno *c-error-number*
               :text (format nil "failed to close cleanly (~a)"
                             (zlib-error-message gz))))))

(defun gz-eof-p (gz)
  "Returns T if GZ has reached EOF, or NIL otherwise."
  (gzeof (gz-ptr gz)))

(defun gz-read (gz buffer n)
  "Reads up to N bytes from GZ into octet vector BUFFER. Returns the
number of bytes read, which may be 0."
  (cond ((not (gz-open-p gz))
         (error 'zlib-io-error :text "attempted to read from a closed stream"))
        ((gz-eof-p gz)
         0)
        (t
         (with-foreign-pointer (buf (length buffer))
           (let ((x (gzread (gz-ptr gz) buf n)))
             (cond ((zerop x)
                    0)
                   ((= -1 x)
                    (error 'zlib-io-error :errno *c-error-number*
                           :text (zlib-error-message gz)))
                   (t
                    (loop
                       for i from 0 below x
                       do (setf (aref buffer i)
                                (mem-aref buf :char i))
                       finally (return x)))))))))

(defun gz-write (gz buffer n)
  "Writes up to N bytes in octet vector BUFFER to GZ. Returns the
number of bytes written. BUFFER must be a simple-array of
unsigned-byte 8."
  (declare (optimize (speed 3)))
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum n))
  (unless (gz-open-p gz)
    (error 'zlib-io-error :text "attempted to write to a closed stream"))
  (with-foreign-pointer (buf (length buffer))
    (loop
       for i from 0 below n
       do (setf (mem-aref buf :char i) (aref buffer i)))
    (let ((x (the fixnum (gzwrite (gz-ptr gz) buf n))))
      (if (zerop x)
          (error 'zlib-io-error :errno *c-error-number*
                 :text (zlib-error-message gz))
        x))))

(defun gz-read-string (gz str n)
  "Reads up to N characters from GZ into string STR. Returns the
number of characters read, which may be 0."
  (cond ((not (gz-open-p gz))
         (error 'zlib-io-error :text "attempted to read from a closed stream"))
        ((gz-eof-p gz)
         0)
        (t
         (let ((x (gzgets (gz-ptr gz) str (1+ n))))
           (if (= -1 x)
               (error 'zlib-io-error :errno *c-error-number*
                      :text (zlib-error-message gz))
             x)))))

(defun gz-write-string (gz buffer)
  "Writes up to N characters in octet vector BUFFER to GZ. Returns the
number of characters written."
  (unless (gz-open-p gz)
    (error 'zlib-io-error :text "attempted to write to a closed stream"))
  (let ((n (gzputs (gz-ptr gz) buffer)))
    (if (= -1 n)
        (error 'zlib-io-error :errno *c-error-number*
               :text (zlib-error-message gz))
      n)))

(defun gz-read-byte (gz)
  "Returns a byte read from GZ, or :eof ."
  (cond ((not (gz-open-p gz))
         (error 'zlib-io-error :text "attempted to read from a closed stream"))
        ((gz-eof-p gz)
         :eof)
        (t
         (let ((b (gzgetc (gz-ptr gz))))
           (if (= -1 b)
               (error 'zlib-io-error :errno *c-error-number*
                      :text (zlib-error-message gz))
             b)))))

(defun gz-write-byte (gz byte)
  "Writes BYTE to GZ and returns BYTE."
  (unless (gz-open-p gz)
    (error 'zlib-io-error :text "attempted to write to a closed stream"))
  (let ((b (gzputc (gz-ptr gz) byte)))
    (if (= -1 b)
        (error 'zlib-io-error :errno *c-error-number*
               :text (zlib-error-message gz))
      b)))

(defun zlib-error-message (gz)
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
  (let ((out-filespec (or out-filespec (gunzip-pathname in-filespec))))
    (assert (not (equalp in-filespec out-filespec)) (in-filespec)
            "Unable to make implicit output filename from ~s, please specify OUT-FILESPEC explicitly."
            in-filespec)
    (with-open-file (stream out-filespec :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (with-gz-file (gz in-filespec)
        (let ((x (1- (expt 2 16))))
          (loop
             with buffer = (make-array x :element-type '(unsigned-byte 8))
             for n = (gz-read gz buffer x)
             sum n into num-bytes
             do (write-sequence buffer stream :end n)
             until (gz-eof-p gz)
             finally (return (values out-filespec num-bytes))))))))

(defun gzip (in-filespec &optional out-filespec)
  "Compresses IN-FILESPEC to OUT-FILESPEC using the default
compression level. Returns two values, OUT-FILESPEC and the number of
bytes compressed."
  (let ((out-filespec (or out-filespec (gzip-pathname in-filespec))))
    (assert (not (equalp in-filespec out-filespec)) (in-filespec)
            "Unable to make implicit output filename from ~s, please specify OUT-FILESPEC explicitly."
            in-filespec)
    (with-open-file (stream in-filespec :element-type '(unsigned-byte 8))
      (with-gz-file (gz out-filespec :direction :output)
        (let ((x (1- (expt 2 16))))
          (loop
             with buffer = (make-array x :element-type '(unsigned-byte 8))
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
