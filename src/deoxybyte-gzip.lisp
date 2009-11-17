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
- open-p: A flag indicating that while T indicates that the foreign
  pointer may be freed."
  (ptr nil :type t)
  (open-p nil :type t))

(defun gz-open (filespec &key (direction :input)
                (compression nil compression-supplied-p))
   (when compression-supplied-p
     (assert (and (integerp compression)
                  (<= 0 compression 9))
             (compression)
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
        (error 'zlib-io-error :errno unix-ffi:*c-error-number*
               :text (format nil "failed to open ~a (~a)"
                             filespec (zlib-error-message gz)))
      gz)))

(defun gz-close (gz)
  (when (gz-open-p gz)
    (setf (gz-open-p gz) nil)
    (or (= +z-ok+ (gzclose (gz-ptr gz)))
        (error 'zlib-io-error :errno unix-ffi:*c-error-number*
               :text (format nil "failed to close cleanly (~a)"
                             (zlib-error-message gz))))))

(defun gz-eof-p (gz)
  (gzeof (gz-ptr gz)))

(defun gz-read (gz buffer n)
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
                    (error 'zlib-io-error :errno unix-ffi:*c-error-number*
                           :text (zlib-error-message gz)))
                   (t
                    (loop
                       for i from 0 below x
                       do (setf (aref buffer i)
                                (mem-aref buf :char i))
                       finally (return x)))))))))

(defun gz-write (gz buffer n)
  (unless (gz-open-p gz)
    (error 'zlib-io-error :text "attempted to write to a closed stream"))
  (with-foreign-pointer (buf (length buffer))
    (loop
       for i from 0 below n
       do (setf (mem-aref buf :char i) (aref buffer i)))
    (let ((x (gzwrite (gz-ptr gz) buf n)))
      (if (zerop x)
          (error 'zlib-io-error :errno unix-ffi:*c-error-number*
                 :text (zlib-error-message gz))
        x))))

(defun gz-read-string (gz buffer n)
  (cond ((not (gz-open-p gz))
         (error 'zlib-io-error :text "attempted to read from a closed stream"))
        ((gz-eof-p gz)
         0)
        (t
         (let ((x (gzgets (gz-ptr gz) buffer (1+ n))))
           (if (= -1 x)
               (error 'zlib-io-error :errno unix-ffi:*c-error-number*
                      :text (zlib-error-message gz))
             x)))))

(defun gz-write-string (gz buffer)
  (unless (gz-open-p gz)
    (error 'zlib-io-error :text "attempted to write to a closed stream"))
  (let ((n (gzputs (gz-ptr gz) buffer)))
    (if (= -1 n)
        (error 'zlib-io-error :errno unix-ffi:*c-error-number*
               :text (zlib-error-message gz))
      n)))

(defun gz-read-byte (gz )
  (cond ((not (gz-open-p gz))
         (error 'zlib-io-error :text "attempted to read from a closed stream"))
        ((gz-eof-p gz)
         :eof)
        (t
         (let ((b (gzgetc (gz-ptr gz))))
           (if (= -1 b)
               (error 'zlib-io-error :errno unix-ffi:*c-error-number*
                      :text (zlib-error-message gz))
             b)))))

(defun gz-write-byte (gz byte)
  (unless (gz-open-p gz)
    (error 'zlib-io-error :text "attempted to write to a closed stream"))
  (let ((b (gzputc (gz-ptr gz) byte)))
    (if (= -1 b)
        (error 'zlib-io-error :errno unix-ffi:*c-error-number*
               :text (zlib-error-message gz))
      b)))

(defun zlib-error-message (gz)
  "Returns a zlib error message string relevant to GZ."
  (let ((msg (with-foreign-pointer (ptr (foreign-type-size :int))
               (gzerror (gz-ptr gz) ptr))))
    (if (string= "" msg)
        "no error message available"
      msg)))

(defun gunzip (in out)
  (with-open-file (stream out :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (let ((gz (gz-open in))
          (x (1- (expt 2 16))))
      (loop
         with buffer = (make-array x :element-type '(unsigned-byte 8))
         for n = (gz-read gz buffer x)
         sum n into num-bytes
         do (write-sequence buffer stream :end n)
         until (gz-eof-p gz)
         finally (return num-bytes)))))
