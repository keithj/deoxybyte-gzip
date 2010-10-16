;;;
;;; Copyright (C) 2009-2010 Keith James. All rights reserved.
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

(defconstant +octet-buffer-size+ (1- (expt 2 16)))

(deftype octet-buffer ()
  `(simple-array (unsigned-byte 8) (,+octet-buffer-size+)))

(deftype octet-buffer-index ()
  `(integer 0 ,+octet-buffer-size+))

(defclass gzip-stream (fundamental-stream)
  ((gz :initarg :gz
       :documentation "The gzip handle.")
   (buffer :initarg :buffer
           :initform (make-array +octet-buffer-size+
                                 :element-type 'octet
                                 :initial-element (char-code #\.)))
   (open-stream-p :initform t))
  (:documentation "A gzip stream capable of reading or writing
compressed data."))

(defclass gzip-input-stream (gzip-stream
                             fundamental-binary-input-stream)
  ()
  (:documentation "A stream that reads bytes from a compressed
stream."))

(defclass gzip-output-stream (gzip-stream
                              fundamental-binary-output-stream)
  ()
  (:documentation "A stream that writes characters to a compressed
stream."))

(defmacro with-open-gzip ((var filespec &rest args) &body body)
  `(let ((,var (gzip-open ,filespec ,@args)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var
         (close ,var)))))

(defun gzip-open (filespec &key (direction :input)
                  (compression +z-default-compression+))
  "Opens a gzip stream for FILESPEC.

Key:

- direction (symbol): One of :input (the default) or :output
- compression (integer): The zlib compression level, if compressing.
  An integer between 0 and 9, inclusive.

Returns:

- A {defclass gzip-stream}"
  (make-instance (ecase direction
                   (:input 'gzip-input-stream)
                   (:output 'gzip-output-stream))
                 :gz (gz-open filespec :direction direction
                              :compression compression)))

(defmethod stream-element-type ((stream gzip-stream))
  'octet)

(defmethod open-stream-p ((stream gzip-stream))
  (slot-value stream 'open-stream-p))

(defmethod close ((stream gzip-stream) &key abort)
  (declare (ignore abort))
  (with-slots (open-stream-p gz)
      stream
    (when open-stream-p
      (setf open-stream-p nil)
      (gz-close gz))))

(defmethod stream-file-position ((stream gzip-stream) &optional position)
  (with-slots (gz)
      stream
    (if position
        (gz-seek gz position)
        (gz-tell gz))))

(defmethod stream-clear-input ((stream gzip-input-stream))
  nil)

(defmethod stream-clear-output ((stream gzip-output-stream))
  nil)

(defmethod stream-finish-output ((stream gzip-output-stream))
  (gz-flush (slot-value stream 'gz) :finish))

(defmethod stream-force-output ((stream gzip-output-stream))
  (gz-flush (slot-value stream 'gz) :flush-full))

(defmethod stream-read-byte ((stream gzip-input-stream))
  (gz-read-byte (slot-value stream 'gz)))

#+(or :sbcl :ccl)
(defmethod stream-read-sequence ((stream gzip-input-stream)
                                 (seq sequence) &optional (start 0) end)
  (%stream-read-sequence stream seq start end))

#+:ccl
(defmethod stream-read-vector ((stream gzip-input-stream)
                               (vec vector) &optional (start 0) end)
  (%stream-read-sequence stream vec start end))

#+:lispworks
(defmethod stream-read-sequence ((stream gzip-output-stream)
                                 (seq sequence) start end)
  (%stream-read-sequence stream seq start end))

(defmethod stream-write-byte ((stream gzip-output-stream) (byte fixnum))
  (gz-write-byte (slot-value stream 'gz) byte))

#+(or :sbcl :ccl)
(defmethod stream-write-sequence ((stream gzip-output-stream)
                                  (seq sequence) &optional (start 0) end)
  (%stream-write-sequence stream seq start end))

#+:ccl
(defmethod stream-write-vector ((stream gzip-output-stream)
                                (vec vector) &optional (start 0) end)
  (%stream-write-sequence stream vec start end))

#+:lispworks
(defmethod stream-write-sequence ((stream gzip-output-stream)
                                 (seq sequence) start end)
  (%stream-write-sequence stream seq start end))

(declaim (inline %stream-read-sequence))
(defun %stream-read-sequence (stream seq &optional (start 0) end)
  (macrolet ((define-copy-op (seq-type seq-accessor &key (speed 1) (safety 2))
               `(let ((seq-offset start))
                  (declare (optimize (speed ,speed) (safety ,safety)))
                  (declare (type simple-octet-vector buffer)
                           (type ,seq-type seq))
                  (loop
                     while (plusp num-buffered)
                     do (loop
                           for i from 0 below num-buffered
                           do (progn
                                (setf (,seq-accessor seq seq-offset)
                                      (aref buffer i))
                                (incf seq-offset))
                           finally (progn
                                     (incf num-written num-buffered)
                                     (decf num-to-write num-buffered)
                                     (setf num-buffered
                                           (gz-read
                                            gz buffer (min
                                                       num-to-write
                                                       +octet-buffer-size+)))))
                     finally (return num-written)))))
    (let ((end (or end (length seq)))
          (gz (slot-value stream 'gz)) 
          (buffer (slot-value stream 'buffer))) ; CCL 1.4 with-slots bug?
      (let* ((num-to-write (- end start))
             (num-buffered (gz-read gz buffer (min num-to-write
                                                   +octet-buffer-size+)))
             (num-written 0))
        (declare (type fixnum num-to-write num-written)
                 (type octet-buffer-index num-buffered))
        (typecase seq
          (simple-octet-vector
           (define-copy-op simple-octet-vector aref
             :speed 3 :safety 0))
          (simple-vector
           (define-copy-op simple-vector svref
             :speed 3 :safety 0))
          ((simple-array * (*))
           (define-copy-op (simple-array * (*)) aref))
          (list
           (define-copy-op list elt
             :speed 3 :safety 0))
          (t
           (define-copy-op sequence elt)))))))

(declaim (inline %stream-write-sequence))
(defun %stream-write-sequence (stream seq &optional (start 0) end)
  (macrolet ((define-copy-op (seq-type seq-accessor &key (speed 1) (safety 2))
               `(let ((seq-offset start))
                  (declare (optimize (speed ,speed) (safety ,safety)))
                  (declare (type simple-octet-vector buffer)
                           (type ,seq-type seq))
                  (loop
                     while (plusp num-to-write)
                     for num-buffered = (min num-to-write +octet-buffer-size+)
                     for n of-type octet-buffer-index =
                       (loop
                          for i from 0 below num-buffered
                          do (progn
                               (setf (aref buffer i)
                                     (,seq-accessor seq seq-offset))
                               (incf seq-offset))
                          finally (return
                                    (gz-write gz buffer num-buffered)))
                     do (progn
                          (incf num-written n)
                          (if (< n num-buffered)
                              (setf num-to-write 0)
                              (decf num-to-write n)))
                     finally (return num-written)))))
    (let ((end (or end (length seq)))
          (gz (slot-value stream 'gz))
          (buffer (slot-value stream 'buffer)))  ; CCL 1.4 with-slots bug?
      (let ((num-to-write (- end start))
            (num-written 0))
        (declare (type fixnum num-to-write num-written))
        (typecase seq
          (simple-octet-vector
           (define-copy-op simple-octet-vector aref
             :speed 3 :safety 0))
          (simple-vector
           (define-copy-op simple-vector svref
             :speed 3 :safety 0))
          ((simple-array * (*))
           (define-copy-op (simple-array * (*)) aref))
          (list
           (define-copy-op list elt
             :speed 3 :safety 0))
          (t
           (define-copy-op sequence elt)))))))
