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

(defconstant +byte-buffer-size+ (1- (expt 2 16)))

(deftype byte-buffer ()
  `(simple-array (unsigned-byte 8) (,+byte-buffer-size+)))

(deftype byte-buffer-index ()
  `(integer 0 ,+byte-buffer-size+))

(defclass gzip-stream (fundamental-stream)
  ((gz :initarg :gz
       :documentation "The gzip handle.")
   (buffer :initarg :buffer
           :initform (make-array +byte-buffer-size+
                                 :element-type '(unsigned-byte 8)
                                 :initial-element (char-code #\.)))
   (buffer-offset :initform 0)
   (num-buffered :initform 0)
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

(defun make-gzip-stream (filespec &key (direction :input) compression)
  (make-instance (ecase direction
                   (:input 'gzip-input-stream)
                   (:output 'gzip-output-stream))
                 :gz (gz-open filespec :direction direction
                              :compression compression)))

(defmethod stream-element-type ((stream gzip-stream))
  '(unsigned-byte 8))

(defmethod open-stream-p ((stream gzip-stream))
  (slot-value stream 'open-stream-p))

(defmethod close ((stream gzip-stream) &key abort)
  (declare (ignore abort))
  (with-slots (open-stream-p gz)
      stream
    (when open-stream-p
      (setf open-stream-p nil)
      (gz-close gz))))

(defmethod stream-file-position ((stream gzip-stream) &optional position-spec)
  (with-slots (gz)
      stream
    (if position-spec
        (gz-seek gz position-spec)
      (gz-tell gz))))

(defmethod stream-clear-input ((stream gzip-input-stream))
  nil)

(defmethod stream-read-sequence ((stream gzip-input-stream) (seq sequence)
                                 &optional (start 0) end)
  (let ((end (or end (length seq))))
    (with-slots (gz buffer buffer-offset num-buffered)
        stream
      (flet ((fill-buffer (n)
               (setf buffer-offset 0
                     num-buffered (gz-read gz buffer
                                           (min n +byte-buffer-size+)))
               num-buffered)
             (buffer-empty-p ()
               (= buffer-offset num-buffered)))
        (let* ((m (- end start))
               (n m))
          (if (zerop (fill-buffer n))
              0
            (loop
               for i from start below end
               do (cond ((and (buffer-empty-p) (zerop (fill-buffer n)))
                         (return (- m n)))
                        (t
                         (setf (elt seq i) (aref buffer buffer-offset))
                         (incf buffer-offset)
                         (decf n)))
               finally (return (- m n)))))))))

(defmethod stream-clear-output ((stream gzip-output-stream))
  nil)

(defmethod stream-finish-output ((stream gzip-output-stream))
  (gz-flush (slot-value stream 'gz) :finish))

(defmethod stream-force-output ((stream gzip-output-stream))
  (gz-flush (slot-value stream 'gz) :flush-full))

(defmethod stream-write-sequence ((stream gzip-output-stream)
                                  (seq sequence) &optional (start 0) end)
  (with-slots (gz buffer)
      stream
    (let* ((end (or end (length seq)))
           (num-bytes (- end start))
           (num-to-write num-bytes)
           (seq-offset start))
      (flet ((fill-buffer (n)
               (let ((num-buffered (min n +byte-buffer-size+)))
                 (loop
                    for i from 0 below num-buffered
                    do (progn
                         (setf (aref buffer i) (elt seq seq-offset))
                         (incf seq-offset))
                    finally (return num-buffered)))))
        (loop
           while (plusp num-to-write)
           do (let ((num-buffered (fill-buffer num-to-write)))
                (gz-write gz buffer num-buffered)
                (decf num-to-write num-buffered))
           finally (return num-bytes))))))

(defmethod stream-read-byte ((stream gzip-input-stream))
  (gz-read-byte (slot-value stream 'gz)))

(defmethod stream-write-byte ((stream gzip-output-stream) (byte fixnum))
  (gz-write-byte (slot-value stream 'gz) byte))
