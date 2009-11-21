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
  (macrolet ((define-copy-op (seq-type seq-accessor
                                       &key (speed 1) (safety 2))
               `(let ((seq-offset start))
                  (declare (optimize (speed ,speed) (safety ,safety)))
                  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
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
                                           (gz-read gz buffer
                                                    (min num-to-write
                                                         +byte-buffer-size+)))))
                     finally (return num-written)))))
    (let ((end (or end (length seq))))
      (with-slots (gz buffer)
          stream
        (let* ((num-to-write (- end start))
               (num-buffered (gz-read gz buffer (min num-to-write
                                                     +byte-buffer-size+)))
               (num-written 0))
          (declare (type fixnum num-to-write num-written)
                   (type byte-buffer-index num-buffered))
          (typecase seq
            ((simple-array (unsigned-byte 8) (*))
             (define-copy-op (simple-array (unsigned-byte 8) (*)) aref
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
             (define-copy-op sequence elt))))))))

(defmethod stream-clear-output ((stream gzip-output-stream))
  nil)

(defmethod stream-finish-output ((stream gzip-output-stream))
  (gz-flush (slot-value stream 'gz) :finish))

(defmethod stream-force-output ((stream gzip-output-stream))
  (gz-flush (slot-value stream 'gz) :flush-full))

(defmethod stream-write-sequence ((stream gzip-output-stream)
                                  (seq sequence) &optional (start 0) end)
  (macrolet ((define-copy-op (seq-type seq-accessor
                                       &key (speed 1) (safety 2))
               `(let ((seq-offset start))
                  (declare (optimize (speed ,speed) (safety ,safety)))
                  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
                           (type ,seq-type seq))
                  (loop
                     while (plusp num-to-write)
                     for num-buffered = (min num-to-write +byte-buffer-size+)
                     for n of-type byte-buffer-index =
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
    (with-slots (gz buffer)
        stream
      (let* ((end (or end (length seq)))
             (num-to-write (- end start))
             (num-written 0))
        (declare (type fixnum num-to-write num-written))
        (typecase seq
          ((simple-array (unsigned-byte 8) (*))
           (define-copy-op (simple-array (unsigned-byte 8) (*)) aref
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

(defmethod stream-read-byte ((stream gzip-input-stream))
  (gz-read-byte (slot-value stream 'gz)))

(defmethod stream-write-byte ((stream gzip-output-stream) (byte fixnum))
  (gz-write-byte (slot-value stream 'gz) byte))
