;;;
;;; Copyright (c) 2010-2011 Keith James. All rights reserved.
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

(defclass gzip-line-input-stream (line-input-stream gzip-stream)
  ((buffer :initarg :buffer
           :initform (make-array +octet-buffer-size+
                                 :element-type 'octet :initial-element 0)
           :documentation "The buffer from which lines are read.")
   (eol-code :initarg :eol-code
             :initform (char-code #\Newline)
             :documentation "The end of line character code. If two
characters are used, this is the first of the pair.")
   (num-bytes :initform 0
              :documentation "The number of bytes that were read into
the buffer from the stream.")
   (offset :initform 0
           :documentation "The offset in the byte buffer from which
the next byte is to be read.")
   (filespec :initarg :filespec)
   (gz :initarg :gz)))

(defmethod stream-element-type ((stream gzip-line-input-stream))
  'string)

(defmethod stream-file-position ((stream gzip-line-input-stream)
                                 &optional position)
  (with-accessors ((line-stack line-stack-of))
      stream
    (with-slots (gz)
        stream
      (cond (position
             (setf line-stack ())
             (gz-seek gz position))
            (t
             (let ((buffered-chars (loop
                                      for line in line-stack
                                      sum (1+ (length line))))) ; eol
               (- (gz-tell gz) buffered-chars)))))))

;;; Deoxybyte Gray streams methods
(defmethod stream-delete-file ((stream gzip-line-input-stream))
  (delete-file (slot-value stream 'filespec)))

(defmethod stream-clear-input ((stream gzip-line-input-stream))
  (with-accessors ((line-stack line-stack-of))
      stream
    (setf line-stack ())))

#+(or :sbcl :ccl)
(defmethod stream-read-sequence ((stream gzip-line-input-stream)
                                 sequence &optional (start 0) end)
  (let ((end (or end (length sequence))))
    (%line-stream-read-sequence stream sequence start end)))

#+:lispworks
(defmethod stream-read-sequence ((stream gzip-line-input-stream)
                                 sequence start end)
  (%line-stream-read-sequence stream sequence start end))

(defmethod stream-read-line ((stream gzip-line-input-stream))
  (declare (optimize (speed 3)))
  (flet ((build-string (chunks) ; this is 3x faster than with-output-to-string
           (let ((length (if (endp (rest chunks))
                             (length (the simple-octet-vector (first chunks)))
                             (reduce #'+ chunks :key #'length))))
              (declare (type vector-index length))
              (loop
                 with line = (make-array length :element-type 'base-char)
                 with offset of-type vector-index = 0
                 for chunk of-type simple-octet-vector in chunks
                 for clength = (length chunk)
                 do (unless (zerop clength)
                      (copy-vector chunk 0 clength
                                   line offset #'code-char)
                      (incf offset clength))
                 finally (return line)))))
    (with-accessors ((line-stack line-stack-of))
        stream
      (if (null line-stack)
          (multiple-value-bind (chunks has-eol-p)
              (read-octet-line stream)
            (if (null chunks)
                (values :eof t)
                (values (build-string chunks) has-eol-p)))
          (pop line-stack)))))

(defmethod more-lines-p ((stream gzip-line-input-stream))
  (with-accessors ((line-stack line-stack-of))
      stream
    (with-slots (gz)
        stream
      (or line-stack (not (gz-eof-p gz))))))

(defun %line-stream-read-sequence (stream sequence start end)
  (loop
     with n = 0
     for i from start below end
     for line = (stream-read-line stream)
     until (or (eql :eof line) (= end i))
     do (setf (elt sequence i) line
              n (1+ n))
     finally (return n)))

(defun %line-stream-write-sequence (stream sequence start end)
  (loop
     for i from start below end
     do (write-sequence (elt sequence i) stream)))

(defparameter *empty-chunks* (list (make-array 0 :element-type 'octet)))

(defun read-octet-line (stream)
  "Reads chunks of bytes up to the next newline or end of stream,
returning them in a list. The newline is not included. Returns two
values - a list of chunks and either NIL or T to indicate whether a
terminating newline was missing. When the stream underlying the buffer
is exhausted the list of chunks will be empty."
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (gz buffer offset num-bytes eol-code)
      stream
    (declare (type octet-buffer buffer)
             (type octet-buffer-index offset num-bytes))
    (labels ((buffer-empty-p ()
               (= offset num-bytes))
             (fill-buffer ()
               (setf offset 0
                     num-bytes (gz-read gz buffer +octet-buffer-size+)))
             (find-eol ()
               (let ((eol-pos (position eol-code buffer
                                        :start offset :end num-bytes)))
                 (cond ((and eol-pos (plusp (- eol-pos offset)))
                        ;; There is a newline in the buffer, but not
                        ;; at the zeroth position. Make a chunk up to
                        ;; the newline
                        (let ((chunk (make-array (- eol-pos offset)
                                                 :element-type 'octet
                                                 :initial-element 0)))
                          (replace chunk buffer :start2 offset :end2 eol-pos)
                          (setf offset (1+ eol-pos))
                          (values (list chunk) nil)))
                       ((and eol-pos (zerop (- eol-pos offset)))
                        ;; There is a newline in the buffer at the
                        ;; zeroth position. Make an empty chunk (for
                        ;; sake of consistency)
                        (setf offset (1+ eol-pos))
                        (values nil nil))
                       ((zerop num-bytes)
                        ;; The buffer is empty
                        (values nil t))
                       (t
                        ;; There is no newline in the buffer. Make a
                        ;; chunk and recurse to find the newline
                        (let ((chunk (make-array (- num-bytes offset)
                                                 :element-type 'octet
                                                 :initial-element 0)))
                          (replace chunk buffer :start2 offset :end2 num-bytes)
                          (fill-buffer)
                          (multiple-value-bind (chunks missing-eol-p)
                              (find-eol)
                            (values (cons chunk chunks) missing-eol-p))))))))
      (when (buffer-empty-p)
        (fill-buffer))
      (find-eol))))
