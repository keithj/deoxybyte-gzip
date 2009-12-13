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

(in-package :uk.co.deoxybyte-gzip-test)

(deftestsuite deoxybyte-gzip-tests ()
  ())

(addtest (deoxybyte-gzip-tests) gzip/1
  (let ((in (merge-pathnames "data/lorem.txt"))
        (out (merge-pathnames "data/lorem-test.txt.gz")))
    (multiple-value-bind (name length)
        (gzip in out)
      (ensure (equal name out))
      (ensure (= 120657 length))
      (ensure (probe-file out)))
    (delete-file out)))

(addtest (deoxybyte-gzip-tests) gunzip/1
  (let ((in (merge-pathnames "data/lorem.txt.gz"))
        (out (merge-pathnames "data/lorem-test.txt")))
    (multiple-value-bind (name length)
        (gunzip in out)
      (ensure (equal name out))
      (ensure (= 120657 length))
      (ensure (probe-file out)))
    (delete-file out)))

(addtest (deoxybyte-gzip-tests) gzip-input-stream/1
  (let ((in (merge-pathnames "data/lorem.txt.gz"))
        (out (merge-pathnames "data/lorem-test.txt"))
        (seq (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-file (stream out :direction :output
                            :element-type '(unsigned-byte 8))
      (loop
         with gz = (make-gzip-stream in)
         for n = (stream-read-sequence gz seq)
         sum n into num-bytes
         while (plusp n)
         do (write-sequence seq stream :end n)
         finally (progn
                   (ensure (= 120657 num-bytes))
                   (close gz))))
    (ensure (probe-file out))
    (delete-file out)))

(addtest (deoxybyte-gzip-tests) gzip-output-stream/1
  (let ((in (merge-pathnames "data/lorem.txt"))
        (out (merge-pathnames "data/lorem-test.txt.gz"))
        (seq (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-file (stream in :element-type '(unsigned-byte 8))
      (loop
         with gz = (make-gzip-stream out :direction :output)
         for n = (read-sequence seq stream)
         sum n into num-bytes
         while (plusp n)
         do (stream-write-sequence gz seq 0 n)
         finally (progn
                   (ensure (= 120657 num-bytes))
                   (close gz))))
    (ensure (probe-file out))
    (delete-file out)))

(addtest (deoxybyte-gzip-tests) compress/uncompress/1
  (let* ((in (merge-pathnames "data/lorem.txt"))
         (data (make-array 1024 :element-type '(unsigned-byte 8)))
         (len (+ (ceiling (+ 1024 (/ 1024 1000))) 12))
         (comp (make-array len :element-type '(unsigned-byte 8)))
         (uncomp (make-array len :element-type '(unsigned-byte 8))))
    (with-open-file (stream in)
      (loop
         for i from 0 below (length data)
         do (setf (aref data i) (char-code (read-char stream t)))))
    (compress data comp)
    (uncompress comp uncomp)
    (ensure (equalp data (subseq uncomp 0 1024)))))

(addtest (deoxybyte-gzip-tests) compress/uncompress/2
  (let* ((in (merge-pathnames "data/lorem.txt"))
         (data (make-array 1024 :element-type '(unsigned-byte 8)))
         (len (+ (ceiling (+ 1024 (/ 1024 1000))) 12))
         (comp (make-array len :element-type '(unsigned-byte 8)))
         (uncomp (make-array 1024 :element-type '(unsigned-byte 8))))
    (with-open-file (stream in)
      (loop
         for i from 0 below (length data)
         do (setf (aref data i) (char-code (read-char stream t)))))
    (loop
       for start from 0 below 500 by 10
       do (loop
             for end from 500 below 1000 by 10
             do (progn
                  (fill comp 0)
                  (fill uncomp 0)
                  (compress data comp :source-start start :source-end end)
                  (uncompress comp uncomp)
                  (ensure (equalp (subseq data start end)
                                  (subseq uncomp 0 (- end start)))
                          :report "expected ~a but got ~a"
                          :arguments ((subseq data start end)
                                      (subseq uncomp 0 (- end start)))))))))
