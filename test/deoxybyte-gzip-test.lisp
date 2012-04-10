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

(in-package :uk.co.deoxybyte-gzip-test)

(defun binary-file= (x y)
  (with-open-file (s1 x :element-type 'octet)
    (with-open-file (s2 y :element-type 'octet)
      (let ((v1 (make-array (file-length s1) :element-type 'octet))
            (v2 (make-array (file-length s2) :element-type 'octet)))
        (read-sequence v1 s1)
        (read-sequence v2 s2)
        (equalp v1 v2)))))

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
        (seq (make-array 4096 :element-type 'octet)))
    (with-open-file (stream out :direction :output :element-type 'octet)
      (loop
         with gz = (gzip-open in)
         for n = (stream-read-sequence gz seq 0 4096) ; use start/end
                                                      ; args for
                                                      ; Lispworks
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
        (seq (make-array 4096 :element-type 'octet)))
    (with-open-file (stream in :element-type 'octet)
      (loop
         with gz = (gzip-open out :direction :output)
         for n = (read-sequence seq stream)
         sum n into num-bytes
         while (plusp n)
         do (stream-write-sequence gz seq 0 n)
         finally (progn
                   (ensure (= 120657 num-bytes))
                   (close gz))))
    (ensure (probe-file out))
    (delete-file out)))

;; TODO -- factor the common parts out of these tests
(addtest (deoxybyte-gzip-tests) compress/1
  (let* ((in (merge-pathnames "data/lorem.txt"))
         (data (make-array 1024 :element-type 'octet))
         (len (+ (ceiling (+ 1024 (/ 1024 1000))) 12))
         (comp (make-array len :element-type 'octet)))
    (with-open-file (stream in)
      (loop
         for i from 0 below (length data)
         do (setf (aref data i) (char-code (read-char stream t)))))
    (ensure-error
      (compress data comp :source-start -1))
    (ensure-error
      (compress data comp :source-start 9999))
    (ensure-error
      (compress data comp :source-start 100 :source-end 90))
    (ensure-error
      (compress data comp :dest-start -1))
    (ensure-error
      (compress data comp :dest-start 9999))))

(addtest (deoxybyte-gzip-tests) uncompress/1
  (let* ((in (merge-pathnames "data/lorem.txt"))
         (data (make-array 1024 :element-type 'octet))
         (len (+ (ceiling (+ 1024 (/ 1024 1000))) 12))
         (comp (make-array len :element-type 'octet))
         (uncomp (make-array len :element-type 'octet)))
    (with-open-file (stream in)
      (loop
         for i from 0 below (length data)
         do (setf (aref data i) (char-code (read-char stream t)))))
    (ensure-error
      (uncompress data comp :source-start -1))
    (ensure-error
      (uncompress data comp :source-start 9999))
    (ensure-error
      (uncompress data comp :source-start 100 :source-end 90))
    (ensure-error
      (uncompress data comp :dest-start -1))
    (ensure-error
      (uncompress data comp :dest-start 9999))))

(addtest (deoxybyte-gzip-tests) compress/uncompress/1
  (let* ((in (merge-pathnames "data/lorem.txt"))
         (data (make-array 1024 :element-type 'octet))
         (len (+ (ceiling (+ 1024 (/ 1024 1000))) 12))
         (comp (make-array len :element-type 'octet))
         (uncomp (make-array len :element-type 'octet)))
    (with-open-file (stream in)
      (loop
         for i from 0 below (length data)
         do (setf (aref data i) (char-code (read-char stream t)))))
    (compress data comp)
    (uncompress comp uncomp)
    (ensure (equalp data (subseq uncomp 0 1024)))))

(addtest (deoxybyte-gzip-tests) compress/uncompress/2
  (let* ((in (merge-pathnames "data/lorem.txt"))
         (data (make-array 1024 :element-type 'octet))
         (len (+ (ceiling (+ 1024 (/ 1024 1000))) 12))
         (comp (make-array len :element-type 'octet))
         (uncomp (make-array 1024 :element-type 'octet)))
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

(addtest (deoxybyte-gzip-tests) deflate-stream/1
   (let ((in (merge-pathnames "data/lorem.txt"))
         (out (merge-pathnames "data/lorem.tmp.dfl"))
         (test (merge-pathnames "data/lorem.txt.dfl")))
     (with-open-file (s1 in :element-type 'octet)
       (with-open-file (s2 out :element-type 'octet :direction :output
                           :if-exists :supersede)
         (deflate-stream s1 s2)))
     (ensure (binary-file= out test))
     (delete-file out)))

(addtest (deoxybyte-gzip-tests) inflate-stream/1
   (let ((in (merge-pathnames "data/lorem.txt.dfl"))
         (out (merge-pathnames "data/lorem.tmp.txt"))
         (test (merge-pathnames "data/lorem.txt")))
     (with-open-file (s1 in :element-type 'octet)
       (with-open-file (s2 out :element-type 'octet :direction :output
                           :if-exists :supersede)
         (inflate-stream s1 s2)))
     (ensure (binary-file= out test))
     (delete-file out)))

(addtest (deoxybyte-gzip-tests) deflate/inflate-vector/1
  (let ((in (make-array 1024 :element-type 'octet
                        :initial-contents (loop
                                             repeat 1024
                                             collect (random 255))))
         (out (make-array 1384 :element-type 'octet)))
    (multiple-value-bind (vec count)
        (deflate-vector in out)
      (ensure (equalp out vec))
      (ensure (= 1024 count)))
    (multiple-value-bind (vec compressed count)
        (inflate-vector out (make-array 1024 :element-type 'octet
                                        :initial-element 0))
      (ensure (equalp in vec))
      (ensure (equalp 1024 count)))))
