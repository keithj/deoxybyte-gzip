
(in-package :gz)

(defun stream-gzip (in-filespec out-filespec)
  (let ((seq (make-array 65535 :element-type 'octet)))
    (with-open-file (stream in-filespec :element-type 'octet)
      (with-open-gzip (gz out-filespec :direction :output
                          :compression 6)
        (loop
           for n = (read-sequence seq stream)
           while (plusp n)
           do (stream-write-sequence gz seq 0 n))))))

(defun stream-gunzip (in-filespec out-filespec)
  (let ((seq (make-array 65535 :element-type 'octet)))
    (with-open-file (stream out-filespec :element-type 'octet
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :overwrite)
      (with-open-gzip (gz in-filespec :direction :input)
        (loop
           for n = (stream-read-sequence gz seq)
           while (plusp n)
           do (write-sequence seq stream :end n))))))
