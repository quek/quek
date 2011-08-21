(in-package :quek)

(defun |#"-reader"| (stream)
  (funcall (get-macro-character #\") stream #\"))

(defun |#"""-reader"""| (stream)
  #1=(read-char stream t nil t)
  (with-output-to-string (out)
    (loop for c1 = #1# then c2
          for c2 = #1# then c3
          for c3 = #1# then #1#
          until (and (char= #\" c1 c2 c3)
                     (char/= #\" (peek-char nil stream nil #\? t)))
          do (write-char c1 out))))

(defun |#"-parser| (s)
  (macrolet ((peek-equal (c)
               `(equal ,c (peek-char nil in nil nil))))
    (let* ((args nil)
           (format
            (with-output-to-string (out)
              (with-input-from-string (in s)
                (loop for c = #1=(read-char in nil nil)
                      while c
                      if (and (equal #\# c) (peek-equal #\,))
                        do (progn
                             #1#
                             (write-string "~a" out)
                             (push (read-preserving-whitespace in) args)
                             (when (peek-equal #\,)
                               #1#))
                      ;;else if (char= #\~ c)
                      ;;       do (write-string "~~" out)
                      else
                        do (write-char c out))))))
      `(format nil ,format ,@(reverse args)))))

(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (|#"-parser|
   (if (equal #\" (peek-char nil stream t nil t))
     (progn
       (read-char stream)
       (if (equal #\" (peek-char nil stream nil nil t))
         (|#"""-reader"""| stream)
         ""))
     (|#"-reader"| stream))))

;; (set-dispatch-macro-character #\# #\" '|#"-reader|)

(named-readtables:defreadtable |#"|
  (:merge :common-lisp)
  (:dispatch-macro-char #\# #\" '|#"-reader|))

(defun string-start-p (string start &key (test #'char=))
  (let ((p (mismatch string start :test test)))
    (if p
        (= p (length start))
        t)))