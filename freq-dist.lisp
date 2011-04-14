(in-package :quek)

(defclass freq-dist ()
  ((hash :initarg :hash)
   (items :initarg :items)))

(defmethod items ((self freq-dist))
  (slot-value self 'items))

(defmethod freq ((self freq-dist) key)
  (let ((total (collect-sum (cdr (scan (slot-value self 'items))))))
    (/ (gethash key (slot-value self 'hash))
       total)))

(defun freq-dist (data)
  (let ((hash (make-hash-table :test #'equal)))
    (iterate ((i (scan data)))
      (incf (gethash i hash 0)))
    (make-instance 'freq-dist
                   :hash hash
                   :items (sort (collect (multiple-value-bind (k v)
                                             (scan-hash hash)
                                           (cons k v)))
                                #'>= :key #'cdr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *data*
  (collect (#M(lambda (x)
                x
                (random 10))
              (scan-range :upto 1000))))

(defparameter *freq-dist* (freq-dist *data*))

(items *freq-dist*)
;;=> ((1 . 127) (6 . 116) (4 . 107) (7 . 105) (5 . 97) (2 . 96) (0 . 94) (9 . 89)
;;    (3 . 88) (8 . 82))

(freq *freq-dist* 3)
;;=> 8/91

