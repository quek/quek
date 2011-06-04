(in-package :quek)

(defclass freq-dist ()
  ((hash :initarg :hash)
   (items :initarg :items)))

(defmethod items ((self freq-dist))
  (slot-value self 'items))

(defmethod freq ((self freq-dist) key)
  (with-slots (hash items) self
    (let ((total (collect-sum (cdr (scan items)))))
      (/ (gethash key hash)
         total))))

(defun freq-dist (&optional data)
  (let ((hash (make-hash-table :test #'equal)))
    (iterate ((i (scan data)))
      (incf (gethash i hash 0)))
    (make-instance 'freq-dist
                   :hash hash
                   :items (sort (collect (multiple-value-bind (k v)
                                             (scan-hash hash)
                                           (cons k v)))
                                #'>= :key #'cdr))))

(defmethod max-key ((self freq-dist))
  (with-slots (items) self
    (caar items)))

(defmethod keys ((self freq-dist))
  (with-slots (items) self
    (collect (car (scan items)))))

(defmethod vals ((self freq-dist))
  (with-slots (items) self
    (collect (cdr (scan items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *data*
  (collect (#M(lambda (x)
                x
                (random 10))
              (scan-range :upto 1000))))

(defparameter *freq-dist* (freq-dist *data*))

(items *freq-dist*)
;;=> ((8 . 124) (5 . 110) (4 . 104) (6 . 103) (0 . 103) (2 . 102) (9 . 100) (7 . 86)
;;    (3 . 86) (1 . 83))

(freq *freq-dist* 3)
;;=> 86/1001

(max-key *freq-dist*)
;;=> 8

(keys *freq-dist*)
;;=> (8 5 4 6 0 2 9 7 3 1)

(vals *freq-dist*)
;;=> (124 110 104 103 103 102 100 86 86 83)

