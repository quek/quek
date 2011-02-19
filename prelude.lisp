(in-package :quek)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defun str (&rest args)
  (format nil "~{~a~}" (mapcar (lambda (x) (or x "")) args)))

(defun sym (&rest args)
  (values (intern (apply #'str args))))

(defun symbol-head-p (symbol head)
  (and (symbolp symbol)
       (let ((p (mismatch (symbol-name symbol) head
                          :test #'char-equal)))
         (if p
             (= p (length head))
             t))))

(defun collect-symbols (form pred)
  (sort
   (remove-duplicates
    (collect
        (choose-if pred
                   (scan-lists-of-lists-fringe form))))
   #'string<=
   :key #'symbol-name))

(defun collect-argument-symbols (form)
  (collect-symbols form (lambda (x) (symbol-head-p x "_"))))

(defmacro ^ (&body body)
  (let* ((syms (collect-argument-symbols body))
         (rest (find '_rest syms :key #'symbol-name :test #'string=)))
    (when rest
      (setq syms (append (remove rest syms) `(&rest ,rest))))
    (if (consp (car body))
        `(lambda ,syms
           ,@body)
        `(lambda ,syms
           ,body))))

(defun x!-symbol-p (c s)
  (symbol-head-p s (format nil "~c!" c)))

(defmacro p (&body body)
  `(prog1
       ,@(mapcar (lambda (x)
                   `(let ((v ,x))
                      (format t "~&~30s ; => ~s" ',x v)
                      v))
                    body)))

(defmacro with-ca/dr (var &body body)
  (alexandria:once-only (var)
    `(let ((car (car ,var))
           (cdr (cdr ,var)))
       ,@body)))
