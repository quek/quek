(in-package :quek)

(export '(p
          defmacro!
          with-ca/dr))

(defmacro p (&body body)
  `(prog1
       ,@(mapcar (lambda (x)
                   `(let ((v ,x))
                      (format t "~&~30s ; => ~s" ',x v)
                      v))
                    body)))

(eval-always
  (defmacro defmacro/g! (name args &rest body)
    (let ((syms (collect-symbols body #'g!-symbol-p)))
      `(defmacro ,name ,args
         (let ,(mapcar (lambda (x)
                         `(,x (gensym ,(subseq (symbol-name x) 2))))
                syms)
           ,@body))))

  (defmacro defmacro! (name args &rest body)
    (let* ((os (collect-symbols args #'o!-symbol-p))
           (gs (mapcar #'o!-symbol-to-g!-symbol os)))
      `(defmacro/g! ,name ,args
         `(let ,(mapcar #'list (list ,@gs) (list ,@os))
            ,(progn ,@body)))))
  )


(defmacro! with-ca/dr (o!var &body body)
  `(let ((car (car ,g!var))
         (cdr (cdr ,g!var)))
     ,@body))
