(in-package :quek)

(defun binding-value (var bindings)
  (cdr (assoc var bindings)))

(defun binding-variables (bindings)
  (mapcar #'car bindings))

(defun binding-values (bindings)
  (mapcar #'cdr bindings))

(defun match-variable (pattern input bindings)
  (aif (assoc pattern bindings)
       (values (equal (cdr it) input) bindings)
       (values t (acons pattern input bindings))))

(defun match-p (pattern input)
  (%match-p pattern input nil))

(defun %match-p (pattern input bindings)
  (cond ((null pattern) (values (null input) bindings))
        ((keywordp pattern)
         (values (eq pattern input) bindings))
        ((symbolp pattern)
         (if (string= "_" pattern)
             (values t bindings)
             (match-variable pattern input bindings)))
        ((atom pattern)                 ; ex 123, "abc"
         (values (equal pattern input) bindings))
        ;; ここから pattern は consp => t ね。
        ((eq 'quote (car pattern))      ; ex 'foo
         (values (equal (cadr pattern) input) bindings))
        ((consp input)
         (multiple-value-bind (ok bindings)
             (%match-p (car pattern) (car input) bindings)
           (if ok
               (%match-p (cdr pattern) (cdr input) bindings)
               (values nil bindings))))
        (t (values nil bindings))))

(match-p '(:k a 'a 12 _ (a b) . rest) '(:k 1 a 12 X (1 2) r e s t))
;;=> T
;;   ((REST R E S T) (B . 2) (A . 1))

