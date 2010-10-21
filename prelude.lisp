(in-package :quek)

(export '(eval-always
          string+
          symbol+
          symbol-head-p
          ^))

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defun string+ (&rest args)
  (format nil "~{~a~}" (mapcar (lambda (x) (or x "")) args)))

(defun symbol+ (&rest args)
  (values (intern (apply #'string+ args))))

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

(defvar *original-readtable* nil)

(defun |#{-reader| (stream char)
   (declare (ignore char))
   `(^ ,@(read-delimited-list #\} stream t)))

(defun enable-lambda-reader ()
  (unless *original-readtable*
    (setf *original-readtable* *readtable*
          *readtable* (copy-readtable nil)))
  (set-macro-character #\{ '|#{-reader|)
  (set-macro-character #\} (get-macro-character #\))))

(defun disable-lambda-reader ()
  (when *original-readtable*
    (setf *readtable* *original-readtable*
          *original-readtable* nil)))

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((form (funcall (get-macro-character #\`) stream nil))
         (args (collect-argument-symbols form)))
    `(lambda ,args
       ,form)))

;;(set-dispatch-macro-character #\# #\` #'|#`-reader|)

(defun x!-symbol-p (c s)
  (symbol-head-p s (format nil "~c!" c)))

(defun g!-symbol-p (s)
  (x!-symbol-p #\G s))

(defun o!-symbol-p (s)
  (x!-symbol-p #\O s))

(defun o!-symbol-to-g!-symbol (s)
  (symbol+ "G!" (subseq (symbol-name s) 2)))
