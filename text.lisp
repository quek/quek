(in-package :quek)

(export '(with-line))

(defmacro! with-line ((var
                       (in &rest in-opt)
                       (&optional out &rest out-opt)
                       &optional (line-number-var nil line-number-var-p))
                      &body body)
  (let ((out-opt (append out-opt
                         (unless (getf out-opt :direction)
                           '(:direction :output))
                         (unless (getf out-opt :if-exists)
                           '(::if-exists :supersede))))
        (body `(loop for ,var = (read-line ,g!in nil)
                     ,@(when line-number-var-p
                         `(for ,line-number-var from 1))
                     while ,var
                     do (write-line (progn ,@body) ,g!out))))
    `(with-open-file (,g!in ,in ,@in-opt)
       (aetypecase ,out
         ((or string pathname)
          (with-open-file (,g!out it ,@out-opt)
            ,body))
         (stream
          (let ((,g!out it))
            ,body))
         (null
          (let ((,g!out *standard-output*))
            ,body))))))

#|
(with-line (line ("text.lisp" :external-format :utf8) () line-number)
  (string+ line-number ":" #\Tab (ppcre:regex-replace-all "with" line "羊")))

(with-line (line ("text.lisp" :external-format :utf8)
                 ("/tmp/b.lisp" :external-format :utf8)
                 line-number)
    (print line)
    (string+ line-number ":" #\Tab (ppcre:regex-replace-all "with" line "羊")))

(let ((line-number 0))
  (with-line (line ("text.lisp" :external-format :utf8)
                   ("/tmp/b.lisp" :external-format :utf8))
    (incf line-number)
    (format nil "~d: ~a" line-number line)))

(with-output-to-string (str)
  (with-line (line ("text.lisp" :external-format :utf8)
                   (str)
                   line-number)
    (print line)
    (string+ line-number ":" #\Tab
             (ppcre:regex-replace-all "with" line "羊"))))

|#