(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :quek)
  (require :stefil))

(defpackage quek-process-test
    (:use :cl :quek :stefil))

(in-package :quek-process-test)

(defsuite quek-process-test)
(in-suite quek-process-test)


(deftest test-timeout ()
  (is (eq 'hello (receive (0 'hello))))
  (is (eq 'hello (receive (0.01 'hello))))
  (let ((thread *current-thread*))
    (send thread 'world)
    (is (eq 'world (receive (0.01 'hello)))))
  (let ((thread *current-thread*))
    (spawn (sleep 0.1) (send thread 'world))
    (is (eq 'hello (receive (0.01 'hello)))))
)

(quek-process-test)