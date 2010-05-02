(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :quek)
  (require :stefil))

(defpackage quek-process-test
    (:use :cl :quek :stefil))

(in-package :quek-process-test)

(defsuite quek-process-test)
(in-suite quek-process-test)


(deftest test-timeout ()
  (is (eq 'hello (@ :timeout 0 :timeout-value 'hello)))
  (is (eq 'hello (@ :timeout 0.01 :timeout-value 'hello)))
  (let ((thread *current-thread*))
    (! thread 'world)
    (is (eq 'world (@ :timeout 0.01 :timeout-value 'hello))))
  (let ((thread *current-thread*))
    (spawn (sleep 0.1) (! thread 'world))
    (is (eq 'hello (@ :timeout 0.01 :timeout-value 'hello))))
)

(quek-process-test)