(in-package :quek)

(export '(spawn send receive +exit+ *current-thread*))

(define-symbol-macro *current-thread* (bt:current-thread))

(defvar *thread-process-map* (tg:make-weak-hash-table
                              :weakness :key-and-value :test #'eq))

(defvar *processes-mutex* (bt:make-lock))

(defvar +exit+ '+exit+)

(defclass process ()
  ((name :initarg :name :accessor name-of)
   (mbox-head :accessor mbox-head-of)
   (mbox-tail :accessor mbox-tail-of)
   (waitqueue :initform (bt:make-condition-variable)
              :accessor waitqueue-of)
   (mutex :initform (bt:make-lock) :accessor mutex-of)
   (childrent :initform nil :accessor children-of)
   (thread :initarg :thread :accessor thread-of)))

(defmethod initialize-instance :after ((process process) &rest initargs)
  (declare (ignore initargs))
  (let ((mbox (cons nil nil)))
    (setf (mbox-head-of process) mbox
          (mbox-tail-of process) mbox)))

(defgeneric message-exist-p (process)
  (:method ((process process))
    (not (eq (mbox-head-of process)
             (mbox-tail-of process)))))

(defgeneric kill-children (process)
  (:method ((process process))
    (loop for i in (children-of process)
          do (send i +exit+))))

(defgeneric kill-process (process)
  (:method ((process process))
    (kill-children process)
    (let ((thread (thread-of process)))
      (when (bt:thread-alive-p thread)
        (bt:destroy-thread thread)))))

(defun get-process (&optional (thread *current-thread*))
  (bt:with-lock-held (*processes-mutex*)
    (sif (gethash thread *thread-process-map*)
         it
         (setf it (make-instance 'process
                                 :name (bt:thread-name thread)
                                 :thread thread)))))

(defun spawn% (function)
  (let* ((thread (bt:make-thread function
                                 :name (symbol-name (gensym "quek.pid"))))
         (current-process (get-process *current-thread*)))
    (push (get-process thread) (children-of current-process))
    thread))

(defmacro spawn (&body body)
  `(spawn% (lambda ()
             ,@body)))

(defgeneric send (receiver message))

(defmethod send (thread message)
  "thread is native thread type."
  (send (get-process thread) message))

(defmethod send ((process process) message)
  (if (eq message +exit+)
      (kill-process process)
      (bt:with-lock-held ((mutex-of process))
        (let ((cons (cons nil nil)))
          (setf (car (mbox-tail-of process)) message
                (cdr (mbox-tail-of process)) cons
                (mbox-tail-of process) cons))
        (bt:condition-notify (waitqueue-of process))
        message)))

(defmethod send ((thread-name string) message)
  (awhen (find thread-name (bt:all-threads)
               :key #'bt:thread-name
               :test #'string=)
    (send it message)))

(defmacro receive ((&optional timeout timeout-value) &body body)
  (alexandria:once-only (timeout timeout-value)
    (alexandria:with-gensyms (timeout-p process waitqueue mbox-head mbox-tail wait rev messages prev mutex)
      `(let ((,process (get-process)))
         (with-accessors ((,waitqueue waitqueue-of)
                          (,mutex mutex-of)
                          (,mbox-head mbox-head-of)
                          (,mbox-tail mbox-tail-of)) ,process
           (let (,timeout-p)
             (when (and (not (message-exist-p ,process)) ,timeout)
               (spawn (sleep ,timeout)
                      (bt:with-lock-held (,mutex)
                        (setf ,timeout-p t)
                        (bt:condition-notify ,waitqueue))))
             (labels ((,rev (,messages ,prev)
                        (if (eq ,messages ,mbox-tail) ; no more message
                            (if (,wait)
                                (,rev ,mbox-head nil)
                                ,timeout-value)
                            (mcond (car ,messages)
                              ,@(append
                                 (mapcar
                                  (lambda (x)
                                    `(,(car x)
                                       (bt:with-recursive-lock-held (,mutex)
                                         (if ,prev
                                             (setf (cdr ,prev)
                                                   (cdr ,messages))
                                             (setf ,mbox-head
                                                   (cdr ,messages))))
                                       ,@(cdr x)))
                                  body)
                                 `((_ (,rev (cdr ,messages) ,messages)))))))
                      (,wait ()
                        (if (message-exist-p ,process)
                            t
                            (bt:with-recursive-lock-held (,mutex)
                              (unless ,timeout-p
                                (bt:condition-wait ,waitqueue ,mutex))
                              (message-exist-p ,process)))))
               (,rev ,mbox-head nil))))))))
#|
(let ((p (spawn (labels ((f ()
                           (receive ()
                                    ((:foo val)
                                     (format t "~&foo -> ~a" val)
                                     (force-output)
                                     (f))
                                    ((:bar val)
                                     (format t "~&bar -> ~a" val)
                                     (force-output)
                                     (f))
                                    (:quit
                                     (format t "~&終了")
                                     (force-output)))))
                  (f)))))
  (send p '(:foo 1))
  (send p '(:bar 2))
  (send p :quit))

(let ((thread (spawn (labels ((f ()
                                (receive ()
                                         ('quit (print "quit! bye, bye."))
                                         (x (print x)
                                            (force-output)
                                            (f)))))
                       (f)))))
  (send thread 'hello)
  (sleep 1)
  (send thread 'world)
  (sleep 1)
  (send thread 'quit))

(let ((thread (spawn (loop (receive () (x (print x)))
                           (force-output)))))
  (send thread 'hello)
  (send thread 'world)
  (sleep 0.1)
  (send thread +exit+))

(let ((th (spawn
            (print "start...")
            (print (receive (0 "タイムアウトした")))
            (print "end...")
            (force-output))))
  (sleep 1)
  (send th "おわり"))

(receive (0 "タイムアウトした"))

(let* ((max 10000)
       (this *current-thread*)
       (a (spawn (loop repeat (/ max 2)
                       for i from 1 by 2
                       do (bt:thread-yield)
                       do (send this i))))
       (b (spawn (loop repeat (/ max 2)
                       for i from 2 by 2
                       do (bt:thread-yield)
                       do (send this i)))))
  (loop repeat max
        do (receive ()
             (x (print x))))
  (send a +exit+)
  (send b +exit+))
|#