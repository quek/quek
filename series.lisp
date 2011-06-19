(in-package :quek)

(defun scan-regex-split (regex string)
  (declare (optimizable-series-function))
  (producing (z) ((r regex) (s string) (scan-start 0) subseq-start subseq-end)
    (loop
      (tagbody
         ;; multiple-value-bind の中での setq は認識されないのでダミーで setq する。
         (setq scan-start scan-start
               subseq-start subseq-start
               subseq-end subseq-end)
         (multiple-value-bind (start end) (ppcre:scan r s :start scan-start)
           (unless start
             (if (< (length s) scan-start)
                 (terminate-producing)
                 (setq start (length s)
                       end (1+ start))))
           (setq subseq-start scan-start
                 subseq-end start
                 scan-start end))
         (next-out z (subseq s subseq-start subseq-end))))))

(assert (equal '("a" "b" "c") (collect (scan-regex-split "/" "a/b/c"))))
(assert (equal '("") (collect (scan-regex-split "/" ""))))
(assert (equal '("" "") (collect (scan-regex-split "/" "/"))))
(assert (equal '("" "a") (collect (scan-regex-split "/" "/a"))))
(assert (equal '("" "a" "") (collect (scan-regex-split "/" "/a/"))))
(assert (equal '("" "" "a" "" "") (collect (scan-regex-split "/" "//a//"))))
(assert (equal '("a" "b" "cc" "d") (collect (scan-regex-split "\\s+" "a b   cc
d"))))


(defun scan-line2-wrap (file name external-format body)
  `(with-open-file (,file ,name :external-format ,external-format) ,body))

(defmacro scan-line2 (name &optional (external-format :default))
  (let ((file (gensym)))
    `(encapsulated #'(lambda (body)
                       (scan-line2-wrap ',file ',name ',external-format body))
                   (scan-fn t
                            (lambda () (read-line ,file nil))
                            (lambda (_) (declare (ignore _)) (read-line ,file nil))
                            #'null))))

;;(scan-line2 "~/.sbclrc")
;;(scan-line2 "~/.sbclrc" :utf-8)
;;(collect (scan-line2 "~/.sbclrc"))
;;(collect (scan-line2 "~/.sbclrc" :utf-8))


(series::defS scan-line (name &optional (external-format :default))
  "(scan-line file-name &key (external-format :default))"
  (series::fragl
   ((name) (external-format)) ((items t))
   ((items t)
    (lastcons cons (list nil))
    (lst list))
   ()
   ((setq lst lastcons)
    (with-open-file (f name :direction :input :external-format external-format)
      (loop
        (cl:let ((item (read-line f nil)))
          (unless item
            (return nil))
          (setq lastcons (setf (cdr lastcons) (cons item nil))))))
    (setq lst (cdr lst)))
   ((if (null lst) (go series::end))
    (setq items (car lst))
    (setq lst (cdr lst)))
   ()
   ()
   :context)
  :optimizer
  (series::apply-literal-frag
   (cl:let ((file (series::new-var 'file)))
     `((((external-format)) ((items t))
        ((items t))
        ()
        ()
        ((unless (setq items (read-line ,file nil))
           (go series::end)))
        ()
        ((#'(lambda (code)
              (list 'with-open-file
                    '(,file ,name :direction :input :external-format ,external-format)
                    code)) :loop))
        :context)
       ,external-format))))


;;(scan-line "~/.sbclrc")
;;(scan-line "~/.sbclrc" :utf-8)
;;(collect (scan-line "~/.sbclrc"))
;;(collect (scan-line "~/.sbclrc" :utf-8))



(defun scan-directory (directory)
  (declare (optimizable-series-function))
  (producing (z) ((d (directory directory)) x)
    (loop
      (tagbody
         (when (endp d)
           (terminate-producing))
         (setq x (car d))
         (setq d (cdr d))
         (next-out z x)))))


(defun progs-body (var body)
  (let ((form (cond ((atom (car body))
                     (list (car body) var))
                    ((collect (choose-if (lambda (x) (eq var x))
                                         (scan-lists-of-lists-fringe (cdar body))))
                     (car body))
                    (t
                     (append (car body) (list var))))))
    (if (endp (cdr body))
        form
        `(let ((,var ,form))
           ,(progs-body var (cdr body))))))

(defmacro progs ((&optional (var (gensym))) &body body)
  `(let ((,var ,(car body)))
     ,(progs-body var (cdr body))))

#+baha
(progs ()
  (scan-range :upto 5)
  (choose-if #'oddp)
  (+ 10)
  (collect))
;;=> (11 13 15)
#+baha
(progs (x)
  (scan-range :upto 5)
  (choose-if #'oddp)
  (* x x)
  (collect))
;;=> (1 9 25)
#+baha
(progs (choose-if)
  (scan-range :upto 5)
  (choose-if #'oddp)
  (- choose-if 10)
  (collect))
;;=> (-9 -7 -5)
#+baha
(progs (x)
  (scan-symbols :cl)
  (collect-max (length (symbol-name x)) x))
;;=> LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT


(defun scan-char-range (from to)
  (declare (optimizable-series-function))
  (producing (z) ((c from) (to to) end (next from))
             (declare (type character c to next)
                      (type boolean end))
             (loop
               (tagbody
                  (if end
                      (terminate-producing))
                  (setq c next)
                  (if (char= c to)
                      (setq end t))
                  (setq next (code-char (1+ (char-code c))))
                  (next-out z c)))))

