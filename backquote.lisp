(defpackage :backquote-test
    (:use :cl))

(in-package :backquote-test)

(defmacro mkvar (s)
  `(defvar ,(intern (format nil "*~a*" s)) (make-symbol ,s)))

(defmacro mkvars (&rest ss)
  `(progn
    ,@(loop for i in ss
            collect `(mkvar ,i))))

(mkvars "COMMA"
        "COMMA-ATSIGN"
        "COMMA-DOT"
        "BQ-LIST"
        "BQ-APPEND"
        "BQ-LIST*"
        "BQ-NCONC"
        "BQ-CLOBBERABLE"
        "BQ-QUOTE")

(defvar *bq-quote-nil* (list *bq-quote* nil))


(set-macro-character
 #\$
 (lambda (stream char)
   (declare (ignore char))
   (list 'backquote (read stream t nil t))))

(set-macro-character
 #\%
 (lambda (stream char)
   (declare (ignore char))
   (case (peek-char nil stream t nil t)
     (#\@ (read-char stream t nil t)
        (list *comma-atsign* #1=(read stream t nil t)))
     (#\. (read-char stream t nil t)
        (list *comma-dot* #1#))
     (t (list *comma* #1#)))))


(defparameter *bq-simplify* t)

(defmacro backquote (x)
  (bq-completely-process x))

(defun bq-completely-process (x)
  (let ((raw-result (bq-process x)))
    (bq-remove-tokens (if *bq-simplify*
                        (bq-simplify raw-result)
                        raw-result))))

(defun bq-process (x)
  (cond ((atom x)
         (list *bq-quote* x))
        ((eq (car x) 'backquote)
         (bq-process (bq-completely-process (cadr x))))
        ((eq (car x) *comma*)
         (cadr x))
        ((eq (car x) *comma-atsign*)
         (error ",@~s after `" (cadr x)))
        ((eq (car x) *comma-dot*)
         (error ",.~s after `" (cadr x)))
        (t (do ((p x (cdr p))
                (q () (cons (bracket (car p)) q)))
               ((atom p)
                  (cons *bq-append*
                        (nreconc q (list (list *bq-quote* p)))))
             (when (eq (car p) *comma*)
               (unless (null (cddr p))
                 (error "Malformed ,~s" p))
               (return (cons *bq-append*
                             (nreconc q (list (cadr p))))))
             (when (eq (car p) *comma-atsign*)
               (error "Dotted ,@~S" p))
             (when (eq (car p) *comma-dot*)
               (error "Dotted ,.~S" p))))))

(defun bracket (x)
  (cond ((atom x)
         (list *bq-list* (bq-process x)))
        ((eq (car x) *comma*)
         (list *bq-list* (cadr x)))
        ((eq (car x) *comma-atsign*)
         (cadr x))
        ((eq (car x) *comma-dot*)
         (list *bq-clobberable* (cadr x)))
        (t
         (list *bq-list* (bq-process x)))))

(defun maptree (fn x)
  (if (atom x)
    (funcall fn x)
    (let ((a (funcall fn (car x)))
          (d (maptree fn (cdr x))))
      (if (and (eql a (car x))
               (eql d (cdr x)))
        x
        (cons a d)))))

(defun bq-splicing-frob (x)
  (and (consp x)
       (or (eq (car x) *comma-atsign*)
           (eq (car x) *comma-dot*))))

(defun bq-frob (x)
  (and (consp x)
       (or (eq (car x) *comma*)
           (eq (car x) *comma-atsign*)
           (eq (car x) *comma-dot*))))


(defun bq-simplify (x)
  (if (atom x)
    x
    (let ((x (if (eq (car x) *bq-quote*)
               x
               (maptree #'bq-simplify x))))
      (if (not (eq (car x) *bq-append*))
        x
        (bq-simplify-args x)))))

(defun bq-simplify-args (x)
  (do ((args (reverse (cdr x)) (cdr args))
       (result
         nil
         (cond ((atom (car args))
                (bq-attach-append *bq-append* (car args) result))
               ((and (eq (car args) *bq-list*)
                     (notany #'bq-splicing-frob (cdr args)))
                (bq-attach-conses (cdar args) result))
               ((and (eq (caar args) *bq-list**)
                     (notany #'bq-splicing-frob (cdar args)))
                (bq-attach-conses
                 (reverse (cdr (reverse (cdar args))))
                 (bq-attach-append *bq-append*
                                   (car (last (car args)))
                                   result)))
               ((and (eq (caar args) *bq-quote*)
                     (consp (cadar args))
                     (not (bq-frob (cadar args)))
                     (null (cddar args)))
                (bq-attach-conses (list (list *bq-quote*
                                             (caadar args)))
                                  result))
               ((eq (caar args) *bq-clobberable*)
                (bq-attach-append *bq-nconc* (cadar args) result))
               (t
                (bq-attach-append *bq-append*
                                  (car args)
                                  result)))))
      ((null args) result)))

(defun null-or-quoted (x)
  (or (null x)
      (and (consp x)
           (eq (car x) *bq-quote*))))

(defun bq-attach-append (op item result)
  (cond ((and (null-or-quoted item)
              (null-or-quoted result))
         (list *bq-quote* (append (cadr item) (cadr result))))
        ((or (null result)
             (equal result *bq-quote-nil*))
         (if (bq-splicing-frob item)
           (list op item)
           item))
        ((and (consp result)
              (eq (car result) op))
         (list* (car result) item (cdr result)))
        (t
         (list op item result))))

(defun bq-attach-conses (items result)
  (cond ((and (every #'null-or-quoted items)
              (null-or-quoted result))
         (list *bq-quote*
               (append (mapcar #'cadr items) (cadr result))))
        ((or (null result)
             (equal result *bq-quote-nil*))
         (cons *bq-list* items))
        ((and (consp result)
              (or (eq (car result) *bq-list*)
                  (eq (car result) *bq-list**)))
         (cons (car result) (append items (cdr result))))
        (t
         (cons *bq-list** (append items (list result))))))

(defun bq-remove-tokens (x)
  (cond ((eq x *bq-list*) 'list)
        ((eq x *bq-append*) 'append)
        ((eq x *bq-nconc*) 'nconc)
        ((eq x *bq-list**) 'list*)
        ((eq x *bq-quote*) 'quote)
        ((atom x) x)
        ((eq (car x) *bq-clobberable*)
         (bq-remove-tokens (cadr x)))
        ((and (eq (car x) *bq-list**)
              (consp (cddr x))
              (null (cdddr x)))
         (cons 'cons (maptree #'bq-remove-tokens (cdr x))))
        (t
         (maptree #'bq-remove-tokens x))))


(defun try (ls &optional (n 0))
  (dolist (x ls)
    (format t "~&~a"
            (substitute #\` #\$ (substitute #\, #\% x)))
    (do ((form (macroexpand (read-from-string x)) (eval form))
         (str " = " "~% => ")
         (j 0 (+ j 1)))
        ((>= j n)
           (format t str)
           (write form :pretty t))
      (format t str)
      (write form :pretty t)))
  (format t "~&"))

(defvar q '(r s))
(defun r (x) (reduce #'* x))
(defvar r '(3 5))
(defvar s '(4 6))

(macroexpand-1 (read-from-string "$$(%%q)"))
(eval (read-from-string  "$$(%%q)"))
(eval (eval (read-from-string  "$$(%%q)")))

(defparameter flools2 '("$$(foo %%p)"
                        "$$(foo %%@p)"
                        "$$(foo %'%p)"
                        "$$(foo %'%@p)"
                        "$$(foo %@%p)"
                        "$$(foo %@%@p)"
                        "$$(foo %@'%p)"
                        "$$(foo %@'%@p)"))

(defun foo (&rest x) (apply #'list x))
(defun pp (&rest x) (apply #'list x))
(defparameter pp 'pp)
(defparameter p '(pp))

(try flools2 2)