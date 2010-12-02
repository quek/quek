(in-package :quek)

(export '(mif
          mcond))

(defun s!-symbol-p (s)
  "slot symbol p"
  (x!-symbol-p #\S s))

(defun a!-symbol-p (s)
  "accessor symbol p"
  (x!-symbol-p #\A s))

(defun b!-symbol-p (s)
  "bind symbol p"
  (x!-symbol-p #\B s))

(defun intern-no!-symbol (s)
  (intern (subseq (symbol-name s) 2) (symbol-package s)))

(defun pattern-convert (pattern)
  (cond ((and (symbolp pattern) (not (keywordp pattern)))
         (cond ((b!-symbol-p pattern)
                (intern-no!-symbol pattern))
               (t
                `(quote ,pattern))))
        ((atom pattern)
         pattern)
        ((eq 'quote (car pattern))
         `(quote (quote ,(cadr pattern))))
        (t (with-ca/dr pattern
             (cons (pattern-convert car)
                   (and cdr (pattern-convert cdr)))))))

;;(pattern-convert 'b!char-code-limit)
;;(pattern-convert 'char-code-limit)
;;(eval (pattern-convert 'b!char-code-limit))
;;(eval (pattern-convert 'char-code-limit))
;;(pattern-convert '(a b))
;;(pattern-convert '(a b!b))
;;(pattern-convert '(a . b))     => ('a quete b)
;;(pattern-convert '(a . b!b))   => ('a . b)
;;(pattern-convert '(b!a . b))   => (a quete b)
;;(pattern-convert '(b!a . b!b)) => (a . b)
;;(pattern-convert 1)
;;(pattern-convert ''a)
;;(pattern-convert '('a b))



(defun match-p (pattern value &optional env)
  (cond ((null pattern)                 ; nil
         (values (null value) env))
        ((functionp pattern)            ; function?
         (values (equal value (funcall pattern value)) env))
        ((atom pattern)
         (cond ((keywordp pattern)      ; :xxx
                (values (eq value pattern) env))
               ((symbolp pattern)       ; symbol
                (if (string= (symbol-name pattern) "_")
                    (values t env)
                    (aif (assoc pattern env)
                         (values (equal value (cdr it)) env)
                         (values t (acons pattern value env)))))
               (t (values (equal value pattern) env))))
        (t (if (atom value)
               (and (eq 'quote (car pattern))
                    (eq (cadr pattern) value))
             (with-ca/dr pattern
               (multiple-value-bind (match-p env) (match-p car (car value) env)
                 (if match-p
                     (match-p cdr (cdr value) env)
                     nil)))))))
;;(match-p nil nil)
;;(match-p 'a 1)
;;(match-p '(a b) '(1 2))
;;(match-p '(a (b c)) '(1 (2 3)))
;;(match-p '(a . b) '(1 2 3))
;;(match-p '((a b) c) '((1 2) 3))
;;(match-p "ま" "ま")
;;(match-p ''x 'x)
;;(match-p ''x 'y)

(defun collect-pattern-symbol (pattern)
  (unless (atom pattern)
    (with-ca/dr pattern
      (cond ((and (eq 'quote car)           ; 'a
                  (atom (car cdr)))
             cdr)
            ((and (eq 'quote car)
                  (consp (car cdr)))
             nil)
            ((atom car)                 ; (a ...)
             (collect-pattern-symbol cdr))
            ((and (eq 'quote (car car)) ; ('a ...)
                  (atom (cadr car)))
             (cons (cadr car) (collect-pattern-symbol cdr)))
            ((and (eq 'quote (car car)) ; (''a ...)
                  (eq 'quote (caadr car)))
             (collect-pattern-symbol cdr))
            (t                          ; ((...) ...)
             (let ((car (collect-pattern-symbol car)))
               (if car
                   (cons car (collect-pattern-symbol cdr))
                   (collect-pattern-symbol cdr))))))))
;;(collect-pattern-symbol (pattern-convert 'a))
;;(collect-pattern-symbol (pattern-convert ''a))
;;(collect-pattern-symbol (pattern-convert '''a))


(defmacro mif (pattern value then &optional else)
  (alexandria:with-gensyms (match-p env)
    (let* ((converted-pattern (pattern-convert pattern))
           (pattern-symbols
            (delete-if (lambda (x)
                         (string= "_" (symbol-name x)))
                       (delete-duplicates
                        (collect-pattern-symbol converted-pattern)))))
      `(multiple-value-bind (,match-p ,env)
           (match-p ,(cond ((atom converted-pattern)
                            converted-pattern)
                           ((eq 'quote (car converted-pattern))
                            `(,@converted-pattern))
                           ((eq 'quote (car (last (butlast converted-pattern))))
                            `(rplacd (list ,@(butlast converted-pattern 2))
                                     ',(car (last converted-pattern))))
                           (t
                            `(list ,@converted-pattern)))
                    ,value)
         (declare (ignorable ,env))
         (if ,match-p
             (let ,(mapcar (lambda (x)
                             `(,x (cdr (assoc ',x ,env))))
                    pattern-symbols)
               (declare (ignorable ,@pattern-symbols))
               ,then)
             ,else)))))

;;(mif (a) '(1) a)
;;(mif (a b) '(1 2) (+ a b))
;;(mif (a b) '(1) (+ a b) 'baha)
;;(mif a '(1 2) (+ (car a) (cadr a)))
;;(mif (a) '(1 2) (+ (car a) (cadr a)) 'baha)
;;(mif (a a) '(1 2) (+ a b))
;;(mif (a a) '(2 2) (+ a a))
;;(mif (a . b) '(1 2 3) (list a b))
;;(mif 'x 'x t)
;;(mif 'x 'y t)

(defmacro mcond (value &body body)
  (alexandria:once-only (value)
    (reduce (lambda (x acc)
              `(mif ,(car x) ,value
                    (progn ,@(cdr x))
                    ,acc))
            body
            :initial-value nil ;;`(error "~s" ,g!value)
            :from-end t)))


#|

(match obj
       ((eql s!slot1 b!x)
        slot1)
       (('hello value) value)
       '       ((a nil) v'alast)
       ((a . b) (foo b))
       (nil end)
       ))

s!hello
v!hello

(let ((key "you-name"))
'v'ach ((slot name v!key)
        (ifmatch (cons * (cons x y)) '(1 (2 3) 4) (list x y)) ==> ((2 3) (4))

        (ifmatch (* x y) '(1 (2 3) 4) (list x y)) ==> ((2 3) (4))

        (let ((key "you-name"))
          (ifmatch ((slot name v!key)
                    |#
