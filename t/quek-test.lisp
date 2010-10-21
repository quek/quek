(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :quek)
  (require :stefil))

(defpackage quek-test
    (:use :cl :quek :stefil))

(in-package :quek-test)

(defsuite quek-test)
(in-suite quek-test)


(deftest test-symbol-head-p ()
  (is (symbol-head-p 'abc "ab"))
  (is (symbol-head-p 'ab "ab"))
  (is (not (symbol-head-p 'a "ab")))
  (is (not (symbol-head-p "ab" "ab")))
  (is (symbol-head-p 'abc "")))


(defmacro! x*x (o!x)
  `(* ,g!x ,g!x))

(deftest test-demcaro! ()
  (is (= 4
         (let ((x 1))
           (x*x (incf x))))))


(deftest test-^ ()
  (is (equal '((2) (3) (4)) (mapcar (^ list (1+ _)) '(1 2 3))))
  (is (equal '(1 4 9) (mapcar (^ * _ _) '(1 2 3))))
  (is (equal '((1 a "A") (2 b "B"))
             (mapcar (^ list _z _y _x) '("A" "B") '(a b) '(1 2))))
  (is (equal '(1 2 3)
             (funcall (^ identity _rest) 1 2 3)))
  (is (equal '(2 1 3 4)
             (funcall (^ apply #'list _b _a _rest) 1 2 3 4)))
  (is (equal "a"
             (with-output-to-string (*standard-output*)
               (funcall (^ princ _) #\a))))
  (is (equal "cba"
             (with-output-to-string (*standard-output*)
               (funcall (^ (princ _z) (princ _y) (princ _x)) #\a #\b #\c)))))


(eval-always (quek::enable-lambda-reader))

(deftest |test-{}| ()
  (is (equal '((2) (3) (4)) (mapcar {list (1+ _)} '(1 2 3))))
  (is (equal '(1 4 9) (mapcar {* _ _} '(1 2 3))))
  (is (equal '((1 a "A") (2 b "B"))
             (mapcar {list _z _y _x} '("A" "B") '(a b) '(1 2))))
  (is (equal '(1 2 3)
             (funcall {identity _rest} 1 2 3)))
  (is (equal '(2 1 3 4)
             (funcall {apply #'list _b _a _rest} 1 2 3 4)))
  (is (equal "a"
             (with-output-to-string (*standard-output*)
               (funcall {princ _} #\a))))
  (is (equal "abc"
             (with-output-to-string (*standard-output*)
               (funcall {(princ _x) (princ _y) (princ _z)} #\a #\b #\c)))))

(eval-always (quek::disable-lambda-reader))


(deftest test-lambda ()
  (is (equal '((2) (3) (4)) (mapcar (lambda (x) (list (1+ x))) '(1 2 3))))
  (is (equal '(1 4 9) (mapcar (lambda (x) (* x x)) '(1 2 3))))
  (is (equal '((1 a "A") (2 b "B"))
             (mapcar (lambda (x y z) (list z y x)) '("A" "B") '(a b) '(1 2))))
  (is (equal '(1 2 3)
             (funcall (lambda (&rest rest) (identity rest)) 1 2 3)))
  (is (equal '(2 1 3 4)
             (funcall (lambda (a b &rest rest)
                        (apply #'list b a rest)) 1 2 3 4)))
  (is (equal "a"
             (with-output-to-string (*standard-output*)
               (funcall (lambda (x) (princ x)) #\a))))
  (is (equal "cba"
             (with-output-to-string (*standard-output*)
               (funcall (lambda (x y z)
                          (princ z) (princ y) (princ x)) #\a #\b #\c)))))

(named-readtables:in-readtable quek:syntax)

(deftest |test-#"-reader| ()
  (is (string= "" #""))
  (is (string= "" #""""""))
  (is (string= "abc" #"abc"))
  (is (string= "abc" #"""abc"""))
  (is (string= "abc" #"a\bc"))
  (is (string= "a\\bc" #"""a\bc"""))
  (is (string= "a\"bc" #"""a"bc""")) ;"))
  (let ((var "b"))
    (is (string= "abc" #"a#,var,c")))
  (let ((var "b"))
    (is (string= "abc" #"""a#,var,c""")))
  (let ((var "b"))
    (is (string= "ab c" #"a#,var c")))
  (let ((var "b"))
    (is (string= "ab c" #"""a#,var c""")))
  )


(deftest test-mif ()
  (is (= 1 (mif 1 1 1)))
  (is (= 2 (mif 1 0 1 2)))
  (is (string= "ま" (mif "あ" "あ" "ま" "さ")))
  (is (string= "さ" (mif "ああ" "あ" "ま" "さ")))
  (is (= 1 (mif (a) '(1) a)))
  (is (= 3 (mif (a b) '(1 2) (+ a b))))
  (is (eq 'baha (mif (a b) '(1) (+ a b) 'baha)))
  (is (= 3 (mif a '(1 2) (+ (car a) (cadr a)))))
  (is (eq 'baha (mif (a) '(1 2) (+ (car a) (cadr a)) 'baha)))
  (is (null (mif (a a) '(1 2) (+ a a))))
  (is (= 4 (mif (a a) '(2 2) (+ a a))))
  (is (null (mif (a b) 1 1)))
  (is (null (mif (a b) '(1) 1)))
  (is (null (mif (a b c) '(1 2) 1)))
  (is (= 1 (mif (_ a _) '(2 1 3) a)))
  (is (= 3 (let ((x 2))
             (mif (a b!x) '(1 2) (+ a x)))))
  (is (null (let ((x 1))
              (mif (a b!x) '(1 2) (+ a x)))))
  (is (= 6 (mif (a . b) '(1 2 3) (apply #'+ a b))))
  (is (= 1 (mif (a . b) '(1) (apply #'+ a b))))
  (is (= 2 (mif (a . b) '() 1 2)))
  (is (mif 'x 'x t))
  (is (null (mif 'x 'y t)))
  (is (mif ('x 'y) '(x y) t))
  (is (null (mif ('x 'y) '(x x) t)))
  (is (mif ('x y) '(x x) t))
  (is (null (mif ('x y) '(y y) t)))
  (is (mif (x 'y) '(y y) t))
  (is (null (mif (x 'y) '(x x) t)))
  )

(deftest test-mcond ()
  (is (mcond 1
        (1 t)))
  (is (null (mcond 1
              (2 t))))
  (labels ((len (x)
                (mcond x
                  ((_ . b) (1+ (len b)))
                  (_ 0))))
          (is (= 0 (len ())))
          (is (= 1 (len '(x))))
          (is (= 2 (len '(x x ))))
          (is (= 3 (len '(x x x)))))
  )

(quek-test)
