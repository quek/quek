(in-package :quek)

(defun set-package-nicknames (package &rest nicknames)
  (rename-package package (package-name package) nicknames))

(defun collect-untinterned-symbol (tree)
  (remove-duplicates
   (collect
       (choose-if (^ and (symbolp _)
                     (not (symbol-package _)))
                  (scan-lists-of-lists-fringe tree)))))

(defun count-symbol-names (syms)
  (let ((tab (make-hash-table :test 'equal)))
    (collect-ignore
     (#M(^ incf (gethash (gensym-symbol-name _)
                         tab 0))
       (scan syms)))
    tab))

(defun gensym-symbol-name (sym)
  (ppcre:regex-replace-all "-{0,1}\\d+$"
                           (symbol-name sym)
                           ""))

(defun cons-tree-map (fun x)
  (cond
    ((null x)
     nil)
    ((consp x)
     (cons (cons-tree-map fun (car x))
           (cons-tree-map fun (cdr x))))
    (t
     (funcall fun x))))

(defun mexp (form)
  (let ((symtab (count-symbol-names
                 (collect-untinterned-symbol
                  (sb-cltl2:macroexpand-all form)))))
    (cons-tree-map
     (lambda (x)
       (cond
         ;; シンボルでない場合は何もしない
         ((not (symbolp x)) x)
         ;; パッケージ名がある
         ((symbol-package x)
          (cond
            ;; 現在のパッケージ名と同じ
            ((string= (package-name (symbol-package x))
                      (package-name *package*))
             x)
            ;; 関数が束縛されていたらそのまま
            ((fboundp x) x)
            ;; それ以外は、パッケージ名を省略(現在のパッケージにする)
            ('T (intern (symbol-name x)))))
         ;; 接頭辞が一度しか使われてない場合は数字を取り除く
         ((eql 1 (gethash (gensym-symbol-name x)
                          symtab
                          0))
          (intern (gensym-symbol-name x)))
         ;; それ以外は、そのまま
         ('T x)))
     (sb-cltl2:macroexpand-all form))))

(defun mexp-string (form)
  (write-to-string (mexp (read-from-string form))))

#|
(mexp '(collect (scan "123")))
|#


#+sbcl
(defun thread-backtrace (thread)
  "Interrupt THREAD and print a backtrace. This should not affect the thread."
  (sb-thread:interrupt-thread
   thread
   (let ((debug-io *debug-io*))
     (lambda () (let ((*debug-io* debug-io))
                  (sb-debug:backtrace 128))))))
