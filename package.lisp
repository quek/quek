(cl:in-package :cl)

(macrolet ((m ()
             (let ((anaphora-symbols (loop for symbol being the external-symbols in :anaphora
                                           collect symbol))
                   (series-symbols (loop for symbol being the external-symbols in :series
                                         collect symbol)))
               `(defpackage :quek
                  (:use :cl :anaphora :series)
                  (:shadowing-import-from :series ,@series::/series-forms/)
                  (:import-from :series ,@series-symbols)
                  (:export ,@anaphora-symbols
                           ;; ,@series::/series-forms/
                           ,@series-symbols
                           ;;
                           #:str
                           #:sym
                           #:eval-always
                           #:scan-line
                           #:syntax
                           #:string-start-p
                           #:symbol-head-p
                           #:with-ca/dr
                           #:emptyp
                           #:p
                           #:^
                           #:mcond
                           #:mif
                           ;; process
                           #:*current-thread*
                           #:receive
                           #:send
                           #:spawn
                           #:+exit+
                           ;; series
                           #:scan-directory
                           #:scan-regex-split
                           #:progs
                           ;; ragne
                           #:0..
                           #:0...
                           #:1...
                           #:1..
                           #:n..
                           #:n...
                           ;; dev.lisp
                           #:set-package-nicknames
                           #:use-symbol
                           #:sdefpackage)
                  (:nicknames #:q)))))
  (m))

(series::install :pkg :quek :implicit-map t)
