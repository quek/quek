(cl:in-package :cl)

(macrolet ((m ()
             (let ((cl-symbols (loop for symbol being the external-symbols in :cl
                                     unless (member (symbol-name symbol) series::/series-forms/
                                                    :key #'symbol-name :test #'string=)
                                       collect symbol))
                   (anaphora-symbols (loop for symbol being the external-symbols in :anaphora
                                           collect symbol))
                   (series-symbols (loop for symbol being the external-symbols in :series
                                         collect symbol)))
               `(defpackage :quek
                  (:use :anaphora)
                  (:import-from :cl ,@cl-symbols)
                  (:import-from :series ,@series::/series-forms/ ,@series-symbols)
                  (:export ,@cl-symbols
                           ,@anaphora-symbols
                           ,@series::/series-forms/
                           ,@series-symbols)
                  (:nicknames #:q)))))
  (m))

(series::install :pkg :quek :implicit-map t)
