;;;; -*- Mode: LISP; -*-
(asdf:defsystem :quek
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "prelude")
               (:file "util")
               (:file "match")
               (:file "process")
               (:file "string")
               (:file "range"))
  :depends-on (alexandria
               series
               anaphora
               cl-ppcre
               bordeaux-threads
               trivial-garbage
               named-readtables))
