(in-package :quek)

(export '(emptyp))

(defgeneric emptyp (x)
  (:method ((x null))
    t)
  (:method ((x string))
    (string= x ""))
  (:method (x)
    nil))
