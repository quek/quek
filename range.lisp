(in-package :quek)

(export '(n..
          0..
          1..
          n...
          0...
          1...))

(defun n.. (from to)
  (loop for i from from to to collect i))

(defun 0.. (to)
  (n.. 0 to))

(defun 1.. (to)
  (n.. 1 to))

(defun n... (from below)
  (n.. from (1- below)))

(defun 0... (below)
  (0.. (1- below)))

(defun 1... (below)
  (1.. (1- below)))
