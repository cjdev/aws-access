#!/usr/bin/env sbcl --script
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:cl-yaml :yason :uiop :alexandria)))

(defparameter *in-file*
  (pathname (first (uiop:command-line-arguments))))

(defparameter *out-file*
  (make-pathname :type "json"
                 :defaults *in-file*))

(format t "~a -> ~a" *in-file* *out-file*)

(defparameter *accounts-in*
  (alexandria:read-file-into-string *in-file*))

(alexandria:with-output-to-file (out *out-file* :if-exists :supersede)
  (with-open-stream (out-stream (yason:make-json-output-stream out :indent t))
    (yason:encode (cl-yaml:parse *accounts-in*)
                  out-stream)))
