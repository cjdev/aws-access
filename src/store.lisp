(defpackage :mfa-tool.store
  (:use :cl)
  (:export #:store #:execute #:dispatch))
(in-package :mfa-tool.store)

(defclass store ()
  ())

(defgeneric execute (store action)
  (:argument-precedence-order action store)
  (:method :around (store action)
   (call-next-method)
   store)
  (:method (store action)
   store))

(defgeneric dispatch (store action)
  (:argument-precedence-order action store)
  (:method ((store store) action)
   (execute store action)))