(defpackage :mfa-tool.store
  (:use :cl)
  (:export #:store #:execute #:dispatch
           #:next-store
           #:next-store-p
           #:propagate
           #:recording-store
           #:record))
(in-package :mfa-tool.store)

(defclass store ()
  ((%next-store :initarg :next-store :reader next-store)))
(defun next-store-p (store)
  (slot-boundp store '%next-store))

(fw.lu:defclass+ recording-store ()
  ((%record :reader record :accessor %record :initform ())))

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
    (execute store action))
  (:method ((store symbol) action)
    (execute store action)))

(defun propagate (store action)
  (when (next-store-p store)
    (dispatch (next-store store)
              action))
  store)

(defmethod dispatch :before ((store recording-store) action)
  (push action
        (%record store)))
