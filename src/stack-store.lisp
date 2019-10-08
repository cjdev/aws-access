(defpackage :mfa-tool.stack-store
  (:use :cl)
  (:export #:stack-store #:available-stacks selected-stack parameters outputs))
(in-package :mfa-tool.stack-store)

(defun column (key)
  (serapeum:op (serapeum:assocadr key _
                                  :test 'equal)))

(defclass stack-store (mfa-tool.store:store mfa-tool.aws-dispatcher:aws-dispatcher)
  ((%available-stacks :accessor available-stacks :initform nil)
   (%selected-stack :accessor selected-stack :initform nil)
   (%parameters :accessor parameters :initform nil)
   (%outputs :accessor outputs :initform nil)))


(defun output-columns (output)
  (funcall (data-lens:juxt (column "OutputKey")
                           (column "OutputValue"))
           output))

(defun parameter-columns (output)
  (funcall (data-lens:juxt (column "ParameterKey")
                           (column "ParameterValue"))
           output))

(defmethod mfa-tool.store:dispatch :after ((store stack-store)
                                           (action mfa-tool.aws-dispatcher:update-stacks))
  (alexandria:when-let ((stack (car (mfa-tool.aws-dispatcher:stacks action))))
    (mfa-tool.store:dispatch store (mfa-tool.aws-dispatcher:select-stack stack))))

(defmethod mfa-tool.store:execute ((store stack-store) (action mfa-tool.aws-dispatcher:update-stacks))
  (setf (available-stacks store) (sort (mfa-tool.aws-dispatcher:stacks action)
                                       'string-lessp
                                       :key 'daydreamer.aws-result:stack-name)))

(defmethod mfa-tool.store:execute ((store stack-store) (action mfa-tool.aws-dispatcher:select-stack))
  (let ((stack (mfa-tool.aws-dispatcher:stack action)))
    (setf (selected-stack store) stack

          (parameters store) (mapcar 'parameter-columns (daydreamer.aws-result:parameters stack))

          (outputs store) (mapcar 'output-columns (daydreamer.aws-result:outputs stack)))))
