(defpackage :mfa-tool.aws-dispatcher
  (:use :cl)
  (:export #:aws-dispatcher #:update-stacks #:select-stack #:stacks #:stack))
(in-package :mfa-tool.aws-dispatcher)

(defclass aws-dispatcher ()
  ((%region :reader region :accessor %region
            :initarg :region)
   (%credentials :reader credentials
                 :initarg :credentials))
  (:default-initargs
   :region "us-east-1"
   :credentials (error "AWS-DISPATCHER requires a :CREDENTIALS initarg")))

(defclass update-stacks ()
  ((%stacks :initarg :stacks :reader stacks)))
(defun update-stacks (stacks)
  (fw.lu:new 'update-stacks stacks))

(defclass select-stack ()
  ((%stacks :initarg :stack :reader stack)))

(defun select-stack (stack)
  (fw.lu:new 'select-stack stack))

(defclass update-region ()
  ((%new-region :initarg :region :reader region)))
(defun update-region (region)
  (fw.lu:new 'update-region region))

(defmethod mfa-tool.store:dispatch :around ((store aws-dispatcher) action)
  (let ((aws-sdk:*session* (aws-sdk:make-session :credentials (credentials store)
                                                 :region (region store))))
    (call-next-method)))

(defmethod mfa-tool.store:execute ((store aws-dispatcher) (action update-region))
  (setf (%region store) (region action)))

(defmethod mfa-tool.store:dispatch :after ((store aws-dispatcher) (action update-region))
  (mfa-tool.store:dispatch store :|Get Stacks|))

(defmethod mfa-tool.store:dispatch :after ((store aws-dispatcher) (action (eql :|Get Stacks|)))
  (bt:make-thread
   (lambda ()
     (let ((aws-sdk:*session* (aws-sdk:make-session :credentials (credentials store)
                                                    :region (region store))))
       (mfa-tool.store:dispatch store
                                (update-stacks (mapcar 'daydreamer.aws-result:extract-stack
                                                       (daydreamer.aws-result:extract-list
                                                        (cdar
                                                         (aws/cloudformation:describe-stacks))))))))
   :name "Stack Fetcher"))