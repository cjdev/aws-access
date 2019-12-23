(defpackage :mfa-tool.aws-dispatcher
  (:use :cl)
  (:export #:aws-dispatcher #:update-stacks #:select-stack #:stacks #:stack
           #:put-stack
           #:refresh-stack))
(in-package :mfa-tool.aws-dispatcher)

(defclass aws-dispatcher ()
  ((%region :reader region :accessor %region
            :initarg :region)
   (%credentials :reader credentials
                 :initarg :credentials))
  (:default-initargs
   :region "us-east-1"
   :credentials (error "AWS-DISPATCHER requires a :CREDENTIALS initarg")))

(defmacro defprint-slots (class slots)
  `(defmethod print-object ((o ,class) s)
     (with-slots ,slots o
       (print-unreadable-object (o s :type t :identity t)
         (format s "~@{~s~^, ~}"
                 ,@slots)))))
(defprint-slots daydreamer.aws-result:stack
    (daydreamer.aws-result::%stack-name))

(defclass refresh-stack ()
  ((%stack :reader stack :initarg :stack)))
(defprint-slots refresh-stack (%stack))
(defun refresh-stack (stack)
  (fw.lu:new 'refresh-stack stack))

(defclass put-stack ()
  ((%stack :reader stack :initarg :stack)))
(defun put-stack (stack)
  (fw.lu:new 'put-stack stack))

(defclass update-stacks ()
  ((%stacks :initarg :stacks :reader stacks)))
(defun update-stacks (stacks)
  (fw.lu:new 'update-stacks stacks))

(defclass select-stack ()
  ((%stacks :initarg :stack :reader stack)))
(defprint-slots select-stack (%stacks))

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

(defmethod mfa-tool.store:dispatch :after ((store aws-dispatcher) (action refresh-stack))
  (let ((new-value (daydreamer.aws-result:extract-stack
                    (car
                     (daydreamer.aws-result:extract-list
                      (cdar
                       (aws/cloudformation:describe-stacks
                        :stack-name (daydreamer.aws-result:stack-name
                                     (stack action)))))))))
    (mfa-tool.store:dispatch store (put-stack new-value))))

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