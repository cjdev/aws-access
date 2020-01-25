(in-package :mfa-tool.aws-utils)

(defclass cj-organization-role ()
  ((account :initarg :account
            :reader account
            :initform (error "must pass an account"))))

(fw.lu:defclass+ cj-developer-role ((cj-organization-role (account)))
  ())
(fw.lu:defclass+ cj-provisioner-role ((cj-organization-role (account)))
  ())

(defgeneric arn-for (type account resource)
  (:method-combination list :most-specific-last)
  (:documentation "get the arn for an aws resource")
  (:method :around (-t account u)
    (format nil "arn:aws:iam::~a:~a"
            account
            (serapeum:string-join (call-next-method)
                                  "/")))
  (:method list ((type (eql :mfa)) account user)
    "mfa")
  (:method list ((type (eql :mfa)) account (user string))
    user)
  (:method list ((type (eql :role)) account role)
    "role")
  (:method list ((type (eql :role)) account (role cj-organization-role))
    "cjorganization")
  (:method list ((type (eql :role)) account (role cj-developer-role))
    "CJDeveloperAccessRole")
  (:method list ((type (eql :role)) account (role cj-provisioner-role))
    "CJProvisionerAccessRole")
  (:method list ((type (eql :role)) account (role string))
    role))

(defun cj-organization-role-arn (role)
  (arn-for :role (account role) role))

(defgeneric session-duration (role)
  (:method ((role cj-developer-role))
    :max)
  (:method ((role cj-provisioner-role))
    #.(* 60 60)))
