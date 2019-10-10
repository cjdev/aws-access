(defpackage :fwoar.credential-provider
  (:use :cl )
  (:export
   #:make-aws-session))
(in-package :fwoar.credential-provider)

(defstruct hash-ref name hash-table)
(defstruct leaf name value)

(defun normalize (key)
  (string-trim " " key))

(defmethod architecture.builder-protocol:make-node
    ((builder (eql :my-builder)) (kind (eql :section)) &key name bounds)
  (declare (ignore bounds))
  (let ((name (normalize (car name))))
    (make-hash-ref :name (normalize name)
                   :hash-table (alexandria:alist-hash-table `((,name . ,(make-hash-table :test 'equal)))
                                                            :test 'equal))))

(defmethod architecture.builder-protocol:relate
    ((builder  (eql :my-builder))
     (relation (eql :section-option))
     (left     hash-ref)
     (right    cons)
     &key)
  (alexandria:ensure-gethash (normalize (car right))
                             (gethash (hash-ref-name left)
                                      (hash-ref-hash-table left))
                             (normalize (cdr right)))
  left)

(defmethod architecture.builder-protocol:make-node
    ((builder (eql :my-builder)) (kind (eql :option)) &key name value bounds)
  (declare (ignore bounds))
  (cons (car name)
        value))

(defmethod architecture.builder-protocol:finish-node
    ((builder (eql :my-builder)) (kind (eql :section)) (node hash-ref))
  (hash-ref-hash-table node))

(defun merge-hash-tables (tables)
  (reduce (lambda (next acc)
            (loop for key being each hash-key in next using (hash-value value)
                  do (setf (gethash key acc) value))
            acc)
          tables
          :initial-value (make-hash-table :test 'equal)
          :from-end t))

(defun parse-ini (fn)
  (merge-hash-tables (parser.ini:parse (pathname fn)
                                       :my-builder)))

(defclass fwoar-provider (aws-sdk/credentials/base:provider)
  ((file :initarg :file
     :initform #P"~/.aws/credentials")
   (profile :initarg :profile
            :initform aws-sdk:*aws-profile*
            :accessor provider-profile)

   (retrievedp :initform nil)))

(defun read-credentials (provider)
  (with-slots (file) provider
    (when (probe-file file)
      (gethash (provider-profile provider)
               (parse-ini file)))))

(defmethod aws-sdk/credentials/base:retrieve ((provider fwoar-provider))
  (with-slots (retrievedp file) provider
    (setf retrievedp nil)
    (let ((section (read-credentials provider)))
      (when section
        (setf retrievedp t)
        (aws-sdk:make-credentials
         :access-key-id (gethash "aws_access_key_id" section)
         :secret-access-key (gethash "aws_secret_access_key" section)
         :session-token (gethash "aws_session_token" section)
         :provider-name "fwoar-provider")))))

(defun make-aws-session ()
  (let ((aws-sdk/credentials::*chained-providers*
          (list* (make-instance 'fwoar-provider)
                 aws-sdk/credentials::*chained-providers*)))
    (aws:make-session :credentials (aws:default-aws-credentials))))
