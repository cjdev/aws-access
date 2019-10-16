(defpackage :mfa-tool.read-credentials
  (:use :cl)
  (:export #:access-key
           #:secret-access-key
           #:credential-update
           #:prompt-for-aws-credentials))
(in-package :mfa-tool.read-credentials)

(defvar *store*)

(defun column (attrs &rest children)
  (apply 'make-instance 'capi:column-layout
         :children children
         attrs))

(defun row (attrs &rest children)
  (apply 'make-instance 'capi:row-layout
         :children children
         attrs))

(defun dispatch-with-action-creator (action-creator)
  (let ((store *store*))
    (lambda (data)
      (mfa-tool.store:dispatch store
                               (funcall action-creator data)))))

(defun dispatch-with-item (store)
  (lambda (data)
    (mfa-tool.store:dispatch store data)))

(defun list-panel (action attrs items)
  (apply 'make-instance 'capi:list-panel
         :items items
         :selection-callback (dispatch-with-action-creator action)
         :callback-type :data
         attrs))

(defun select (action attrs items)
  (apply 'make-instance 'capi:option-pane
         :items items
         :selection-callback (dispatch-with-action-creator action)
         :callback-type :data
         attrs))

(defun text-input (action attrs &optional (text ""))
  (apply 'make-instance 'capi:text-input-pane
         :text text
         :change-callback (dispatch-with-action-creator action)
         attrs))

(defun button-panel (attrs &rest items)
  (apply 'make-instance 'capi:push-button-panel
         :items items
         :selection-callback (dispatch-with-item *store*)
         :callback-type :data
         attrs))

(defclass update-access-key ()
  ((%key :initarg :key :reader key)))
(defun update-access-key (key)
  (fw.lu:new 'update-access-key key))

(defclass update-secret-access-key ()
  ((%key :initarg :key :reader key)))
(defun update-secret-access-key (key)
  (fw.lu:new 'update-secret-access-key key))

(defun layout ()
  (column ()
          (make-instance 'capi:grid-layout
                         :x-adjust :right
                         :y-adjust :center
                         :description
                         (list "Access Key:"
                               (text-input 'update-access-key
                                           ())

                               "Secret Access Key:"
                               (text-input 'update-secret-access-key
                                           (list :min-width '(:character 45)
                                                 :callback-type nil
                                                 :callback (alexandria:compose
                                                            (dispatch-with-item *store*)
                                                            (lambda () :|Ok|))))))
          (row ()
               nil
               (button-panel '(:default-button  :|Ok|
                               :cancel-button   :|Cancel|
                               :accepts-focus-p t)
                             :|Cancel|
                             :|Ok|))))


(defclass credential-input-store (mfa-tool.store:store)
  ((access-key :accessor access-key)
   (secret-access-key :accessor secret-access-key)
   (%done-cb :writer bind :reader done-callback)))
(defun done-callback-p (store)
  (slot-boundp store '%done-cb))

(fw.lu:defclass+ credential-update ()
  ((access-key :reader access-key :initarg :access-key)
   (secret-access-key :accessor secret-access-key :initarg :secret-access-key)))

(defmethod mfa-tool.store:execute ((store credential-input-store) (action update-access-key))
  (setf (access-key store) (key action)))
(defmethod mfa-tool.store:execute ((store credential-input-store) (action update-secret-access-key))
  (setf (secret-access-key store) (key action)))

(defmethod print-object ((object credential-update) s)
  (print-unreadable-object (object s :type t :identity t)
    (format s "~s ~s"
            (access-key object)
            (secret-access-key object))))

(defmethod mfa-tool.store:dispatch :after ((store credential-input-store) (action (eql :|Ok|)))
  (fw.lu:with-accessors* (access-key secret-access-key) store
    (let ((credential-update (credential-update access-key secret-access-key)))
      (mfa-tool.store:propagate store credential-update)
      (capi:exit-dialog credential-update))))

(defmethod mfa-tool.store:dispatch :after ((store credential-input-store) (action (eql :|Cancel|)))
  (capi:abort-dialog))

(defun prompt-for-aws-credentials (next-store)
  (let* ((*store* (make-instance 'credential-input-store :next-store next-store))
         (interface (make-instance 'capi:interface
                                   :layout (layout)
                                   :title "Enter Credentials")))
    (capi:display-dialog interface)))
