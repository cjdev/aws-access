(in-package :mfa-tool)

(defvar *accounts* ())
(defun session-name ()
  (format nil "bootstrap~d" (+ 5000 (random 5000))))

(defparameter *user_management_account_id* 597974043991)

(defun mfa-serial-number (account user)
  (format nil "arn:aws:iam::~a:mfa/~a"
          account
          user))

(defun role-arn (account role)
  (format nil "arn:aws:iam::~a:role/cjorganization/~a"
          account
          role))

(defun read-new-mfa-token ()
  (format *query-io* "~&New MFA token: ")
  (finish-output *query-io*)
  (list (read-line *query-io*)))

(defun do-auth (user role token account)
  (let ((mfa-serial-number
          (mfa-serial-number *user_management_account_id*
                             user))
        (role-arn (role-arn account role)))
    (loop
      (restart-case
          (return
            (aws/sts:assume-role :role-arn role-arn
                                 :role-session-name (session-name)
                                 :serial-number mfa-serial-number
                                 :duration-seconds #.(* 12 60 60)
                                 :token-code token))
        (change-mfa-token (new-token)
          :interactive read-new-mfa-token
          (setf token new-token))
        (continue ())))))

(defun change-mfa-token (new-value)
  (when (find-restart 'change-mfa-token)
    (invoke-restart 'change-mfa-token new-value)))

(defun get-url (params)
  (format nil "https://signin.aws.amazon.com/federation?Action=getSigninToken&Session=~a"
          (quri.encode:url-encode (with-output-to-string (s)
                                    (yason:encode params s))
                                  :space-to-plus t)))

(cells:defmodel 
    sts-result-handler ()
  ((api-result :initarg :api-result :accessor api-result :initform (cells:c-in nil))
   (credentials :reader credentials
                :initform (cells:c? (serapeum:assocdr "Credentials" (^api-result)
                                                      :test 'equal)))
   (session-id :reader session-id
               :initform (cells:c? (serapeum:assocadr "AccessKeyId" (^credentials)
                                                      :test 'equal)))
   (session-key :reader session-key 
                :initform (cells:c? (serapeum:assocadr "SecretAccessKey" (^credentials)
                                                       :test 'equal)))
   (session-token :reader session-token 
                  :initform (cells:c? (serapeum:assocadr "SessionToken" (^credentials)
                                                         :test 'equal)))
   (url-params :reader url-params 
               :initform (cells:c? (fw.lu:alist-string-hash-table
                                    `(("sessionId" . ,(^session-id))
                                      ("sessionKey" . ,(^session-key))
                                      ("sessionToken" . ,(^session-token)))) ))
   (url :reader url
        :initform (cells:c? (get-url (^url-params))))))

(defgeneric session-credentials (source)
  (:method ((source sts-result-handler))
   (aws-sdk:make-credentials
    :access-key-id (session-id source)
    :secret-access-key (session-key source)
    :session-token (session-token source))))

(defun url-from-signin-token (signin-token)
  (format nil "https://signin.aws.amazon.com/federation?Action=login&Destination=https%3A%2F%2Fconsole.aws.amazon.com&SigninToken=~a"
          signin-token))

(defun read-new-aws-credentials ()
  (serapeum:collecting
    (fresh-line *query-io*)
    (format *query-io* "Access Key?")
    (finish-output *query-io*)
    (collect (read-line *query-io*))
    (format *query-io* "Secret Access Key?")
    (finish-output *query-io*)
    (collect (read-line *query-io*))))

(defun run-process (account user token)
  (loop
    (restart-bind ((set-aws-credentials (lambda (access-key-id secret-access-key)
                                          (setf aws:*session*
                                                (aws:make-session
                                                 :credentials (aws:make-credentials
                                                               :access-key-id access-key-id
                                                               :secret-access-key secret-access-key
                                                               :session-token nil
                                                               :provider-name "restart-provider")))
                                          (continue))
                                        :interactive-function 'read-new-aws-credentials
                                        :report-function (lambda (s)
                                                           (princ "Supply new AWS credentials" s))
                                        :test-function (lambda (c)
                                                         (and (find-restart 'continue)
                                                              (typep c 'aws:no-credentials)))))
      (let* ((api-result (cells:c-in (do-auth user "CJDeveloperAccessRole" token account)))
             (parser (make-instance 'sts-result-handler :api-result api-result))
             (federation-url (url parser))
             (signin-token (gethash "SigninToken" 
                                    (yason:parse
                                     (dexador:get federation-url)))))
        (return-from run-process
          (values signin-token
                  parser))))))

(defun set-aws-credentials (access-key-id secret-access-key &optional condition)
  (alexandria:when-let ((restart (find-restart 'set-aws-credentials condition)))
    (invoke-restart restart access-key-id secret-access-key)))

(defun open-url (url)
  (capi:contain (make-instance 'capi:browser-pane
                               :url url)
                :best-width 1280
                :best-height 800))

(defun sts-error-value (sts-response)
  (let ((parsed-error (dom:first-child
                       (cxml:parse sts-response
                                   (cxml-dom:make-dom-builder)))))
    (flet ((get-node (path)
             (dom:node-value
              (dom:first-child
               (xpath:first-node
                (xpath:with-namespaces (("" "https://sts.amazonaws.com/doc/2011-06-15/"))
                  (xpath:evaluate path parsed-error)))))))
      (values (get-node "//Error/Type")
              (get-node "//Error/Code")
              (get-node "//Error/Message")))))
