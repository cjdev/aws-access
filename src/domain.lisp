(in-package :mfa-tool)

(defvar *accounts* ())

(defun session-name ()
  (format nil "bootstrap~d" (+ 5000 (random 5000))))

(defparameter *user_management_account_id* 597974043991)


(defun do-auth (user role token account)
  (let ((mfa-serial-number
          (format nil "arn:aws:iam::~a:mfa/~a"
                  *user_management_account_id*
                  user))
        (role-arn
          (format nil "arn:aws:iam::~a:role/cjorganization/~a"
                  account
                  role)))
    (aws/sts:assume-role :role-arn role-arn
                         :role-session-name (session-name)
                         :serial-number mfa-serial-number
                         :duration-seconds #.(* 12 60 60)
                         :token-code token)))

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

(defun url-from-signin-token (signin-token)
  (format nil "https://signin.aws.amazon.com/federation?Action=login&Destination=https%3A%2F%2Fconsole.aws.amazon.com&SigninToken=~a"
          signin-token))

(defun run-process (account user token)
  (let* ((api-result (cells:c-in (do-auth user "CJDeveloperAccessRole" token account)))
         (parser (make-instance 'sts-result-handler :api-result api-result))
         (federation-url (url parser))
         (signin-token (gethash "SigninToken" 
                                (yason:parse
                                 (dexador:get federation-url)))))
    (values signin-token
            parser)))

(defun open-url (url)
  (capi:contain (make-instance 'capi:browser-pane
                               :url url)
                :best-width 1280
                :best-height 800))
