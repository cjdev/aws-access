(in-package :mfa-tool)

(defun account-selected (account)
  (format t "~s" account)
  (setf (ubiquitous:value :default-account)
        (cdr account)))

(defparameter *developer-p* (equal "elangley" (uiop/os:getenv "USER")))

(defgeneric assumed-credentials (store))
(defgeneric (setf assumed-credentials) (value store))

(defun current-account (interface)
  (cdr (capi:choice-selected-item (account-selector interface))))

(defun credentials-for-account (interface account)
   (gethash account 
            (assumed-credentials interface)))
(defun (setf credentials-for-account) (new-credentials interface account)
  (setf (gethash account
                 (assumed-credentials interface))
        new-credentials))

(defun current-credentials (interface)
  (credentials-for-account interface
                           (current-account interface)))

(defun go-on (_ interface)
  (declare (ignore _))
  (let ((token (capi:text-input-pane-text (mfa-input interface)))
        (user-name (capi:text-input-pane-text (user-input interface)))
        (account (current-account interface)))
    (clear-cookies)
    (multiple-value-bind (signin-token creds)
        (handler-bind (((or dexador:http-request-forbidden
                            dexador:http-request-bad-request)
                         (lambda (c)
                           (let ((message (nth-value 2 (sts-error-value (dex:response-body c)))))
                             (multiple-value-bind (new-code completed)
                                 (capi:prompt-for-string (format nil "~a~%Enter new MFA token:"
                                                                 message)
                                                         :ok-check (lambda (v)
                                                                     (and (every 'digit-char-p v)
                                                                          (= (length v) 6))))
                               (if completed
                                   (progn (setf (capi:text-input-pane-text (mfa-input interface))
                                                new-code)
                                          (change-mfa-token new-code))
                                   (capi:abort-callback)))))))
          (run-process account user-name token))
      (with-open-file (stream (make-pathname :name ""
                                             :type "cj-aws"
                                             :defaults (user-homedir-pathname))
                              :direction :output
                              :if-exists :rename
                              :if-does-not-exist :create)
        (format (make-broadcast-stream stream
                                       (capi:collector-pane-stream (output interface)))
                "export AWS_ACCESS_KEY_ID='~a'~%export AWS_SECRET_ACCESS_KEY='~a'~%export AWS_SESSION_TOKEN='~a'~%"
                (session-id creds)
                (session-key creds)
                (session-token creds)))
      (capi:set-button-panel-enabled-items (slot-value interface 'action-buttons)
                                           :set t)
      (setf (credentials-for-account interface account) (session-credentials creds)
            (signin-url interface) (url-from-signin-token signin-token)))))

(defun close-active-screen ()
  (let ((active-interface
          (capi:screen-active-interface
           (capi:convert-to-screen))))
    (unless (typep active-interface 'mfa-tool)
      (capi:destroy active-interface))))


(defun close-active-screen-enabled ()
  (let ((active-interface
          (capi:screen-active-interface
           (capi:convert-to-screen))))
    (typep active-interface '(not mfa-tool))))

(defun load-accounts (&optional account-source)
  (yason:parse
   (alexandria:read-file-into-string
    (if account-source
        account-source
        (json-resource "accounts")))))

(defun reprocess-accounts (accounts)
  (let ((accounts (gethash "Accounts" accounts))
        (result ()))
    (mapc (lambda (account)
            (push (cons (format nil "~a (~a)" 
                                (gethash "Name" account)
                                (gethash "Type" account))
                        (gethash "Id" account))
                  result))
          accounts)
    (coerce (sort result 'string-lessp 
                  :key 'car)
            'list)))

