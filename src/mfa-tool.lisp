(in-package :mfa-tool)

(defun account-selected (account)
  (format t "~s" account)
  (setf (ubiquitous:value :default-account)
        (cdr account)))

(defparameter *developer-p* (equal "elangley" (uiop/os:getenv "USER")))

(defun bundle-resource-root ()
  (make-pathname :directory
                 (pathname-directory
                  (objc:invoke-into 'string
                                    (objc:invoke "NSBundle" "mainBundle") 
                                    "pathForResource:ofType:" "app" "icns"))))

(defun clear-cookies ()
  (let ((cookie-storage (objc:invoke "NSHTTPCookieStorage" "sharedHTTPCookieStorage")))
    (map nil
         (lambda (cookie) 
           (objc:invoke cookie-storage "deleteCookie:" cookie))
         (objc:invoke-into 'array cookie-storage "cookies"))))

(defun go-on (_ interface)
  (declare (ignore _))
  (let ((token (capi:text-input-pane-text (mfa-input interface)))
        (user-name (capi:text-input-pane-text (user-input interface)))
        (account (cdr (capi:choice-selected-item (account-selector interface)))))
    (clear-cookies)
    (multiple-value-bind (signin-token creds) (run-process account user-name token)
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
      (setf (signin-url interface) 
            (url-from-signin-token signin-token)))))

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

(defun load-accounts ()
  (yason:parse
   (alexandria:read-file-into-string
    (merge-pathnames (make-pathname :name "accounts"
                                    :type "json")
                     (bundle-resource-root)))))

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

