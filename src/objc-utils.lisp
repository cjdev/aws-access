(in-package :mfa-tool)

(defun bundle-resource-root ()
  (make-pathname :directory
                 (pathname-directory
                  (objc:invoke-into 'string
                                    (objc:invoke "NSBundle" "mainBundle") 
                                    "pathForResource:ofType:" "app" "icns"))))

(defun bundle-resource (relpath)
  (merge-pathnames relpath
                   (bundle-resource-root)))

(defun clear-cookies ()
  (let ((cookie-storage (objc:invoke "NSHTTPCookieStorage" "sharedHTTPCookieStorage")))
    (map nil
         (lambda (cookie) 
           (objc:invoke cookie-storage "deleteCookie:" cookie))
         (objc:invoke-into 'array cookie-storage "cookies"))))
