(defpackage :mfa-tool.aws-utils
  (:use :cl)
  (:export #:cj-organization-role
           #:cj-provisioner-role
           #:cj-developer-role
           #:arn-for
           #:cj-organization-role-arn
           #:session-duration))

(defpackage :mfa-tool
  (:use :cl)
  (:import-from :mfa-tool.aws-utils
                #:cj-organization-role-arn
                #:cj-developer-role
                #:cj-provisioner-role
                #:arn-for)
  (:export :main
   :url
           :sts-result-handler))
