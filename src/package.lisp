(defpackage :mfa-tool.aws-utils
  (:use :cl)
  (:export #:cj-organization-role
           #:cj-developer-role
           #:arn-for
           #:cj-organization-role-arn))

(defpackage :mfa-tool
  (:use :cl)
  (:import-from :mfa-tool.aws-utils
                #:cj-organization-role-arn
                #:cj-developer-role
                #:arn-for)
  (:export :main
   :url
           :sts-result-handler))
