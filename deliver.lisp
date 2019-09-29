(in-package :cl-user)
(setf *default-pathname-defaults*
 (make-pathname :directory (pathname-directory *load-pathname*)))
(format t "~&CURDIR: ~a~%" (truename "."))
(load-all-patches)
(load "~/quicklisp/setup.lisp")
(ql:quickload :swank)
(mapcar 'asdf:load-asd
        (directory "*.asd"))
(ql:quickload :aws-access)

(deliver (intern "MAIN" "MFA-TOOL")
         (create-macos-application-bundle
          "CJAWSAccess.app"
          :document-types nil
          :identifier "fwoar.cj.AWSAccess")
         0
         :KEEP-PRETTY-PRINTER t
         :interface :capi
         :keep-modules t
         :packages-to-keep-symbol-names '(:mfa-tool)
         :packages-to-keep '(:mfa-tool :swank)
         :startup-bitmap-file nil) 
