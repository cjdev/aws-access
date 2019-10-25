(defun utf-8-file-encoding (pathname ef-spec buffer length)
  (declare (ignore pathname buffer length))
  (system:merge-ef-specs ef-spec :utf-8))

(setq system:*file-encoding-detection-algorithm*
      (substitute 'utf-8-file-encoding
                  'system:locale-file-encoding
                  system:*file-encoding-detection-algorithm*))
(set-default-character-element-type 'simple-char)

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
          :application-icns "app.icns"
          :identifier "fwoar.cj.AWSAccess"
          :version (asdf:component-version
                    (asdf:find-system :aws-access))
          :build (concatenate 'string
                              (subseq (uiop:run-program (list "git" "rev-parse" "HEAD") :output :string)
                                      0 7)
                              (uiop:run-program (list "zsh" "scripts/dirty-tag") :output :string)))
         0
         :keep-conditions :all
         :keep-pretty-printer t
         :interface :capi
         :keep-modules t
         :packages-to-keep-symbol-names '(:mfa-tool)
         :packages-to-keep '(:mfa-tool :swank)
         :startup-bitmap-file nil) 
