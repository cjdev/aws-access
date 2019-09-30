;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :aws-access 
    :description "A simple tool for access to CJ's AWS accounts"
    :author "Ed L <edward@elangley.org>"
    :license "MIT"
    :depends-on (:alexandria
                 :aws-sdk
                 :aws-sdk/services/sts
                 :cells
                 :cl-yaml
                 :fwoar-lisputils
                 :serapeum
                 :ubiquitous
                 :uiop
                 :yason
                 :cxml
                 :xpath)
    :serial t
    :components ((:module "src"
                  :serial t
                  :components ((:file "package")
                               (:file "domain")
                               (:file "mfa-tool")
                               (:file "capi-interface")))))
