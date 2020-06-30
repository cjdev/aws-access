;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :aws-access
    :version "0.1.0"
    :description "A simple tool for access to CJ's AWS accounts"
    :author "Ed L <edward@elangley.org>"
    :license "MIT"
    :depends-on (:alexandria
                 :aws-sdk
                 :aws-sdk/services/sts
                 :cells
                 :fset
                 :dexador
                 :quri
                 :cxml
                 :daydreamer
                 :fwoar-lisputils
                 :serapeum
                 :ubiquitous
                 :uiop
                 :xpath
                 :yason)
    :serial t
    :components ((:module "src"
                  :serial t
                  :components ((:file "package")
                               (:file "utils")
                               (:file "capi-utils")
                               (:file "pprint-setup")
                               (:file "editor-color-theme")
                               (:file "store")
                               (:file "aws-dispatcher")
                               (:file "aws-utils")
                               (:file "domain")
                               (:file "objc-utils")
                               (:file "read-credentials")
                               (:file "credential-provider")
                               (:file "mfa-tool")
                               (:file "stack-store")
                               (:file "stack")
                               (:file "capi-interface")))))

(defsystem :aws-access/tests
    :description "Tests for the CJ Access tool"
    :author "Ed L <edward@elangley.org>"
    :license "MIT"
    :depends-on (:aws-access
                 :parachute))
