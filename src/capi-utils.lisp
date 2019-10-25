(in-package :mfa-tool)

(defun open-url (url)
  (capi:contain (make-instance 'capi:browser-pane
                               :url url)
                :title "Management Console"
                :best-width 1280
                :best-height 800))
