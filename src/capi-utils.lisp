(in-package :mfa-tool)

(defun open-url (url)
  (capi:contain (make-instance 'capi:browser-pane
                               :url url)
                :title "Management Console"
                :best-width 1280
                :best-height 800)
  #+(or)
  (let* ((history (make-instance 'capi:list-panel
                                 :items (list (make-instance 'capi:item
                                                             :text "start"
                                                             :data "https://foo.com"))
                                 :selection-callback
                                 (lambda (data interface)
                                   (capi:display-message "~S" data))
                                 :visible-max-width '(:character 20)))
         (browser (make-instance 'capi:browser-pane
                                 :url url
                                 :before-navigate-callback
                                 (lambda (pane url &key &allow-other-keys)
                                   (declare (ignore pane))
                                   (capi:apply-in-pane-process-if-alive
                                    history
                                    (lambda ()
                                      (capi:append-items history
                                                         (list (make-instance 'capi:item
                                                                              :collection history
                                                                              :data url))))))
                                 :navigate-complete-callback
                                 (lambda (pane url _)
                                   (declare (ignore _))
                                   (capi:apply-in-pane-process-if-alive
                                    history
                                    (lambda ()
                                      (setf (capi:collection-items history)
                                            (concatenate 'simple-vector
                                                         (capi:collection-items history)
                                                         (list (make-instance 'capi:item
                                                                              :text
                                                                              (capi:browser-pane-title pane)
                                                                              :data url)))))))
                                 :new-window-callback
                                 (lambda (pane url &key &allow-other-keys)
                                   (capi:apply-in-pane-process-if-alive
                                    pane 'capi:browser-pane-navigate pane url)
                                   nil)
                                 ))
         (browser-interface (make-instance 'capi:row-layout
                                           :description (list history browser))))
    (capi:contain browser-interface
                  :title "Management Console"
                  :best-width 1280
                  :best-height 800)))
