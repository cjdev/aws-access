(in-package :mfa-tool)
(defpackage :mfa-tool.stack
  (:use :cl)
  (:export #:stack-interface
           #:format-stack-status))
(in-package :mfa-tool.stack)

(defun dispatch-with-action-creator (action-creator)
  (lambda (store data)
    (mfa-tool.store:dispatch store
                             (funcall action-creator data))))

(defmacro with-pp ((pane) &body body)
  `(capi:apply-in-pane-process ,pane
                               (lambda ()
                                 ,@body)))

(defun human-readable-stack-status (stack)
  (nstring-capitalize
   (substitute #\space #\_
               (string (daydreamer.aws-result:stack-status stack)))))
(defun format-stack-status (stream stack &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (princ (human-readable-stack-status stack)
         stream))

(defun get-output-columns (type col1 col2)
  `((:title ,(format nil "~a Name" type)
     :adjust :right
     :width (character ,(max (+ (length type) 5) col1)))
    (:title "Value"
     :adjust :left
     :width (character ,(max (+ (length type) 5) col2)))))

;; doesn't work, seems to cause segfaults, but would be a cool feature :(
(defun go-to-console (interface stack)
  (with-pp (interface)
    (mfa-tool::open-url
     (format nil "https://us-west-2.console.aws.amazon.com/cloudformation/home?region=us-west-2#/stacks/stackinfo?stackId=~a"
             (quri:url-encode (daydreamer.aws-result:stack-id stack))))))

(capi:define-interface stack-interface (capi:interface mfa-tool.stack-store:stack-store)
  ()
  (:panes
   (region-chooser capi:option-pane
                   :reader region-chooser
                   ;; :external-max-width '(character 35)
                   :items (list :|us-east-1| :|us-east-2|
                                :|us-west-1| :|us-west-2|
                                :|ca-central-1|
                                :|eu-central-1|
                                :|eu-west-1| :|eu-west-2|)
                   :selected-item (mfa-tool.aws-dispatcher::region capi:interface)
                   :selection-callback (dispatch-with-action-creator 'mfa-tool.aws-dispatcher::update-region)
                   :callback-type :interface-data)

   (stack-chooser capi:list-panel
                  :reader stack-chooser
                  ;; :external-max-width '(character 35)
                  :items ()
                  :print-function 'daydreamer.aws-result:stack-name
                  :selection-callback (dispatch-with-action-creator 'mfa-tool.aws-dispatcher:select-stack)
                  :callback-type :interface-data)

   (refresh-status capi:push-button
                   :text "Refresh"
                   :callback-type :interface
                   :callback (lambda (store)
                               (mfa-tool.store:dispatch store :refresh)))
   #+(or)
   (open-web-console capi:push-button
                     :text "Open"
                     :callback-type :interface
                     :callback (lambda (store)
                                 (go-to-console
                                  (mfa-tool.stack-store:selected-stack store))))
   (status-display capi:display-pane
                   :background :transparent
                   :reader status-display
                   :text "")

   (outputs-display capi:multi-column-list-panel
                    :columns (get-output-columns "Output" 10 10)
                    :header-args (list :selection-callback :sort)
                    :sort-descriptions (list (capi:make-sorting-description
                                              :type "Output Name"
                                              :key 'car
                                              :sort 'string-lessp
                                              :reverse-sort 'string-greaterp)
                                             (capi:make-sorting-description
                                              :type "Value"
                                              :key 'cadr
                                              :sort 'string-lessp
                                              :reverse-sort 'string-greaterp))
                    :items nil
                    :vertical-scroll t
                    :reader outputs-display
                    :visible-min-height '(character 10)
                    :visible-min-width '(character 50))
   (parameters-display capi:multi-column-list-panel
                       :columns (get-output-columns "Parameter" 10 10)
                       :header-args (list :selection-callback :sort)
                       :sort-descriptions (list (capi:make-sorting-description
                                                 :type "Parameter Name"
                                                 :key 'car
                                                 :sort 'string-lessp
                                                 :reverse-sort 'string-greaterp)
                                                (capi:make-sorting-description
                                                 :type "Value"
                                                 :key 'cadr
                                                 :sort 'string-lessp
                                                 :reverse-sort 'string-greaterp))
                       :items nil
                       :vertical-scroll t
                       :reader parameters-display
                       :visible-min-height '(character 10)
                       :visible-min-width '(character 50)))
  (:layouts
   (key-layout capi:column-layout
               '(region-chooser
                 stack-chooser)
               :visible-max-width '(character 35))
   (status-layout capi:row-layout
                  '(status-display
                    nil
                    #+(or)open-web-console
                    refresh-status)
                  :adjust :center)
   (attribute-layout capi:column-layout
                     '(status-layout
                       parameters-display
                       outputs-display))
   (main-layout capi:row-layout
                '(key-layout
                  attribute-layout)))
  (:default-initargs
   :layout 'main-layout
   :title "Stack Explorer"
   :visible-min-width 800))

(defmethod mfa-tool.store:execute :after ((interface stack-interface) (_ mfa-tool.aws-dispatcher:update-stacks))
  (with-pp (interface)
    (with-accessors ((stack-chooser stack-chooser)) interface
      (setf (capi:collection-items stack-chooser) (mfa-tool.stack-store:available-stacks interface)
            (capi:choice-selected-item
             (stack-chooser interface)) (mfa-tool.stack-store:selected-stack interface)))))

(defun max-widths (cols)
  (loop for (col1 col2) in cols
        maximizing (length col1) into len1
        maximizing (length col2) into len2
        finally (return (list len1 len2))))

(defmethod mfa-tool.store:execute :after ((interface stack-interface) (_ mfa-tool.aws-dispatcher:select-stack))
  (with-pp (interface)
    (with-accessors ((status-display status-display) (selected-stack mfa-tool.stack-store:selected-stack)
                     (outputs-display outputs-display) (outputs mfa-tool.stack-store:outputs)
                     (parameters-display parameters-display) (parameters mfa-tool.stack-store:parameters))
        interface
      (capi:modify-multi-column-list-panel-columns
       outputs-display :columns (apply 'get-output-columns "Output" (max-widths outputs)))
      (capi:modify-multi-column-list-panel-columns
       parameters-display :columns (apply 'get-output-columns "Parameter" (max-widths parameters)))

      (when (not (eq (capi:choice-selected-item (stack-chooser interface))
                     (mfa-tool.aws-dispatcher:stack _)))
        (setf (capi:choice-selected-item (stack-chooser interface)) (mfa-tool.stack-store:selected-stack interface)))
      (setf (capi:display-pane-text status-display)
            (format nil "~a: ~/mfa-tool.stack:format-stack-status/"
                    (daydreamer.aws-result:stack-name selected-stack)
                    selected-stack)
            (capi:collection-items parameters-display) parameters
            (capi:collection-items outputs-display) outputs))))
