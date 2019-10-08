(in-package :mfa-tool)

(capi:define-interface mfa-tool ()
  ((assumed-credentials :accessor assumed-credentials :initform (make-hash-table :test 'equal))
   (%default-account :initarg :default-account :reader default-account)
   (%signin-url :accessor signin-url))
  (:panes
   (output-pane capi:collector-pane :reader output
                                    :visible-min-width (list :character 80)
                                    :visible-min-height (list :character 25))
   (go-button capi:push-button :text "Go!" :callback 'go-on)
   (mfa-input capi:text-input-pane
              :title "MFA Token:"
              :title-position :left
              :max-characters 6
              :callback 'go-on
              :title-args '(:visible-min-width (:character 11))
              :reader mfa-input)
   (user-input capi:text-input-pane
               :title "Email:"
               :title-position :left
               :text (format nil "~a@cj.com" (uiop/os:getenv "USER"))
               :title-args '(:visible-min-width (:character 11))
               :reader user-input)
   (account-selector capi:option-pane
                     :print-function 'car
                     :items *accounts*
                     :selected-item (rassoc %default-account *accounts*
                                            :test 'equal)
                     :selection-callback 'account-selected
                     :callback-type :data
                     :reader account-selector)
   (action-buttons capi:push-button-panel
                   :items '(:|Open Web Console|
                            :|Authorize iTerm|
                            :|Cloudformation Stacks|)
                   :selection-callback 'execute-action
                   :callback-type :data-interface)
   (listener-button capi:push-button
                    :data :|Lisp REPL|
                          :callback 'execute-action
                          :callback-type :data-interface))
  (:layouts
   (button-layout capi:row-layout
                  '(nil
                    go-button))
   (data-layout  capi:column-layout
                 '(account-selector
                   :separator
                   user-input
                   mfa-input
                   button-layout))
   (action-layout capi:row-layout
                  `(nil
                    action-buttons))
   (right-layout capi:column-layout
                 '(output-pane
                   action-layout))
   (main-layout capi:row-layout
                '(data-layout 
                  right-layout)))


  (:default-initargs
   :layout 'main-layout
   :title "CJ AWS Util"))

(defgeneric execute-action (action interface)
  (:method ((action (eql :|Open Web Console|)) (interface mfa-tool))
    (open-url (signin-url interface)))
  (:method ((action (eql :|Authorize iTerm|)) (interface mfa-tool))
    (uiop:run-program (format nil "osascript '~a'" 
                              (probe-file 
                               (merge-pathnames (make-pathname :name "AuthorizeShell" :type "scpt") 
                                                (bundle-resource-root))))))
  (:method ((action (eql :|Cloudformation Stacks|)) (interface mfa-tool))
   (let ((stack-interface (make-instance 'mfa-tool.stack:stack-interface
                                         :credentials (current-credentials interface))))
     (mfa-tool.store:dispatch stack-interface :|Get Stacks|)
     (capi:display stack-interface)))
  (:method ((action (eql :|Lisp REPL|)) (interface mfa-tool))
    (capi:contain (make-instance 'capi:listener-pane)
                  :best-width 1280
                  :best-height 800)))

(defun interface (&rest args &key default-account)
  (declare (ignore default-account))
  (let ((interface (apply 'make-instance 'mfa-tool args)))
    (setf (capi:pane-initial-focus interface)
          (slot-value interface 'mfa-input))
    (capi:set-button-panel-enabled-items (slot-value interface 'action-buttons)
                                         :set nil)
    (capi:set-application-interface (make-instance 'my-app-interface))
    (capi:display interface)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun debugging (condition fun)
    (declare (ignore fun))
    (let ((*print-readably* nil)
          (out (make-instance 'capi:collector-pane)))
      (princ condition (capi:collector-pane-stream out))
      (prin1 (mapcar 'restart-name
                     (compute-restarts condition) )
             (capi:collector-pane-stream out))
      (capi:contain out)
      (abort))))

(capi:define-interface my-app-interface (capi:cocoa-default-application-interface)
  ()
  (:menus 
   (edit-menu
    "Edit"
    (undo-component standard-edit-component selection-component)
    :callback-type :interface)
   (window-menu
    "Window"
    (("Close Window" 
      :accelerator "accelerator-w"
      :enabled-function 'close-active-screen-enabled
      :callback 'close-active-screen
      :callback-type nil)))
   (standard-edit-component
    :component
    (("Cut" :callback 'capi:active-pane-cut
            :enabled-function 'capi:active-pane-cut-p)
     ("Copy" :callback 'capi:active-pane-copy
             :enabled-function 'capi:active-pane-copy-p)
     ("Paste" :callback 'capi:active-pane-paste
              :enabled-function 'capi:active-pane-paste-p)))

   (selection-component
    :component
    (("Select All" :callback 'capi:active-pane-select-all)))
   
   (undo-component
    :component
    (("Undo" :data :undo
             :enabled-function 'capi:active-pane-undo-p
             :callback 'capi:active-pane-undo))))

  (:menu-bar edit-menu window-menu))

(defun start-in-repl (&optional (accounts (asdf:system-relative-pathname :aws-access "accounts.json")))
  (ubiquitous:restore :cj.mfa-tool)
  (setf aws:*session* (aws:make-session)
        *print-readably* nil
        *accounts* (reprocess-accounts (load-accounts accounts)))
  (interface :default-account 
             (ubiquitous:value :default-account)))


(defun show-splash (&optional (image (bundle-resource "splash.png")))
  (let* ((image (make-instance 'capi:image-pinboard-object
                               :x 0 :y 0
                               :image image
                               :graphics-args '(:background :gray)))
         (text (make-instance 'capi:item-pinboard-object
                              :x 90 :y 450
                              :text (format nil "( Cloud Access )")
                              :graphics-args '(:font #s(graphics-ports:font
                                                        :font-description #s(graphics-ports:font-description
                                                                             :attributes (:name "LispM-Monospace"
                                                                                          :family "LispM"
                                                                                          :size 36.0d0
                                                                                          :weight :demi
                                                                                          :slant :roman
                                                                                          :pitch :fixed))
                                                        :device-font nil)
                                               :background :transparent)))
         (interface (destructuring-bind ((_ __ width height))
                        (capi:screen-monitor-geometries (capi:convert-to-screen))
                      (declare (ignore _ __))
                      (make-instance 'capi:interface
                                     :best-x (floor (- width 512) 2)
                                     :best-y (floor (- height 512) 2)
                                     :layout (make-instance 'capi:pinboard-layout
                                                            :description (list image text))
                                     :window-styles '(:borderless :internal-borderless :shadowed
                                                      :movable-by-window-background :textured-background)))))


    (prog1 (capi:display interface)
      (mp:process-run-function "foo" ()
                               (lambda ()
                                 (sleep 2)
                                 (capi:apply-in-pane-process interface
                                                             'capi:destroy interface))))))

(defun main ()
  (show-splash)
  (setf *debugger-hook* 'debugging
        *print-readably* nil
        *accounts* (reprocess-accounts (load-accounts)))
  (ubiquitous:restore :cj.mfa-tool)
  (let ((*debugger-hook* 'debugging))
    (setf aws:*session* (aws:make-session))
    (interface :default-account 
               (ubiquitous:value :default-account))))
