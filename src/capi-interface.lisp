(in-package :mfa-tool)

(capi:define-interface mfa-tool ()
  ((assumed-credentials :accessor assumed-credentials :initform (make-hash-table :test 'equal))
   (%default-account :initarg :default-account :reader default-account)
   (%default-region :initarg :default-region :reader default-region)
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
   (role-selector capi:option-pane
                  :items '(:|Developer Role| :|Provisioner Role|)
                  :reader role-selector
                  :selected-item keyword:|Developer Role|)
   (region-selector capi:option-pane
                    :reader region-selector
                    ;; :external-max-width '(character 35)
                    :selection-callback 'region-selected
                    :callback-type :data
                    :selected-item (or %default-region :|us-east-1|)
                    :items (list :|us-east-1| :|us-east-2|
                                 :|us-west-1| :|us-west-2|
                                 :|ca-central-1|
                                 :|eu-central-1|
                                 :|eu-west-1| :|eu-west-2|))
   (open-console-button capi:push-button
                        :enabled nil
                        :selection-callback 'execute-action
                        :callback-type :data-interface
                        :data :|Open Web Console|)
   (action-buttons capi:push-button-panel
                   :items '(:|Authorize iTerm|
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
                   role-selector
                   :separator
                   user-input
                   mfa-input
                   button-layout))
   (action-layout capi:row-layout
                  `(nil
                    action-buttons))
   (webconsole-layout capi:row-layout
                      '(region-selector
                        open-console-button))
   (right-layout capi:column-layout
                 '(output-pane
                   webconsole-layout
                   action-layout))
   (main-layout capi:row-layout
                '(data-layout
                  right-layout)))


  (:default-initargs
   :layout 'main-layout
   :title "CJ AWS Util"))

(defgeneric execute-action (action interface)
  (:method ((action (eql :|Open Web Console|)) (interface mfa-tool))
    (let* ((credentials (current-credentials interface))
           (parser (fw.lu:new 'sts-result-handler credentials))
           (federation-url (url parser))
           (signin-token (gethash "SigninToken"
                                  (yason:parse
                                   (dexador:get federation-url)))))
      (open-url (url-from-signin-token signin-token
                                       (capi:choice-selected-item
                                        (region-selector interface))))))
  (:method ((action (eql :|Authorize iTerm|)) (interface mfa-tool))
    (uiop:run-program (format nil "osascript '~a'"
                              (probe-file
                               (merge-pathnames (make-pathname :name "AuthorizeShell" :type "scpt")
                                                (bundle-resource-root))))))
  (:method ((action (eql :|Cloudformation Stacks|)) (interface mfa-tool))
    (let ((stack-interface (make-instance 'mfa-tool.stack:stack-interface
                                          :credentials (current-credentials interface)
                                          :region (capi:choice-selected-item
                                                   (region-selector interface)))))
      (prog1 (capi:display stack-interface)
        (mfa-tool.store:dispatch stack-interface :|Get Stacks|))))
  (:method ((action (eql :|Lisp REPL|)) (interface mfa-tool))
    (capi:contain (make-instance 'capi:listener-pane)
                  :best-width 1280
                  :best-height 800)))

(defun interface (&rest args &key default-account default-region)
  (declare (ignore default-account))
  (let ((interface (apply 'make-instance 'mfa-tool args)))
    (setf (capi:pane-initial-focus interface)
          (slot-value interface 'mfa-input))
    (capi:set-button-panel-enabled-items (slot-value interface 'action-buttons)
                                         :set nil)
    (capi:display interface)))

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

(defun run (&optional accounts)
  (cl+ssl:reload)
  (setf *print-readably* nil
        *accounts* (reprocess-accounts (load-accounts accounts))
        aws:*session* (mfa-tool.credential-provider:make-aws-session))
  (ubiquitous:restore :cj.mfa-tool)
  (interface
   :default-account (ubiquitous:value :default-account)
   :default-region (or (ubiquitous:value :default-region)
                       :|us-east-1|)))

(defun start-in-repl
    (&optional (accounts (asdf:system-relative-pathname :aws-access "assets/accounts"
                                                        :type "json")))
  (run accounts))

(defun debugging (condition fun)
  (declare (ignore fun))
  (let ((*print-readably* nil)
        (out (make-instance 'capi:collector-pane)))
    (princ condition (capi:collector-pane-stream out))
    (prin1 (mapcar 'restart-name
                   (compute-restarts condition))
           (capi:collector-pane-stream out))
    (dbg:output-backtrace :stream (capi:collector-pane-stream out))
    (terpri)
    (terpri)
    (capi:contain out)
    (dbg:log-bug-form "fail"))
  (abort))

(defun main ()
  (mfa-tool.editor-color-theme:color-theme "zenburn" nil)
  (mfa-tool.credential-provider:setup-default-chain)
  (mfa-tool.pprint-setup:setup-pprint)
  (setf *debugger-hook* 'debugging)
  (capi:set-application-interface (make-instance 'my-app-interface))
  (show-splash)
  (princ (user-homedir-pathname) *standard-output*)
  (run))

(defun cl+ssl::input-wait (_ __ ___)
  (declare (ignore _ __ ___))
  (format t "~&Can't wait for input intelligently, sleeping...~%")
  (sleep 0.5))
(defun cl+ssl::output-wait (_ __ ___)
  (declare (ignore _ __ ___))
  (format t "~&Can't wait for output intelligently, sleeping...~%")
  (sleep 0.5))
