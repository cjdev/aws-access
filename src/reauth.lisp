(in-package :mfa-tool)

(capi:define-interface reauth-interface ()
  ((error-message :initarg :error-message)
   (auth-user :initarg :auth-user))
  (:panes (error-display capi:display-pane
                         :background :transparent
                         :text error-message)
          (user-input capi:text-input-pane
                      :title "User:"
                      :title-position :left
                      :title-args '(:visible-min-width (:character 10))
                      :text auth-user
                      :reader user-input)
          (mfa-input capi:text-input-pane
                     :title "MFA Token:"
                     :title-position :left
                     :max-characters 6
                     :title-args '(:visible-min-width (:character 10))
                     :reader mfa-input))
  (:layouts (main-layout capi:column-layout
                         '(error-display
                           nil
                           user-input
                           mfa-input)
                         :visible-min-width '(:character 50)
                         :adjust :center))

  (:default-initargs
   :layout 'main-layout))

(defun reauth-result (reauth-interface)
  (let ((result (list (capi:text-input-pane-text (user-input reauth-interface))
                      (capi:text-input-pane-text (mfa-input reauth-interface)))))
    (if (and (position #\@ (first result))
             (every 'digit-char-p (second result))
)
        result
      (values result t))))
