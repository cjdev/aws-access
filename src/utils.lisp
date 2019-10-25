(in-package :mfa-tool)

(defmacro with-retry (form &body restart-cases)
  `(loop (restart-case (return ,form)
           (continue ())
           ,@restart-cases)))

(defmacro with-retry* (bindings &body body)
  `(loop
     (restart-bind (,@bindings)
       ,@(butlast body)
       ,(cons 'return (last body)))))

(defmacro safely-invoke-restart (name condition &rest args)
  (alexandria:with-gensyms (restart)
    `(alexandria:when-let ((,restart (find-restart ,name ,condition)))
       (invoke-restart ,restart ,@args))))
