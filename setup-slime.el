(defun lw-aws-access ()
  (interactive nil)
  (let ((inferior-lisp-program (expand-file-name "CJAWSAccess.app/Contents/MacOS/lw"
                                                 (file-name-as-directory (projectile-project-root)))))
    (slime-start)))
