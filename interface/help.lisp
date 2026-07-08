;; interface/help.lisp
(in-package :bogu)

(defun help (&optional target)
  "Dynamically generates the Bogu user manual based on the *command-dictionary*."
  (if target
      ;; 1. Specific Command Help (e.g., [ help choose ])
      (let* ((raw-sym (if (listp target) (car target) target))
             (sym (if (symbolp raw-sym) raw-sym (intern (string-upcase (string raw-sym)))))
             (entry (gethash sym *command-dictionary*)))
        (if entry
            (progn
              (format t "~%========================================~%")
              (format t " Command : [ ~A ]~%" sym)
              (format t " Expects : ~A~%" (getf entry :sig))
              (format t "----------------------------------------~%")
              (format t " ~A~%" (getf entry :doc))
              (format t "========================================~%~%"))
            (format t "~%[Help] Unknown command '~A'. Type 'help' for a full list.~%" sym)))
            
      ;; 2. Master Command List
      (progn
        (format t "~%========================================~%")
        (format t "           BOGU COMMAND MANUAL          ~%")
        (format t "========================================~%")
        ;; Get a sorted list of all commands
        (let ((cmd-keys nil))
          (maphash (lambda (k v) (push k cmd-keys)) *command-dictionary*)
          (setf cmd-keys (sort cmd-keys #'string< :key #'symbol-name))
          
          (dolist (k cmd-keys)
            (let* ((entry (gethash k *command-dictionary*))
                   (sig (getf entry :sig)))
              (format t " [ ~A ]~%" k)
              (format t "    -> ~A~%" sig))))
        (format t "========================================~%")
        (format t " Type [ help command-name ] for details.~%~%"))))
