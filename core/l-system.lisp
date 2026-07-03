;; core/l-system.lisp
(in-package :bogu)

(defparameter *rewrite-rules* (make-hash-table :test 'equal))

(def-bogu-cmd RULE (:ast :ast) (args)
  "Defines a universal functional pattern matcher.
   Syntax: [ RULE [ TRIGGER ?var1 ?var2 ] [ transformation ] ]"
  ;; THE FIX: Flatten the pattern so standalone symbols don't stay trapped in fragmented sub-lists
  (let* ((pattern (flatten (car args))) 
         (transform (cadr args))
         (trigger (car pattern))
         (vars (cdr pattern)))
    (setf (gethash trigger *rewrite-rules*) 
          (list :vars vars :transform transform))
    (format t "~%[L-SYSTEM] Registered functional rule: ~A -> ~A~%" trigger vars))
  nil)

(defun apply-rewrite-rule (node)
  "Executes a matched functional rule by dynamically binding ASTs to pattern variables."
  (let* ((trigger (car node))
         (args (cdr node))
         (rule (gethash trigger *rewrite-rules*))
         (vars (getf rule :vars))
         (transform-ast (getf rule :transform))
         (saved-vars (make-hash-table)))
    
    ;; 1. Save old variable states and bind the new incoming pattern variables
    (loop for var in vars
          for arg in args do
      (when (gethash var *vars*)
        (setf (gethash var saved-vars) (gethash var *vars*)))
      (setf (gethash var *vars*) arg))
    
    ;; 2. Evaluate the transformation inside this isolated environment
    (unwind-protect
         (execute-ast (list transform-ast))
      
      ;; 3. Cleanup: Restore the old environment
      (loop for var in vars do
        (if (gethash var saved-vars)
            (setf (gethash var *vars*) (gethash var saved-vars))
            (remhash var *vars*))))))
