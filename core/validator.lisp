;; core/validator.lisp
(in-package :bogu)

;; =============================================================================
;; ENGINE CORE: AST SIGNATURE VALIDATION & MACROS
;; =============================================================================

(defun validate-signature (cmd-name signature args)
  "Traverses the arguments and ensures they match the command's symbolic blueprint."
  (let* ((arg-count (length args))
         (rest-pos (position '&rest signature))
         (required-sigs (if rest-pos (subseq signature 0 rest-pos) signature))
         ;; THE FIX: Force uppercase string conversion so -OPTIONAL is always detected!
         (min-args (count-if-not (lambda (s) (search "-OPTIONAL" (string-upcase (string s)))) required-sigs))
         (max-args (if rest-pos most-positive-fixnum (length signature))))

    ;; 1. Arity Check
    (when (or (< arg-count min-args) (> arg-count max-args))
      (error "[Syntax Error] ~A expects ~A arguments, but received ~A." 
             cmd-name (if (= min-args max-args) min-args (format nil "~A to ~A" min-args (if rest-pos "infinity" max-args))) arg-count))

    ;; 2. Smart State-Machine Type Checking
    (let ((sig-idx 0))
      (loop for provided in args
            for i from 0
            do (let* ((expected (nth sig-idx signature)))
                 (when (eq expected '&rest)
                   (incf sig-idx)
                   (setf expected (nth sig-idx signature)))

                 (let* ((base-type (extract-base-type expected))
                        (is-optional (search "-OPTIONAL" (string-upcase (string expected))))
                        (matches (cond
                                   ((eq base-type :NUMBER) (numberp provided))
                                   ((eq base-type :SYMBOL) (symbolp provided))
                                   ((eq base-type :RHYTHM) (or (numberp provided) (and (symbolp provided) (rtm provided))))
                                   ((eq base-type :AST) (listp provided))
                                   (t t)))) ; :ANY or unknown match automatically
                   
                   (if matches
                       ;; It matches! Advance the pointer (unless we are locked in &rest mode)
                       (unless (and rest-pos (>= sig-idx rest-pos))
                         (incf sig-idx))
                       
                       ;; It doesn't match...
                       (if is-optional
                           ;; Optional skip logic
                           (progn
                             (incf sig-idx)
                             (let* ((next-expected (nth sig-idx signature)))
                               (when (eq next-expected '&rest)
                                 (incf sig-idx)
                                 (setf next-expected (nth sig-idx signature)))
                               (let* ((next-base (extract-base-type next-expected))
                                      (next-matches (cond
                                                      ((eq next-base :NUMBER) (numberp provided))
                                                      ((eq next-base :SYMBOL) (symbolp provided))
                                                      ((eq next-base :RHYTHM) (or (numberp provided) (and (symbolp provided) (rtm provided))))
                                                      ((eq next-base :AST) (listp provided))
                                                      (t t))))
                                 (unless next-matches
                                   (error "[Type Error] ~A expects a ~A at position ~A, but got ~A." cmd-name next-base (1+ i) provided))
                                 ;; Advanced successfully
                                 (unless (and rest-pos (>= sig-idx rest-pos))
                                   (incf sig-idx)))))
                           ;; Not optional, hard error
                           (error "[Type Error] ~A expects a ~A at position ~A, but got ~A." cmd-name base-type (1+ i) provided)))))))
    t))

(defmacro def-bogu-cmd (name signature args &body body)
  "Defines a modular command, registers its signature, and extracts docstrings."
  (let* ((func-name (intern (format nil "CMD-~A" name)))
         (docstring (if (stringp (car body)) (car body) "No documentation available."))
         (actual-body (if (stringp (car body)) (cdr body) body)))
    `(progn
       (defun ,func-name ,args
         (handler-case
             (progn
               (validate-signature ',name ',signature ,(car args))
               ,@actual-body)
           (error (e)
             (format t "~%~A~%" e)
             nil)))
       (setf (gethash ',name *command-dictionary*) 
             (list :fn #',func-name :sig ',signature :doc ,docstring)))))
