(defparameter *allowed-commands* '(def vars seq play rpt rst save help i reset poly sarp del % bpm where pas psgs psg bogu-load load))

(defun error-message (cmd)
  "Prints error message followed by the faulty code."
  (format t "~%[Syntax Warning] Unknown symbol or unauthorized command: ~A~%" cmd))

(defun composition-eval (sexp)
  "Tests commands, expanding variables inline before evaluation. Completely bulletproof."
  ;; The Global Shield: Catches ANY error inside the evaluator
  (handler-case
      (let* ((cmd (car sexp))
             (args (cdr sexp))
             (var-lookup (assoc cmd *vars*)))
        (cond
          ;; 0. If they typed a variable name by itself
          (var-lookup
           (let* ((expanded-list (cdr var-lookup))
                  (new-sexp (cons (car expanded-list)
                                  (loop for val in (cdr expanded-list) collect (list 'quote val)))))
             (composition-eval new-sexp)))

          ;; 1. If we are defining a variable, run it directly
          ((eq cmd 'def)
           (eval sexp) t)
           
          ;; 1.5. The LOOP control structure
          ((eq cmd 'loop)
           (let* ((expanded-args (expand-vars args))
                  (raw-n (car expanded-args))
                  (n-val (if (and (listp raw-n) (eq (car raw-n) 'quote)) (cadr raw-n) raw-n))
                  (n (if (numberp n-val) n-val (parse-integer (string n-val))))
                  (raw-body-cmd (cadr expanded-args))
                  (actual-cmd (if (and (listp raw-body-cmd) (eq (car raw-body-cmd) 'quote)) 
                                  (cadr raw-body-cmd) 
                                  raw-body-cmd))
                  (body (cons actual-cmd (cddr expanded-args))))
             (dotimes (i n)
               (composition-eval body))
             t))
           
          ;; 2. Is it a whitelisted command or a note?
          ((or (member cmd *allowed-commands*)
               (note-p cmd))
           (let ((expanded-args (expand-vars args)))
             ;; Smart-quote arguments so Lisp never mistakes data for variables
             (eval `(,cmd ,@(loop for arg in expanded-args 
                                  collect (if (and (listp arg) (eq (car arg) 'quote)) 
                                              arg 
                                              `(quote ,arg)))))
             t))
                 
          ;; 3. Unknown command
          (t (error-message sexp) nil)))
          
    ;; The Catch: If ANYTHING above fails, it lands here cleanly.
    (error (e)
      (format t "~%[Bogu Crash Prevented] ~A~%Failed on: ~A~%" e sexp)
      nil)))

(defun composition-repl ()
  "REPL interface for bogu. Uses iteration instead of recursion for stability."
  (loop
    (format t "~%bogu> ")
    (finish-output) ; Ensures the prompt prints before waiting for input
    (let* ((line (read-line))
           ;; Safely read the line in case of unmatched parentheses or formatting issues
           (cmd (handler-case (bogu-reader line)
                  (error (e)
                    (format t "~%[Reader Error] Could not parse line: ~A~%Details: ~A~%" line e)
                    nil))))
      (when cmd
        (cond ((eq (car cmd) 'quit)
               (return (format t "~%Exiting bogu. Goodbye!~%")))
              ((eq (car cmd) 'reset)
               (reset-bogu))
              (t
               ;; If composition-eval succeeds, push to code history
               (when (composition-eval cmd)
                 (push line *bogu-code*))))))))

(defun bogu ()
  "Initializes the Bogu environment and prompts for project loading."
  (format t "~%===========================================================~%")
  (format t "                    WELCOME TO BOGU                        ~%")
  (format t "===========================================================~%")
  (format t " Type 'help' for a comprehensive list of commands.~%~%")

  (reset-bogu) 
  
  (format t " Type a project name to LOAD, or press ENTER for NEW.~%")
  (format t " Project name: ")
  (finish-output) 
  
  (let ((project (read-line)))
    (if (string= project "")
        (format t "~%Starting new blank project...~%")
        (progn
          (format t "~%Loading ~a...~%" project)
          (bogu-load project))))
          
  (composition-repl))
