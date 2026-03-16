(defparameter *allowed-commands* '(def vars seq play rpt rst save help i reset poly sarp del % bpm where pas psgs psg bogu-load load play-live start-audio-engine))

;; Teach Lisp to start a list when it sees '['
(set-macro-character #\[
  (lambda (stream char)
    (declare (ignore char))
    (read-delimited-list #\] stream t)))

;; Teach Lisp to panic if it sees a closing bracket without an opening one
(set-macro-character #\]
  (lambda (stream char)
    (declare (ignore stream char))
    (error "Unmatched close bracket ]")))

(defun error-message (cmd)
  "Prints error message followed by the faulty code."
  (format t "~%[Syntax Warning] Unknown symbol or unauthorized command: ~A~%" cmd))

(defun split-commands (lst delimiter)
  "Splits a flat list into multiple sub-lists based on a delimiter."
  (let ((sub-lists nil)
        (current nil))
    (dolist (item lst)
      (if (eq item delimiter)
          (when current
            (push (reverse current) sub-lists)
            (setf current nil))
          (push item current)))
    (when current
      (push (reverse current) sub-lists))
    (reverse sub-lists)))

(defun eval-block (block)
  "Slices a block by '&' and evaluates each command sequentially."
  (if (listp block)
      ;; If it's a list, slice it up and evaluate each piece
      (dolist (cmd (split-commands block '&))
        (composition-eval cmd))
      ;; If it's not a list, just evaluate it normally
      (composition-eval block)))

(defun composition-eval (sexp)
  "Tests commands, expanding variables inline before evaluation. Completely bulletproof."
  (handler-case
      (let* ((cmd (car sexp))
             (args (cdr sexp))
             ;; Safe lookup: only check for variables if 'cmd' is an actual word, not a list
             (var-lookup (and (symbolp cmd) (assoc cmd *vars*))))
        (cond
          ;; 0.1 Block Unwrapping: If the command is a nested block, send it to the slicer!
          ((listp cmd)
           (eval-block cmd) t)

          ;; 0. Variable Expansion
          (var-lookup
           (let* ((expanded-list (cdr var-lookup))
                  (new-sexp (cons (car expanded-list)
                                  (loop for val in (cdr expanded-list) collect (list 'quote val)))))
             (composition-eval new-sexp)))

          ;; 1. Variable Definition
          ((eq cmd 'def)
           (eval sexp) t)
           
          ;; 1.5. Iteration (LOOP)
          ((eq cmd 'loop)
           (let* ((expanded-args (expand-vars args))
                  ;; Get the number
                  (raw-n (car expanded-args))
                  (n-val (if (and (listp raw-n) (eq (car raw-n) 'quote)) (cadr raw-n) raw-n))
                  (n (if (numberp n-val) n-val (parse-integer (string n-val))))
                  ;; Get the bracketed block
                  (raw-block (cadr expanded-args))
                  (block (if (and (listp raw-block) (eq (car raw-block) 'quote)) (cadr raw-block) raw-block)))
             (dotimes (i n)
               (eval-block block))
             t))

          ;; 1.6. Logic (IF)
          ((eq cmd 'if)
           (let* ((expanded-args (expand-vars args))
                  ;; Peel quote off condition
                  (raw-cond (car expanded-args))
                  (condition (if (and (listp raw-cond) (eq (car raw-cond) 'quote)) (cadr raw-cond) raw-cond))
                  ;; Peel quote off true block
                  (raw-true (cadr expanded-args))
                  (true-branch (if (and (listp raw-true) (eq (car raw-true) 'quote)) (cadr raw-true) raw-true))
                  ;; Peel quote off false block (if it exists)
                  (raw-false (caddr expanded-args))
                  (false-branch (if (and (listp raw-false) (eq (car raw-false) 'quote)) (cadr raw-false) raw-false)))
             
             (if (eval condition)
                 (eval-block true-branch)
                 (when false-branch
                   (eval-block false-branch)))
             t))

          ;; 1.7. Generative Probability (CHANCE)
          ((eq cmd 'chance)
           (let* ((expanded-args (expand-vars args))
                  ;; Peel quote off probability number
                  (raw-p (car expanded-args))
                  (p-val (if (and (listp raw-p) (eq (car raw-p) 'quote)) (cadr raw-p) raw-p))
                  (p (if (numberp p-val) p-val (parse-integer (string p-val))))
                  ;; Peel quote off true block
                  (raw-true (cadr expanded-args))
                  (true-branch (if (and (listp raw-true) (eq (car raw-true) 'quote)) (cadr raw-true) raw-true))
                  ;; Peel quote off false block (if it exists)
                  (raw-false (caddr expanded-args))
                  (false-branch (if (and (listp raw-false) (eq (car raw-false) 'quote)) (cadr raw-false) raw-false)))
             
             (if (< (random 100) p)
                 (eval-block true-branch)
                 (when false-branch
                   (eval-block false-branch)))
             t))
           
          ;; 2. Standard Commands
          ((or (member cmd *allowed-commands*)
               (note-p cmd))
           (let ((expanded-args (expand-vars args)))
             (eval `(,cmd ,@(loop for arg in expanded-args 
                                  collect (if (and (listp arg) (eq (car arg) 'quote)) 
                                              arg 
                                              `(quote ,arg)))))
             t))
                 
          ;; 3. Unknown command
          (t (error-message sexp) nil)))
          
    ;; The Catch
    (error (e)
      (format t "~%[Bogu Crash Prevented] ~A~%Failed on: ~A~%" e sexp)
      nil)))

(defun read-bogu-input ()
  "Reads input from the REPL, waiting if there are open brackets."
  (let ((input (read-line)))
    ;; As long as there are more [ than ], keep asking for more lines!
    (loop while (> (count #\[ input) (count #\] input)) do
          (format t "  > ") ;; A subtle prompt to show it's a continuation
          (finish-output)
          ;; Glue the new line to the end of the previous lines
          (setf input (concatenate 'string input " " (read-line))))
    input))

(defun composition-repl ()
  "REPL interface for bogu. Uses iteration instead of recursion for stability."
  (loop
    (format t "~%bogu> ")
    (finish-output) ; Ensures the prompt prints before waiting for input
    (let* ((line (read-bogu-input))
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
