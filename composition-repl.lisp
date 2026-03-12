
(defparameter *allowed-commands* '(seq play rpt rst save help i reset poly sarp del % defpoly bpm where polys defseq bogu-load seqs pas psgs psg))

(defun error-message (cmd)
  "Prints error message followed by the faulty code."
  (format t "~%[Syntax Warning] Unknown symbol or unauthorized command: ~A~%" cmd))

(defun composition-eval (sexp)
  "Tests commands against the whitelist OR the note-p checker."
  (let ((cmd (car sexp)))
    (if (or (member cmd *allowed-commands*)
            (note-p cmd))
        (handler-case
            (progn (eval sexp) t)
          (error (e)
            (format t "~%[Execution Error] ~A~%Failed on: ~A~%" e sexp)
            nil))
        (progn (error-message sexp) nil))))

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
  "Initiates bogu."
   (format t "Welcome to bogu~%Type 'help' for a comprehensive list of commands.~%~%")
   (reset-bogu)
   (composition-repl))
