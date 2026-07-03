;; interface/composition-repl.lisp
(in-package :bogu)

(defun read-bogu-input ()
  "Reads input from the REPL, applies SMART ASI, and ignores comments."
  (let ((input (read-line)))
    (loop while (> (count #\[ input) (count #\] input)) do
      ;; Calculate how deep we are in brackets
      (let ((depth (- (count #\[ input) (count #\] input))))
        ;; '>' stays on the far left, followed by 2 spaces per depth level
        (format t "> ~A" (make-string (* 2 depth) :initial-element #\Space))
        (finish-output)
        
        (let* ((next-line (read-line))
               (trimmed-next (string-trim " " next-line)))
          (unless (or (string= trimmed-next "")
                      (and (> (length trimmed-next) 0) 
                           (char= (char trimmed-next 0) #\;)))
            ;; Smart Automatic Semicolon Insertion
            (let* ((separator " "))
              (setf input (concatenate 'string input separator next-line)))))))
    input))

(defun composition-repl ()
  "REPL interface using the full Lexer -> Parser -> AST pipeline."
  (loop
    (format t "~%bogu> ")
    (finish-output)
    (let* ((line (read-bogu-input))
           (cmd-str (string-downcase (string-trim " " line))))
      (cond
        ((string= cmd-str "quit")
         (return (format t "~%Exiting bogu. Goodbye!~%")))
        ((string= cmd-str "reset")
         (reset-bogu))
        ((not (string= cmd-str ""))
         (handler-case
             (let* ((tokens (lex-bogu-string line))
                    (ast (parse-bogu-tokens tokens)))
               (when ast
                 (commit ast)
                 (push line *bogu-code*)))
           (error (e)
             (format t "~%[Bogu Error] ~A~%" e))))))))

(defun bogu ()
  "Starts the Bogu REPL and routes all input through the Symbolic Compiler."
  (reset-bogu)
  (format t "~%===========================================================")
  (format t "~%                     WELCOME TO BOGU                        ")
  (format t "~%===========================================================~%")
  (format t " Type 'help' for a comprehensive list of commands.~%~%")
  (format t " Type a project name to LOAD, or press ENTER for NEW.~%")
  (format t " Project name: ")
  (finish-output)
  (let ((proj (string-trim " " (read-line))))
    (if (not (string= proj ""))
        (bogu-load proj)
        (setf *current-project* nil)))
  (loop
    (format t "~%bogu> ")
    (finish-output)
    (let ((input (string-trim " " (read-bogu-input))))
      (cond
        ((or (string-equal input "quit") (string-equal input "exit"))
         (format t "~%Shutting down Bogu...~%")
         (return))
        ((not (string= input ""))
         (handler-case
             (let* ((tokens (lex-bogu-string input))
                    (ast (parse-bogu-tokens tokens)))
               (when ast
                 (commit ast)))
           (error (e)
             (format t "~%[Syntax Error] ~A~%" e))))))))
