(in-package :bogu)

(defparameter *allowed-commands* '(quantize def vars seq play rpt rst save help i reset poly sarp del bpm where bogu-load cell fluid transpose free staccato vol synth stop reverb reboot walk seek sync bang live-loop stop-loop engrave retro invert key sweep pan flt wait))

(defun execute-ast (ast)
  (when ast
    (cond
      ((and (listp ast) (member '& ast))
       (dolist (chunk (split-by-amp ast))
         (execute-ast chunk)))

      ((listp (car ast))
       (dolist (node ast) 
         (execute-ast node))) 

      (t 
       (execute-node ast)))))

(defun execute-node (node)
  "Phase 3: The Engine. Pure Dictionary Dispatcher."
  (when (null node) (return-from execute-node t))

  (let* ((cmd (car node))
         (args (cdr node))
         (var-body (or (and (symbolp cmd) (gethash cmd *vars*))
                       (and (symbolp cmd) (gethash cmd *stdlib-vars*))))
         (dict-cmd (and (symbolp cmd) (gethash cmd *command-dictionary*))))

    (cond
      ;; 1. It's a nested block
      ((listp cmd) (execute-ast node) t)

      ;; 2. It's a User Variable (expand and execute)
      (var-body
       (if (listp (car var-body)) (execute-ast var-body) (execute-node var-body))
       t)

      ;; 3. THE MAGIC: It's a Modular Dictionary Command!
      (dict-cmd
       (funcall dict-cmd args)
       t)

      ;; 4. Completely Unknown
      (t (format t "~%[Compiler Error] Unknown command or variable: ~A~%" cmd)))))

(defun read-bogu-input ()
  "Reads input from the REPL, applies SMART ASI, and ignores comments."
  (let ((input (read-line)))
    (loop while (> (count #\[ input) (count #\] input)) do
      (format t "  > ") 
      (finish-output)
      (let* ((next-line (read-line))
             (trimmed-next (string-trim " " next-line))
             (trimmed-prev (string-trim " " input)))
        (unless (or (string= trimmed-next "")
                    (and (> (length trimmed-next) 0) 
                         (char= (char trimmed-next 0) #\;)))
          
          ;; THE FIX: Smart Automatic Semicolon Insertion
          ;; Only insert '&' if we are NOT opening or closing a block.
          (let* ((len-prev (length trimmed-prev))
                 (last-char-prev (if (> len-prev 0) (char trimmed-prev (1- len-prev)) #\Space))
                 (first-char-next (if (> (length trimmed-next) 0) (char trimmed-next 0) #\Space))
                 (separator (if (or (char= last-char-prev #\[)
                                    (char= first-char-next #\[)
                                    (char= first-char-next #\]))
                                " "      ; <-- Just a space, keeping arguments together!
                                " & "))) ; <-- Add the '&' separator!
            (setf input (concatenate 'string input separator next-line))))))
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
                 (execute-ast ast)
                 (push line *bogu-code*)))
           (error (e)
             (format t "~%[Compiler Error] Could not parse line: ~A~%Details: ~A~%" line e))))))))

(defun bogu ()
  "Initializes the Bogu environment and prompts for project loading."
  (reset-bogu)
  (setf (gethash 1 *synth-rack*) (gethash 'PIANO *synth-templates*))
  (reboot-audio-server)
  (format t "~%===========================================================~%")
  (format t "                    WELCOME TO BOGU                        ~%")
  (format t "===========================================================~%")
  (format t " Type 'help' for a comprehensive list of commands.~%~%")
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
