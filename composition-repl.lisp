(in-package :bogu)

(defparameter *allowed-commands* '(quantize def vars seq play rpt rst save help i reset poly sarp del bpm where bogu-load cell fluid transpose free staccato vol synth stop reverb reboot walk seek sync bang live-loop stop-loop engrave retro invert key sweep pan flt wait))

(defun execute-ast (ast)
  "Phase 3: The Symbolic Reducer. Resolves variables and commands into pure music data."
  (let ((master-events nil)
        (local-cursor 0.0))
    ;; SMART WRAPPER: Determines if the AST is a single commanded node or a raw sequence
    (let ((nodes-to-process 
           (if (and (listp ast) 
                    (symbolp (car ast))
                    (or (gethash (car ast) *command-dictionary*)
                        (gethash (car ast) *vars*)
                        (and (note-p (car ast)) (= (length ast) 2) (rtm (cadr ast)))
                        (and (or (eq (car ast) 'RST) (eq (car ast) 'R)) (= (length ast) 2) (rtm (cadr ast)))))
               (list ast) 
               ast)))
      (dolist (node nodes-to-process)
        (let ((result nil))
          (cond
            ;; 1. Variable Lookup (e.g., CEL)
            ((and (symbolp node) (gethash node *vars*))
             (setf result (execute-ast (gethash node *vars*))))

            ;; 1.5 Bare Rest Handling (e.g., RST or R)
            ((and (symbolp node) (or (eq node 'RST) (eq node 'R)))
             (setf result (list (list :type :note :pitch-symbol 'RST :time 0.0 :written-dur 1.0))))

            ;; 1.6 Rest with Rhythm Handling (e.g., (RST H))
            ((and (listp node) (or (eq (car node) 'RST) (eq (car node) 'R))
                  (cdr node) (rtm (cadr node))) ; <-- STRICT CHECK
             (setf result (list (list :type :note :pitch-symbol 'RST :time 0.0 :written-dur (rtm (cadr node))))))

            ;; 2. Bare Note Handling (e.g., C4) -> Defaults to duration 1.0
            ((and (symbolp node) (note-p node))
             (multiple-value-bind (pitch-sym octave) (parse-note-symbol node)
               (setf result (list (list :type :note :pitch-symbol pitch-sym :octave octave :time 0.0 :written-dur 1.0)))))

            ;; 3. NEW: Note with Rhythm Handling! (e.g., (C4 Q) or (F#3 S.))
            ((and (listp node) (symbolp (car node)) (note-p (car node))
                  (cdr node) (rtm (cadr node))) ; <-- STRICT CHECK: MUST actually be a rhythm!
             (multiple-value-bind (pitch-sym octave) (parse-note-symbol (car node))
               (setf result (list (list :type :note :pitch-symbol pitch-sym :octave octave :time 0.0 :written-dur (rtm (cadr node)))))))

            ;; 4. Command Execution (e.g., (SEQ Q C4))
            ((and (listp node) (symbolp (car node))
                  (or (gethash (car node) *command-dictionary*)
                      (gethash (car node) *vars*)))
             (let ((cmd-fn (gethash (car node) *command-dictionary*))
                   (var-val (gethash (car node) *vars*)))
               (cond
                 (cmd-fn (setf result (funcall cmd-fn (cdr node))))
                 (var-val (setf result (execute-ast var-val))))))

            ;; 5. Nested Block Recursion / Uncommanded Raw Pools (e.g. (C4 D4))
            ((listp node)
             (setf result (execute-ast node)))
             
            ;; 6. Unknown 
            (t (format t "~%[Compiler Error] Unrecognized AST node: ~A~%" node)))

          ;; --- THE STITCHER ---
          (when (and result (listp result) (listp (car result))) ; Ensure it is valid musical data
            (let ((block-len 0.0))
              ;; Measure the block
              (dolist (e result)
                ;; THE SAFEGUARD: Fallback to 0.0 if duration is ever somehow NIL
                (setf block-len (max block-len (+ (getf e :time) (or (getf e :written-dur) 0.0)))))
              ;; Shift and Push
              (dolist (e result)
                (let ((new-e (copy-list e)))
                  (setf (getf new-e :time) (+ local-cursor (getf e :time)))
                  (push new-e master-events)))
              (incf local-cursor block-len))))))
    (reverse master-events)))

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
       (execute-ast var-body)
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
          ;; Smart Automatic Semicolon Insertion
          (let* ((len-prev (length trimmed-prev))
                 (last-char-prev (if (> len-prev 0) (char trimmed-prev (1- len-prev)) #\Space))
                 (first-char-next (if (> (length trimmed-next) 0) (char trimmed-next 0) #\Space))
                 (separator (if (or (char= last-char-prev #\[)
                                    (char= first-char-next #\[)
                                    (char= first-char-next #\]))
                                " "      
                                " & ")))
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
                 (commit ast)
                 (push line *bogu-code*)))
           (error (e)
             (format t "~%[Bogu Error] ~A~%" e))))))))

(defun bogu ()
  "Starts the Bogu REPL and routes all input through the Symbolic Compiler."
  (reset-bogu)
  (format t "~%===========================================================")
  (format t "~%                    WELCOME TO BOGU                        ")
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
