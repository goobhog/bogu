(defparameter *allowed-commands* '(def vars seq play rpt rst save help i reset poly sarp del bpm where bogu-load play-live start-audio-engine cell fluid transpose vol synth stop))

(defun execute-ast (ast)
  "Walks the Abstract Syntax Tree and executes each command node in sequence."
  (dolist (node ast)
    (execute-node node)))

(defun execute-node (node)
  "Phase 3: The Engine. Evaluates a single cleanly-parsed AST node."
  (when (null node) (return-from execute-node t))

  (let* ((cmd (car node))
         (args (cdr node))
         ;; O(1) Memory Lookup for custom variables
         (var-body (and (symbolp cmd) (gethash cmd *vars*))))
    (cond
      ;; 0. Block Unwrapping (If user typed a naked block like [ poly q c4 ])
      ((listp cmd)
       (execute-ast node)
       t)

      ;; 1. Variable Execution (If you type 'maj', run its saved AST body)
      (var-body
       (if (listp (car var-body))
           (execute-ast var-body)             ;; It's a proper AST tree (from brackets)
           (execute-node var-body))           ;; It's a single flat command
       t)

      ;; 2. Variable Definition (The 'def' command)
      ((eq cmd 'DEF)
       (let* ((var-name (car args))
              (var-contents (cdr args))
              ;; Normalize storage so brackets don't cause double-nesting
              (stored-ast (if (and (= (length var-contents) 1) (listp (car var-contents)))
                              (car var-contents)
                              var-contents)))
         (setf (gethash var-name *vars*) stored-ast)
         (format t "~%[Bound] ~A -> ~A~%" var-name stored-ast)
         t))

      ;; 3. Cellular Time
      ((eq cmd 'CELL)
       (let* ((expanded-args (expand-vars args))
              (dur-val (car expanded-args))
              (cell-duration (rtm dur-val))
              (block (cadr expanded-args))
              (cell-start (current-time))
              (cell-end (+ cell-start cell-duration)))
         (execute-ast block)
         ;; Order out of chaos: snap the current track's playhead to the end of the cell
         (setf (gethash *current-instrument* *playheads*) cell-end)
         t))

      ;; 4. Logic & Iteration
      ((eq cmd 'LOOP)
       (let* ((expanded-args (expand-vars args))
              (n (car expanded-args))
              (block (cadr expanded-args)))
         (dotimes (i n) (execute-ast block))
         t))

      ((eq cmd 'CHANCE)
       (let* ((expanded-args (expand-vars args))
              (p (car expanded-args))
              (true-branch (cadr expanded-args))
              (false-branch (caddr expanded-args)))
         (if (< (random 100) p)
             (execute-ast true-branch)
             (when false-branch (execute-ast false-branch)))
         t))

      ;; 4.1 Instrument Switcher (With Playhead Feedback)
      ((eq cmd 'I)
       (let* ((expanded-args (expand-vars args))
              (id (car expanded-args)))
         (setf *current-instrument* id)
         (format t "~%[TRACK] Switched to Instrument ~A (Current Time: ~,3fs)~%" 
                 id (current-time)))
       t)

      ;; 4.2 Synthesizer Macro-Assembler
      ((eq cmd 'SYNTH)
       (let* ((expanded-args (expand-vars args))
              (id (car expanded-args))
              (template (cadr expanded-args)))
         (let ((code (gethash template *synth-templates*)))
           (if code
               (progn
                 (setf (gethash id *synth-rack*) code)
                 (format t "~%[BIOS] Flashing Instrument ~A with ~A...~%" id template)
                 (start-audio-engine)) ;; Trigger the micro-second reboot!
               (format t "~%[Hardware Error] Unknown synth cartridge: ~A~%" template))))
       t)

      ;; 4.3 Amplitude State
      ((eq cmd 'VOL)
       (let* ((expanded-args (expand-vars args))
              (raw-val (car expanded-args))
              (val (if (numberp raw-val) raw-val (parse-integer (string raw-val)))))
         ;; Convert 0-100 scale to 0.0-1.0 float for Csound
         (setf *velocity* (float (/ val 100.0))))
       t)

      ;; 4.4 Transposition Context (The Lisp Superpower)
      ((eq cmd 'TRANSPOSE)
       (let* ((expanded-args (expand-vars args))
              (raw-offset (car expanded-args))
              ;; Safely handle the integer whether it was passed as a number or a string
              (offset (if (numberp raw-offset) raw-offset (parse-integer (string raw-offset))))
              ;; THE FIX: Use CDR instead of CADR to scoop up the entire flattened block!
              (block (cdr expanded-args)))
         ;; Temporarily shadow the global offset just for the duration of this block!
         (let ((*transpose-offset* (+ *transpose-offset* offset)))
           (if (and (listp block) (listp (car block)))
               (execute-ast block)         ;; It's a bracketed block
               (execute-node block)))      ;; It's a single flat command
         t))

      ;; 4.5 System Commands
      ((eq cmd 'LOAD)
       (bogu-load (car args))
       t)

      ;; The Panic Button (Now kills the background thread!)
      ((eq cmd 'STOP)
       (when (and *play-thread* (sb-thread:thread-alive-p *play-thread*))
         (sb-thread:terminate-thread *play-thread*))
       (format t "~%[PANIC] Halting Audio Engine...~%")
       (start-audio-engine)
       (setf *score* nil)
       (clrhash *playheads*) ;; <--- Eradicated the *itime* ghost!
       t)

      ;; 5. Standard Physics Commands (seq, poly, fluid, note generation)
      ;; We use (symbolp cmd) to armor the note-p check so it never crashes on lists
      ((or (member cmd *allowed-commands*) 
           (and (symbolp cmd) (note-p cmd)))
       (let ((expanded-args (expand-vars args)))
         ;; We quote the expanded args securely right before passing them to Lisp's core eval
         (eval `(,cmd ,@(loop for arg in expanded-args
                              collect (if (numberp arg) arg `(quote ,arg)))))
         t))

      ;; 6. The Safety Net
      (t (format t "~%[Syntax Warning] Unknown command: ~A~%" cmd) nil))))

(defun read-bogu-input ()
  "Reads input from the REPL, applies ASI, and ignores comments."
  (let ((input (read-line)))
    ;; As long as there are more [ than ], keep asking for more lines!
    (loop while (> (count #\[ input) (count #\] input)) do
      (format t "  > ") ;; A subtle prompt to show it's a continuation
      (finish-output)
      (let* ((next-line (read-line))
             (trimmed-line (string-trim " " next-line)))
        ;; Ignore empty lines and REPL comments!
        (unless (or (string= trimmed-line "")
                    (and (> (length trimmed-line) 0) 
                         (char= (char trimmed-line 0) #\;)))
          ;; ASI: Glue the new line with " & " instead of a blank space
          (setf input (concatenate 'string input " & " next-line)))))
    input))

(defun composition-repl ()
  "REPL interface using the full Lexer -> Parser -> AST pipeline."
  (loop
    (format t "~%bogu> ")
    (finish-output)
    (let ((line (read-bogu-input)))
      
      ;; Intercept system commands before they hit the parser
      (when (string= (string-downcase (string-trim " " line)) "quit")
        (return (format t "~%Exiting bogu. Goodbye!~%")))
      (when (string= (string-downcase (string-trim " " line)) "reset")
        (reset-bogu)
        (continue))

      ;; The Compiler Pipeline
      (handler-case
          (let* ((tokens (lex-bogu-string line))
                 (ast (parse-bogu-tokens tokens)))
            (when ast
              (execute-ast ast)
              (push line *bogu-code*)))
        (error (e)
          (format t "~%[Compiler Error] Could not parse line: ~A~%Details: ~A~%" line e))))))

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
