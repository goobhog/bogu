(defparameter *allowed-commands* '(def vars seq play rpt rst save help i reset poly sarp del bpm where bogu-load play-live start-audio-engine cell fluid transpose vol synth stop reverb reboot walk seek sync bang live-loop stop-loop engrave))

(defun execute-ast (ast)
  "Walks the Abstract Syntax Tree and executes each command node in sequence."
  (dolist (node ast)
    (execute-node node)))

(defun execute-node (node)
  "Phase 3: The Engine. Evaluates a single cleanly-parsed AST node."
  (when (null node) (return-from execute-node t))

  (let* ((cmd (car node))
         (args (cdr node))
         ;; Check user vars first, THEN check standard library
         (user-body (and (symbolp cmd) (gethash cmd *vars*)))
         (std-body (and (symbolp cmd) (gethash cmd *stdlib-vars*)))
         (var-body (or user-body std-body)))
         
    (cond
      ;; 0. Block Unwrapping
      ((listp cmd)
       (execute-ast node)
       t)

      ;; 1. Variable Execution
      (var-body
       (if (listp (car var-body))
           (execute-ast var-body)             
           (execute-node var-body))           
       t)

      ;; 2. Variable Definition
      ((eq cmd 'DEF)
       (let* ((var-name (car args))
              (var-contents (cdr args))
              (stored-ast (if (and (= (length var-contents) 1) (listp (car var-contents)))
                              (car var-contents)
                              var-contents)))
         (setf (gethash var-name *vars*) stored-ast)
         (format t "~%[Bound] ~A -> ~A~%" var-name stored-ast))
       t)

      ;; 3. Cellular Time
      ((eq cmd 'CELL)
       (let* ((expanded-args (expand-vars args))
              (dur-val (car expanded-args))
              (cell-duration (rtm dur-val))
              (block (cadr expanded-args))
              (cell-start (current-time))
              (cell-end (+ cell-start cell-duration)))
         (execute-ast block)
         (setf (gethash *current-instrument* *playheads*) cell-end))
       t)

      ;; 4. Logic & Iteration
      ((eq cmd 'LOOP)
       (let* ((expanded-args (expand-vars args))
              (n (car expanded-args))
              (block (cadr expanded-args)))
         (dotimes (i n) (execute-ast block)))
       t)

      ((eq cmd 'CHANCE)
       (let* ((expanded-args (expand-vars args))
              (p (car expanded-args))
              (true-branch (cadr expanded-args))
              (false-branch (caddr expanded-args)))
         (if (< (random 100) p)
             (execute-ast true-branch)
             (when false-branch (execute-ast false-branch))))
       t)

      ((eq cmd 'I)
       (let* ((expanded-args (expand-vars args))
              (id (car expanded-args)))
         (setf *current-instrument* id)
         (format t "~%[TRACK] Switched to Instrument ~A (Current Time: ~,3fs)~%" 
                 id (current-time)))
       t)

      ((eq cmd 'SYNTH)
       (let* ((expanded-args (expand-vars args))
              (id (car expanded-args))
              (template (cadr expanded-args)))
         (let ((code (gethash template *synth-templates*)))
           (if code
               (progn
                 (setf (gethash id *synth-rack*) code)
                 (format t "~%[BIOS] Instrument ~A memory updated to ~A (Pending Reboot)...~%" id template))
               (format t "~%[Hardware Error] Unknown synth cartridge: ~A~%" template))))
       t)

      ((eq cmd 'REBOOT)
       (format t "~%[BIOS] Flashing Hardware Rack...~%")
       (start-audio-engine)
       t)

      ((eq cmd 'VOL)
       (let* ((expanded-args (expand-vars args))
              (raw-val (car expanded-args))
              (val (if (numberp raw-val) raw-val (parse-integer (string raw-val)))))
         (setf *velocity* (float (/ val 100.0))))
       t)

      ((eq cmd 'REVERB)
       (let* ((expanded-args (expand-vars args))
              (raw-val (car expanded-args))
              (val (if (numberp raw-val) raw-val (parse-integer (string raw-val)))))
         ;; 1. Push to score for offline exports
         (push (list :type :note :instr 98 :time (current-time) :dur 0.01 
                     :pitch 0 :octave 0 :pch (float (/ val 100.0)) :vel 0)
               *score*)
         ;; 2. Instantly change the hardware mixing console in real-time!
         (when (and *csound-process* (sb-ext:process-alive-p *csound-process*))
           (let ((csound-in (sb-ext:process-input *csound-process*)))
             (format csound-in "i 98 0 0.01 ~,2f 0~%" (float (/ val 100.0)))
             (force-output csound-in)))
         (format t "~%[FX] Reverb automation set to ~A%~%" val))
       t)

      ((eq cmd 'TRANSPOSE)
       (let* ((expanded-args (expand-vars args))
              (raw-offset (car expanded-args))
              (offset (if (numberp raw-offset) raw-offset (parse-integer (string raw-offset))))
              (block (cdr expanded-args)))
         (let ((*transpose-offset* (+ *transpose-offset* offset)))
           (if (and (listp block) (listp (car block)))
               (execute-ast block)         
               (execute-node block))))
       t)

      ((eq cmd 'LOAD)
       (bogu-load (car args))
       t)

      ((eq cmd 'ENGRAVE)
       (let* ((expanded-args (expand-vars args))
              (filename (car expanded-args))
              (instr (cadr expanded-args)))
         (if (and filename instr)
             (bogu->ly (string-downcase (string filename)) instr)
             (format t "~%[Syntax Error] engrave requires a filename and a track number. (e.g., engrave \"mysong\" 3)~%")))
       t)

      ((eq cmd 'STOP)
       (when (and *play-thread* (sb-thread:thread-alive-p *play-thread*))
         (sb-thread:terminate-thread *play-thread*))
       (format t "~%[TRANSPORT] Halting Audio Engine...~%")
       (start-audio-engine)
       t)

      ((eq cmd 'BANG)
       (let* ((expanded-args (expand-vars args))
              (instr (car expanded-args))
              (note (cadr expanded-args)))
         (bang instr note))
       t)

      ;; 4.8 The Algorave Looper (Silenced and Validated)
      ((eq cmd 'LIVE-LOOP)
       (let* ((expanded-args (expand-vars args))
              (name (car expanded-args))
              (dur-val (cadr expanded-args))
              (block (caddr expanded-args)))
         
         ;; 1. Safety Check: Did the user forget the duration or the block?
         (unless (and name dur-val block)
           (format t "~%[Syntax Error] live-loop requires a name, duration, and block.~%")
           (format t "Example: live-loop bass 4 [ seq q c4 ]~%")
           (return-from execute-node t))

         (let ((duration (if (numberp dur-val) dur-val (rtm dur-val))))
           (setf (gethash name *live-loops*) block)
           (unless (and (gethash name *loop-threads*)
                        (sb-thread:thread-alive-p (gethash name *loop-threads*)))
             (format t "~%[LOOP] Starting live-loop '~A' (~A beats)...~%" name duration)
             (setf (gethash name *loop-threads*)
                   (sb-thread:make-thread
                    (lambda ()
                      (let ((next-time-internal (get-internal-real-time)))
                        (loop
                          (let ((current-block (gethash name *live-loops*)))
                            (unless current-block (return))
                            (let ((*score* '())
                                  (*playheads* (make-hash-table))
                                  (*current-instrument* *current-instrument*)
                                  (*transpose-offset* *transpose-offset*))
                              
                              ;; 2. THE SILENCER: Redirects all prints from this thread into a black hole!
                              (with-open-stream (*standard-output* (make-broadcast-stream))
                                (execute-ast current-block))
                              
                              (let* ((current-bpm (if *bpm* (car *bpm*) 60.0))
                                     (sec-per-beat (/ 60.0 current-bpm)))
                                (when (and *csound-process* (sb-ext:process-alive-p *csound-process*))
                                  (let ((csound-in (sb-ext:process-input *csound-process*)))
                                    (dolist (event *score*)
                                      (let ((start-sec (* (getf event :time) sec-per-beat))
                                            (dur-sec (* (getf event :dur) sec-per-beat)))
                                        (format csound-in "i ~a ~,3f ~,3f ~,3f ~,2f~%"
                                                (getf event :instr) start-sec dur-sec
                                                (getf event :pch) (getf event :vel))))
                                    (force-output csound-in)))))
                            (let* ((current-bpm (if *bpm* (car *bpm*) 60.0))
                                   (sec-per-beat (/ 60.0 current-bpm))
                                   (loop-dur-sec (* duration sec-per-beat))
                                   (internal-dur (* loop-dur-sec internal-time-units-per-second)))
                              (incf next-time-internal internal-dur)
                              (let ((now (get-internal-real-time)))
                                (if (> next-time-internal now)
                                    (sleep (/ (- next-time-internal now) internal-time-units-per-second))
                                    (setf next-time-internal now))))))))
                    :name (format nil "bogu-loop-~A" name))))))
       t)

      ;; 4.9 The Loop Assassin 
      ((eq cmd 'STOP-LOOP)
       (let* ((expanded-args (expand-vars args))
              (name (car expanded-args)))
         (if (eq name 'ALL)
             (progn
               (maphash (lambda (k thread)
                          (when (and thread (sb-thread:thread-alive-p thread))
                            (sb-thread:terminate-thread thread)))
                        *loop-threads*)
               (clrhash *loop-threads*)
               (clrhash *live-loops*)
               (format t "~%[LOOP] All live loops terminated.~%"))
             (let ((thread (gethash name *loop-threads*)))
               (if thread
                   (progn
                     (when (sb-thread:thread-alive-p thread)
                       (sb-thread:terminate-thread thread))
                     (remhash name *loop-threads*)
                     (remhash name *live-loops*)
                     (format t "~%[LOOP] Terminated '~A'.~%" name))
                   (format t "~%[LOOP Error] No loop named '~A' is running.~%" name)))))
       t)

      ;; 5. Standard Physics Commands
      ((or (member cmd *allowed-commands*) 
           (and (symbolp cmd) (note-p cmd)))
       (let* ((expanded-args (expand-vars args))
              ;; The Universal Safety Net: Flatten all arguments before evaluation
              (flat-args (flatten expanded-args)))
         (eval `(,cmd ,@(loop for arg in flat-args
                              collect (if (numberp arg) arg `(quote ,arg))))))
       t)

      ;; 6. The Safety Net
      (t (format t "~%[Syntax Warning] Unknown command: ~A~%" cmd) nil))))

(defun read-bogu-input ()
  "Reads input from the REPL, applies ASI, and ignores comments."
  (let ((input (read-line)))
    (loop while (> (count #\[ input) (count #\] input)) do
      (format t "  > ") 
      (finish-output)
      (let* ((next-line (read-line))
             (trimmed-line (string-trim " " next-line)))
        (unless (or (string= trimmed-line "")
                    (and (> (length trimmed-line) 0) 
                         (char= (char trimmed-line 0) #\;)))
          (setf input (concatenate 'string input " & " next-line)))))
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
