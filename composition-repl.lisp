(in-package :bogu)

(defparameter *allowed-commands* '(quantize def vars seq play rpt rst save help i reset poly sarp del bpm where bogu-load cell fluid transpose free staccato vol synth stop reverb reboot walk seek sync bang live-loop stop-loop engrave))

(defun execute-ast (ast)
  "Walks the Abstract Syntax Tree and executes each command node in sequence."
  (dolist (node ast)
    (execute-node node)))

(defun execute-node (node)
  "Phase 3: The Engine. Evaluates a single cleanly-parsed AST node."
  (when (null node) (return-from execute-node t))

  (let* ((cmd (car node))
         (args (cdr node))
         (user-body (and (symbolp cmd) (gethash cmd *vars*)))
         (std-body (and (symbolp cmd) (gethash cmd *stdlib-vars*)))
         (var-body (or user-body std-body)))
         
    (cond
      ((listp cmd) (execute-ast node) t)

      (var-body
       (if (listp (car var-body))
           (execute-ast var-body)             
           (execute-node var-body))            
       t)

      ((eq cmd 'DEF)
       (let* ((var-name (car args))
              (var-contents (cdr args))
              (stored-ast (if (and (= (length var-contents) 1) (listp (car var-contents)))
                              (car var-contents)
                              var-contents)))
         (setf (gethash var-name *vars*) stored-ast)
         (format t "~%[Bound] ~A -> ~A~%" var-name stored-ast))
       t)

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
         (format t "~%[TRACK] Switched to Instrument ~A~%" id))
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
         (osc-play 98 0.01 0.0 (float (/ val 100.0)))
         (format t "~%[FX] Reverb set to ~A%~%" val))
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

      ;; --- THE UNIFIED CONCEPT BLOCK ---
      ((eq cmd 'STACCATO)
       (let ((*current-articulation* :staccato)) 
         (if (and (listp args) (listp (car args)))
             (execute-ast args)         
             (execute-node args)))
       t)

      ;; --- THE UNMETERED NOTATION FORCEFIELD ---
      ((eq cmd 'FREE)
       (push (list :type :meta :subtype :cadenza-on 
                   :instr *current-instrument* :time (current-time)) *score*)
       (if (and (listp args) (listp (car args)))
           (execute-ast args)         
           (execute-node args))
       (push (list :type :meta :subtype :cadenza-off 
                   :instr *current-instrument* :time (current-time)) *score*)
       (format t "~%[TIMELINE] Unmetered 'free' block executed.~%")
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
             (format t "~%[Syntax Error] engrave requires a filename and a track number.~%")))
       t)

      ((eq cmd 'STOP)
       (when (and *play-thread* (sb-thread:thread-alive-p *play-thread*))
         (sb-thread:terminate-thread *play-thread*))
       (format t "~%[TRANSPORT] Sequence Halted.~%")
       t)

      ((eq cmd 'BANG)
       (let* ((expanded-args (expand-vars args))
              (instr (car expanded-args))
              (note (cadr expanded-args)))
         (bang instr note))
       t)

      ((eq cmd 'QUANTIZE)
       (let* ((expanded-args (expand-vars args))
              (mode (car expanded-args)))
         (cond
           ((string-equal mode "EXACT") (setf *quantize-mode* :exact))
           ((string-equal mode "FREE") (setf *quantize-mode* :free))
           ((numberp mode) (setf *quantize-mode* (float mode)))
           (t (setf *quantize-mode* :exact)))
         (format t "~%[CLOCK] Master Sync Mode set to: ~A~%" *quantize-mode*))
       t)

      ((eq cmd 'LIVE-LOOP)
       (let* ((expanded-args (expand-vars args))
              (name (car expanded-args))
              (dur-val (cadr expanded-args))
              (block (caddr expanded-args)))
         
         (unless (and name dur-val block)
           (format t "~%[Syntax Error] live-loop requires a name, duration, and block.~%")
           (return-from execute-node t))

         (let* ((raw-dur (if (numberp dur-val) dur-val (ignore-errors (parse-integer (string dur-val)))))
                (duration (or raw-dur (rtm dur-val))))
           
           (setf (gethash name *live-loops*) block)
           (unless (and (gethash name *loop-threads*)
                        (sb-thread:thread-alive-p (gethash name *loop-threads*)))
             (format t "~%[LOOP] Armed live-loop '~A' (~A beats). Syncing to Network...~%" name duration)
             (setf (gethash name *loop-threads*)
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case 
                          (let* ((current-bpm (if *bpm* (car *bpm*) 60.0))
                                 (sec-per-beat (float (/ 60.0 current-bpm)))
                                 (loop-dur-sec (* duration sec-per-beat))
                                 (next-loop-start-time (get-internal-real-time)))
                            
                            (loop
                              (let ((current-block (gethash name *live-loops*)))
                                (unless current-block (return))
                                (let ((*score* '())
                                      (*playheads* (make-hash-table))
                                      (*current-instrument* *current-instrument*)
                                      (*transpose-offset* *transpose-offset*))
                                  
                                  (execute-ast current-block)
                                  (setf *score* (sort *score* #'< :key (lambda (x) (getf x :time))))
                                  
                                  (let* ((current-bpm (if *bpm* (car *bpm*) 60.0))
                                         (sec-per-beat (float (/ 60.0 current-bpm))))
                                    
                                    (dolist (event *score*)
                                      ;; THE METADATA SHIELD: Only process actual audio notes
                                      (when (eq (getf event :type) :note)
                                        (let* ((event-time-sec (* (getf event :time) sec-per-beat))
                                               (event-dur-sec (* (getf event :dur) sec-per-beat))
                                               (target-ms (+ next-loop-start-time (* event-time-sec internal-time-units-per-second))))
                                          
                                          (loop while (< (get-internal-real-time) target-ms)
                                                do (sleep 0.001))
                                          
                                          (osc-play (getf event :instr) 
                                                    event-dur-sec 
                                                    (getf event :pch) 
                                                    (getf event :vel))))))
                                  
                                  (let* ((current-bpm (if *bpm* (car *bpm*) 60.0))
                                         (sec-per-beat (float (/ 60.0 current-bpm)))
                                         (loop-dur-sec (* duration sec-per-beat)))
                                    
                                    (incf next-loop-start-time (* loop-dur-sec internal-time-units-per-second))
                                    (loop while (< (get-internal-real-time) next-loop-start-time)
                                          do (sleep 0.001))))))) ;; <--- The missing 7th parenthesis is now correctly placed here!
                        (error (e)
                          (format t "~%[FATAL LOOP ERROR in '~A'] ~A~%bogu> " name e)
                          (force-output)))) ;; <--- The extra parenthesis has been removed from here!
                    :name (format nil "bogu-loop-~A" name))))))
               t)

      ((eq cmd 'STOP-LOOP)
       (let* ((expanded-args (expand-vars args))
              (name (car expanded-args)))
         (if (eq name 'ALL)
             (progn
               (maphash (lambda (k thread)
                          (declare (ignore k))
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

      ((or (member cmd *allowed-commands*) 
           (and (symbolp cmd) (note-p cmd)))
       (let* ((expanded-args (expand-vars args))
              (flat-args (flatten expanded-args)))
         (eval `(,cmd ,@(loop for arg in flat-args
                              collect (if (numberp arg) arg `(quote ,arg))))))
       t)

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
  (reset-bogu) 
  (start-audio-engine)
  (sleep 0.1) 
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
