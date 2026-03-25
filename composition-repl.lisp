(in-package :bogu)

(defparameter *allowed-commands* '(quantize def vars seq play rpt rst save help i reset poly sarp del bpm where bogu-load cell fluid transpose free staccato vol synth stop reverb reboot walk seek sync bang live-loop stop-loop engrave retro invert key sweep pan wait))

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

      ((eq cmd 'WAIT)
       (let* ((expanded-args (expand-vars args))
              (dur (rtm (car expanded-args))))
         (advance-time dur))
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

      ((eq cmd 'KEY)
       (let* ((expanded-args (expand-vars args)))
         (if (or (null expanded-args) (eq (car expanded-args) 'OFF) (eq (car expanded-args) 'NIL))
             (progn
               (setf *current-key* nil)
               (format t "~%[THEORY] Diatonic Mode OFF (Absolute Chromatic).~%"))
             (let ((tonic (car expanded-args))
                   (mode (cadr expanded-args)))
               (setf *current-key* (list tonic mode))
               (format t "~%[THEORY] Key set to ~A ~A.~%" tonic mode))))
       t)

      ((eq cmd 'PAN)
       (let* ((expanded (expand-vars args))
              (val (/ (car expanded) 100.0)))
         ;; THE FIX: Fire instantly down the pipe! Don't wait for the playhead.
         (osc-control *current-instrument* 2 0.01 val val)
         (format t "~%[MIXER] Track ~A Pan -> ~A%~%" *current-instrument* (car expanded)))
       t)

      ((eq cmd 'VOL)
       (let* ((expanded (expand-vars args))
              (val (/ (car expanded) 100.0)))
         ;; THE FIX: Fire instantly! 
         (osc-control *current-instrument* 1 0.01 val val)
         (format t "~%[MIXER] Track ~A Volume -> ~A%~%" *current-instrument* (car expanded)))
       t)

      ((eq cmd 'REVERB)
       (let* ((expanded (expand-vars args))
              (val (/ (car expanded) 100.0)))
         ;; THE FIX: Fire instantly!
         (osc-control *current-instrument* 3 0.01 val val)
         (format t "~%[MIXER] Track ~A Reverb -> ~A%~%" *current-instrument* (car expanded)))
       t)

      ((eq cmd 'SWEEP)
       (let* ((expanded (expand-vars args))
              (param-sym (car expanded))
              (start (/ (cadr expanded) 100.0))
              (end (/ (caddr expanded) 100.0))
              (dur (rtm (cadddr expanded)))
              (param-id (cond ((eq param-sym 'VOL) 1)
                              ((eq param-sym 'PAN) 2)
                              ((eq param-sym 'REVERB) 3)
                              (t 1))))
         (push (list :type :control :instr *current-instrument* :time (current-time)
                     :dur dur :param param-id :start start :end end) *score*))
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

      ((eq cmd 'RETRO)
       (let* ((block (car args))
              (sandbox-score '())
              (start-time (current-time)))
         
         (let ((*score* '())
               (*playheads* (make-hash-table)))
           (setf (gethash *current-instrument* *playheads*) 0.0)
           
           (cond
             ((symbolp block) (execute-node (list block)))
             ((and (listp block) (listp (car block))) (execute-ast block))
             (t (execute-node block)))
           
           (setf sandbox-score *score*))
           
         (when sandbox-score
           (let* ((max-time (apply #'max (mapcar (lambda (x) (+ (getf x :time) (getf x :dur))) sandbox-score))))
             (dolist (event sandbox-score)
               (let ((new-time (- max-time (+ (getf event :time) (getf event :dur)))))
                 (setf (getf event :time) (+ start-time new-time))
                 (push event *score*)))
             (setf (gethash *current-instrument* *playheads*) (+ start-time max-time)))))
       t)

      ((eq cmd 'INVERT)
       (let* ((block (car args))
              (sandbox-score '())
              (start-time (current-time)))
         
         ;; 1. The Sandbox Capture
         (let ((*score* '())
               (*playheads* (make-hash-table)))
           (setf (gethash *current-instrument* *playheads*) 0.0)
           
           (cond
             ((symbolp block) (execute-node (list block)))
             ((and (listp block) (listp (car block))) (execute-ast block))
             (t (execute-node block)))
           
           ;; Sort chronologically to safely capture the "first note"
           (setf sandbox-score (sort *score* #'< :key (lambda (x) (getf x :time)))))
           
         ;; 2. The Mathematical Inversion
         (when sandbox-score
           ;; Find the first note to act as the "axis" of inversion
           (let* ((first-note (find-if (lambda (x) (eq (getf x :type) :note)) sandbox-score))
                  (axis-st (if first-note 
                               (let* ((p (getf first-note :pch))
                                      (oct (truncate p))
                                      (pc (round (* (- p oct) 100))))
                                 (+ (* oct 12) pc))
                               0))
                  (max-time 0.0))
             
             (dolist (event sandbox-score)
               (when (eq (getf event :type) :note)
                 ;; Convert Csound PCH (e.g., 8.00) to absolute semitones
                 (let* ((p (getf event :pch))
                        (oct (truncate p))
                        (pc (round (* (- p oct) 100)))
                        (current-st (+ (* oct 12) pc))
                        ;; Flip the distance across the axis
                        (diff (- current-st axis-st))
                        (new-st (- axis-st diff))
                        ;; Convert back to Csound PCH
                        (new-oct (truncate new-st 12))
                        (new-pc (mod new-st 12)))
                   (setf (getf event :pch) (float (+ new-oct (/ new-pc 100.0))))))
               
               ;; Shift time forward to the real timeline and push
               (let ((shifted-time (+ start-time (getf event :time))))
                 (setf (getf event :time) shifted-time)
                 (setf max-time (max max-time (+ (- shifted-time start-time) (getf event :dur))))
                 (push event *score*)))
             
             ;; Advance the real playhead
             (setf (gethash *current-instrument* *playheads*) (+ start-time max-time)))))
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
                (duration (or raw-dur (rtm dur-val)))
                ;; FIX 1: CAPTURE THE ENVIRONMENT STATE BEFORE SPAWNING THE THREAD
                (spawn-instrument *current-instrument*)
                (spawn-transpose *transpose-offset*)
                (spawn-key *current-key*))
           
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
                                 ;; FIX 2: APPLY THE QUANTIZATION TARGET!
                                 (next-loop-start-time (calculate-sync-target)))
                            
                            (loop
                              (let ((current-block (gethash name *live-loops*)))
                                (unless current-block (return))
                                ;; FIX 3: RESTORE THE CAPTURED STATE FOR THIS THREAD
                                (let ((*score* '())
                                      (*playheads* (make-hash-table))
                                      (*current-instrument* spawn-instrument)
                                      (*transpose-offset* spawn-transpose)
                                      (*current-key* spawn-key))
                                  
                                  (execute-ast current-block)
                                  (setf *score* (sort *score* #'< :key (lambda (x) (getf x :time))))
                                  
                                  (let* ((current-bpm (if *bpm* (car *bpm*) 60.0))
                                         (sec-per-beat (float (/ 60.0 current-bpm))))
                                    
                                    (dolist (event *score*)
                                      ;; Catch BOTH notes and controls
                                      (when (or (eq (getf event :type) :note)
                                                (eq (getf event :type) :control))
                                        (let* ((event-time-sec (* (getf event :time) sec-per-beat))
                                               (event-dur-sec (* (getf event :dur) sec-per-beat))
                                               (target-ms (+ next-loop-start-time (* event-time-sec internal-time-units-per-second))))
                                          
                                          (loop while (< (get-internal-real-time) target-ms)
                                                do (sleep 0.001))
                                          
                                          ;; Route the data to the correct pipe function!
                                          (if (eq (getf event :type) :note)
                                              (osc-play (getf event :instr) event-dur-sec (getf event :pch) (getf event :vel))
                                              (osc-control (getf event :instr) (getf event :param) event-dur-sec (getf event :start) (getf event :end)))))))
                                  
                                  (let* ((current-bpm (if *bpm* (car *bpm*) 60.0))
                                         (sec-per-beat (float (/ 60.0 current-bpm)))
                                         (loop-dur-sec (* duration sec-per-beat)))
                                    
                                    (incf next-loop-start-time (* loop-dur-sec internal-time-units-per-second))
                                    (loop while (< (get-internal-real-time) next-loop-start-time)
                                          do (sleep 0.001))))))) 
                        (error (e)
                          (format t "~%[FATAL LOOP ERROR in '~A'] ~A~%bogu> " name e)
                          (force-output))))
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

      ;; THE FIX: If it's not a native command, check if it's a Bogu Variable!
      (t (let ((val (gethash cmd *variables*)))
           (if val
               (execute-ast val)
               (format t "~%[Compiler Error] Unknown command or variable: ~A~%" cmd)))))))

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
