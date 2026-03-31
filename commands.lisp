(in-package :bogu)

(defmacro def-bogu-cmd (name args &body body)
  "A macro that defines a modular command and automatically registers it in the dictionary."
  (let ((func-name (intern (format nil "CMD-~A" name))))
    `(progn
       (defun ,func-name ,args
         ,@body)
       (setf (gethash ',name *command-dictionary*) #',func-name))))

(def-bogu-cmd SEQ (args)
  "Generates a sequential list of events. If first arg is a rhythm, it overrides the block."
  (let* ((expanded (expand-vars args))
         ;; Safely check if the first element is a master rhythm (like Q)
         (rhythm (if (and (atom (car expanded)) (numberp (rtm (car expanded)))) (rtm (car expanded)) nil))
         (nodes (if rhythm (cdr expanded) expanded))
         (master-events nil)
         (local-cursor 0.0))
    (dolist (node nodes)
      (let ((evaluated-block (execute-ast (list node)))
            (block-len 0.0))
        ;; 1. Measure the block's true length BEFORE shifting it!
        (dolist (e evaluated-block)
          (let ((effective-dur (if rhythm rhythm (getf e :written-dur))))
            (setf block-len (max block-len (+ (getf e :time) effective-dur)))))
        ;; 2. Shift the events and apply master rhythm
        (dolist (e evaluated-block)
          (let ((new-e (copy-list e)))
            (when rhythm (setf (getf new-e :written-dur) rhythm)) 
            (setf (getf new-e :time) (+ local-cursor (getf e :time)))
            (push new-e master-events)))
        ;; 3. Advance cursor by the true measured length of the block
        (incf local-cursor block-len)))
    (reverse master-events)))

(def-bogu-cmd POLY (args)
  "Simultaneous evaluation. Stacks all elements vertically at local time 0.0."
  (let* ((expanded (expand-vars args))
         (rhythm (if (and (atom (car expanded)) (numberp (rtm (car expanded)))) (rtm (car expanded)) nil))
         (nodes (if rhythm (cdr expanded) expanded))
         (master-events nil))
    (dolist (node nodes)
      (let ((evaluated-block (execute-ast (list node))))
        (dolist (e evaluated-block)
          (let ((new-e (copy-list e)))
            (when rhythm (setf (getf new-e :written-dur) rhythm))
            ;; POLY leaves :time alone, stacking everything!
            (push new-e master-events)))))
    (reverse master-events)))

(def-bogu-cmd SARP (args)
  "Arpeggiator that loops over a pool of notes to fill a target duration (s)."
  (let* ((expanded (expand-vars args))
         (r (rtm (car expanded)))   ; Step duration
         (s (rtm (cadr expanded)))  ; Target total duration
         (raw-nodes (cddr expanded))
         (nodes (if (and (= (length raw-nodes) 1) (listp (car raw-nodes)))
                    (car raw-nodes)
                    raw-nodes))
         (len (length nodes))
         (local-cursor 0.0)
         (master-events nil)
         ;; Calculate exactly how many steps fit in the target duration
         (iterations (floor s r))) 
         
    (dotimes (i iterations)
      ;; Use modulo to loop back to the start of the pool if we run out of notes
      (let ((node (nth (mod i len) nodes)))        
        (let ((evaluated-block (execute-ast (list node)))
              (physics-dur (max 0.1 (+ 0.5 (- s (* r i))))))
          (dolist (e evaluated-block)
            (let ((new-e (copy-list e)))
              (setf (getf new-e :time) (+ local-cursor (getf e :time)))
              (setf (getf new-e :written-dur) r)
              (setf (getf new-e :dur) physics-dur) 
              (push new-e master-events)))))
      (incf local-cursor r))
      
    ;; Explicitly anchor the end of the block so the Stitcher knows EXACTLY where 
    ;; the 15.0 boundary is, just like FLUID does!
    (push (list :type :note :pitch-symbol 'RST :time s :written-dur 0.0) master-events)
    
    (reverse master-events)))

(def-bogu-cmd FLUID (args)
  "Generates an aleatoric cloud of notes over a specific duration (r)."
  (let* ((expanded (expand-vars args))
         (density (if (numberp (car expanded)) (car expanded) (parse-integer (string (car expanded)))))
         (r (rtm (cadr expanded)))
         (raw-nodes (cddr expanded))
         ;; THE FIX: Unwrap the pool!
         (nodes (if (and (= (length raw-nodes) 1) (listp (car raw-nodes))) 
                    (car raw-nodes) 
                    raw-nodes))
         (len (length nodes))
         (master-events nil))
    (dotimes (i density)
      (let* ((random-node (nth (random len) nodes))
             (random-offset (* r (/ (random 1000) 1000.0))))
        (let ((evaluated-node (execute-ast (list random-node))))
          (dolist (e evaluated-node)
            (let ((new-e (copy-list e)))
              (setf (getf new-e :time) (+ random-offset (getf e :time)))
              
              ;; Mathematically chop bleeding notes so they don't stretch the loop!
              (when (> (+ (getf new-e :time) (getf new-e :written-dur)) r)
                (setf (getf new-e :written-dur) (max 0.0 (- r (getf new-e :time)))))
              (when (getf new-e :dur)
                (when (> (+ (getf new-e :time) (getf new-e :dur)) r)
                  (setf (getf new-e :dur) (max 0.0 (- r (getf new-e :time))))))
              
              ;; Only push the note if it hasn't been completely chopped away
              (when (> (getf new-e :written-dur) 0)
                (push new-e master-events)))))))
    (push (list :type :note :pitch-symbol 'RST :time r :written-dur 0.0) master-events)
    (reverse master-events)))

(def-bogu-cmd WALK (args)
  "Generates a random walk through a sequence of notes."
  (let* ((expanded (expand-vars args))
         (steps (if (numberp (car expanded)) (car expanded) (parse-integer (string (car expanded)))))
         (r (rtm (cadr expanded)))
         (raw-nodes (cddr expanded))
         ;; THE FIX: Unwrap the pool!
         (nodes (if (and (= (length raw-nodes) 1) (listp (car raw-nodes))) 
                    (car raw-nodes) 
                    raw-nodes))
         (len (length nodes))
         (current-idx (random len))
         (local-cursor 0.0)
         (master-events nil))
    (dotimes (i steps)
      (let ((node (nth current-idx nodes)))
        (let ((evaluated-node (execute-ast (list node))))
          (dolist (e evaluated-node)
            (let ((new-e (copy-list e)))
              (setf (getf new-e :time) (+ local-cursor (getf e :time)))
              (setf (getf new-e :written-dur) r)
              (push new-e master-events)))))
      (incf local-cursor r)
      (let ((step (- (random 3) 1)))
        (setf current-idx (+ current-idx step))
        (setf current-idx (max 0 (min (- len 1) current-idx)))))
    (reverse master-events)))

(def-bogu-cmd CELL (args)
  "A strict time-window. Evaluates a block, truncates anything that bleeds over, and forces the footprint to exactly cell-duration."
  (let* ((expanded (expand-vars args))
         (cell-duration (rtm (car expanded)))
         (block (cadr expanded))
         (evaluated-block (execute-ast (list block)))
         (master-events nil))
    (dolist (e evaluated-block)
      ;; Only keep events that start BEFORE the cell dies
      (when (< (getf e :time) cell-duration)
        (let ((new-e (copy-list e)))
          ;; If the note bleeds past the cell wall, mathematically chop its written duration!
          (when (> (+ (getf new-e :time) (getf new-e :written-dur)) cell-duration)
            (setf (getf new-e :written-dur) (- cell-duration (getf new-e :time))))
          (push new-e master-events))))
    ;; Append a dummy rest at the cell wall so the parent SEQ knows exactly where to place the next block
    (push (list :type :note :pitch-symbol 'RST :time cell-duration :written-dur 0.0) master-events)
    (reverse master-events)))

(def-bogu-cmd SIM (args)
  "Parallel execution. Evaluates multiple blocks simultaneously. Structurally identical to POLY, but semantically designed for layering larger sequences."
  (let* ((master-events nil))
    (dolist (block args)
      (let ((evaluated-block (execute-ast (list block))))
        (dolist (e evaluated-block)
          ;; Stack everything at 0.0
          (push (copy-list e) master-events))))
    (reverse master-events)))

(def-bogu-cmd INVERT (args)
  "Purely inverts the raw pitch symbols of a block around its first note, before diatonic math is applied."
  (let* ((expanded (expand-vars args))
         (block (car expanded))
         (evaluated-stream (execute-ast (list block)))
         (first-note (find-if (lambda (x) (eq (getf x :type) :note)) evaluated-stream)))
    
    (if (null first-note)
        evaluated-stream ; Return empty/rests if no notes exist
        
        (let* ((axis-sym (getf first-note :pitch-symbol))
               (axis-oct (getf first-note :octave))
               ;; Calculate the absolute semitone of the axis note
               (axis-semitone (+ (* axis-oct 12) (cdr (assoc axis-sym *notes*)))))
               
          (mapcar (lambda (event)
                    (if (eq (getf event :type) :note)
                        (let* ((new-event (copy-list event))
                               (sym (getf new-event :pitch-symbol))
                               (oct (getf new-event :octave)))
                          
                          ;; Safely skip rests
                          (unless (or (eq sym 'R) (eq sym 'RST))
                            (let* ((current-semitone (+ (* oct 12) (cdr (assoc sym *notes*))))
                                   (diff (- current-semitone axis-semitone))
                                   ;; The flip
                                   (new-semitone (- axis-semitone diff))
                                   ;; Convert back to Symbol and Octave
                                   (new-oct (floor new-semitone 12))
                                   (new-pc (mod new-semitone 12))
                                   (new-sym (car (find-if (lambda (pair) (= (cdr pair) new-pc)) *notes*))))
                              
                              (setf (getf new-event :pitch-symbol) new-sym)
                              (setf (getf new-event :octave) new-oct)))
                          new-event)
                        (copy-list event)))
                  evaluated-stream)))))

(def-bogu-cmd CHANCE (args)
  "Usage: (CHANCE 0.5 (SEQ C4 E4 G4)). Keeps elements based on probability."
  (let* ((expanded (expand-vars args))
         (probability (car expanded))
         (music-block (cadr expanded))
         (evaluated-stream (execute-ast (list music-block)))
         (filtered-stream nil))
    (dolist (event evaluated-stream)
      (when (<= (/ (random 100) 100.0) probability)
        (push event filtered-stream)))
    (reverse filtered-stream)))

(def-bogu-cmd CHOOSE (args)
  "Usage: (CHOOSE 0.6 BlockA BlockB). Picks BlockA 60% of the time, else BlockB."
  (let* ((expanded (expand-vars args))
         (probability (car expanded))
         (block-a (cadr expanded))
         (block-b (caddr expanded)))
    (if (<= (/ (random 100) 100.0) probability)
        (execute-ast (list block-a))
        (if block-b (execute-ast (list block-b)) nil))))

(def-bogu-cmd RPT (args)
  "Data Loop. Evaluates the AST once, then pastes exact time-shifted copies."
  (let* ((expanded (expand-vars args))
         (iterations (car expanded))
         (body (cadr expanded))
         (blueprint-events (execute-ast (list body)))
         (master-events nil)
         (local-cursor 0.0)
         (blueprint-len 0.0))
         
    ;; Find total length of blueprint
    (dolist (e blueprint-events)
      (setf blueprint-len (max blueprint-len (+ (getf e :time) (getf e :written-dur)))))
        
    ;; Stamp out the copies
    (dotimes (i iterations)
      (dolist (e blueprint-events)
        (let ((new-event (copy-list e)))
          (setf (getf new-event :time) (+ local-cursor (getf e :time)))
          (push new-event master-events)))
      (incf local-cursor blueprint-len))
    (reverse master-events)))

(def-bogu-cmd RETRO (args)
  "Reverses the timeline of a block."
  (let* ((expanded (expand-vars args))
         (block (car expanded))
         (evaluated-stream (execute-ast (list block)))
         (max-time 0.0))
         
    ;; Find the end of the block
    (dolist (e evaluated-stream)
       (setf max-time (max max-time (+ (getf e :time) (getf e :written-dur)))))
       
    ;; Invert the start times!
    (mapcar (lambda (e)
              (let ((new-e (copy-list e)))
                (setf (getf new-e :time) (- max-time (+ (getf e :time) (getf e :written-dur))))
                new-e))
            evaluated-stream)))

;; --- ARRANGEMENT & LOGIC ---
(def-bogu-cmd DEF (args)
  (let* ((var-name (car args))
         (var-contents (cdr args))
         (stored-ast (if (and (= (length var-contents) 1) (listp (car var-contents)))
                         (car var-contents) var-contents)))
    (setf (gethash var-name *vars*) stored-ast)
    nil)) ; <-- Force a nil return so the AST stitcher ignores it!

(def-bogu-cmd I (args)
  (setf *current-instrument* (car (expand-vars args)))
  nil)

(def-bogu-cmd KEY (args)
  (let* ((expanded (expand-vars args))
         (trk (get-current-track)))
    (if (or (null expanded) (eq (car expanded) 'OFF) (eq (car expanded) 'NIL))
        (setf (track-key trk) nil) 
        (setf (track-key trk) (list (car expanded) (cadr expanded))))
    nil))

;; --- SYSTEM COMMAND DICTIONARY WRAPPERS ---
(def-bogu-cmd BPM (args) (apply #'bpm (expand-vars args)) nil)
(def-bogu-cmd PLAY (args) (apply #'play (expand-vars args)) nil)
(def-bogu-cmd SAVE (args) (apply #'save (expand-vars args)) nil)
(def-bogu-cmd VARS (args) (apply #'vars (expand-vars args)) nil)
(def-bogu-cmd WHERE (args) (apply #'where (expand-vars args)) nil)
(def-bogu-cmd HELP (args) (apply #'help (expand-vars args)) nil)
(def-bogu-cmd RESET (args) (apply #'reset (expand-vars args)) nil)
(def-bogu-cmd LOAD (args) (apply #'bogu-load (expand-vars args)) nil)
(def-bogu-cmd DEL (args) (apply #'del (expand-vars args)) nil)
(def-bogu-cmd SEEK (args) (apply #'seek (expand-vars args)) nil)
(def-bogu-cmd SYNC (args) (apply #'sync (expand-vars args)) nil)
(def-bogu-cmd BANG (args) (apply #'bang (expand-vars args)) nil)
(def-bogu-cmd SYNTH (args) (apply #'synth (expand-vars args)) nil)


(def-bogu-cmd WAIT (args)
  "Generates a pure rest of the specified duration. Defaults to 1.0 (Q)."
  (let* ((expanded (expand-vars args))
         (dur (if expanded (rtm (car expanded)) 1.0)))
    (list (list :type :note :pitch-symbol 'RST :time 0.0 :written-dur dur))))

(def-bogu-cmd IF (args)
  (let* ((expanded (expand-vars args))
         (val1 (car expanded))
         (op-sym (cadr expanded))
         (val2 (caddr expanded))
         (op-fn (cond ((eq op-sym '=) #'=) ((eq op-sym '>) #'>) ((eq op-sym '<) #'<)
                      ((eq op-sym '>=) #'>=) ((eq op-sym '<=) #'<=) ((eq op-sym '!=) #'/=) (t nil))))
    (if (and op-fn (numberp val1) (numberp val2))
        (if (funcall op-fn val1 val2)
            (execute-ast (nth 3 expanded))
            (when (nth 4 expanded) (execute-ast (nth 4 expanded))))
        (format t "~%[Logic Error] Invalid IF syntax.~%"))))

;; --- MIXER MACROS ---
(defmacro def-mixer-cmd (name param-id)
  "Safely generates pure static control data AND sends instant top-level initialization."
  `(def-bogu-cmd ,name (args)
     (let* ((expanded (expand-vars args))
            (val (/ (float (car expanded)) 100.0)))
       ;; 1. INSTANT ACTION: Fire immediately to initialize the track!
       (osc-control *current-instrument* ,param-id 0.01 val val)
       ;; 2. SYMBOLIC ACTION: Return pure data for the AST sequencer!
       (list (list :type :control :time 0.0 :written-dur 0.0 :dur 0.01 :param ,param-id :start val :end val)))))

(def-mixer-cmd VOL 1)
(def-mixer-cmd PAN 2)
(def-mixer-cmd REVERB 3)
(def-mixer-cmd FLT 4)

;; --- AUTOMATION ---
(def-bogu-cmd SWEEP (args)
  "Generates pure dynamic control data (automations). Can optionally wrap a child block."
  (let* ((expanded (expand-vars args))
         (param-input (car expanded))
         (start (/ (float (cadr expanded)) 100.0))
         (end (/ (float (caddr expanded)) 100.0))
         (target (nth 3 expanded)) ; Can be a duration value OR an AST block (list)
         (param-str (string-upcase (string param-input)))
         (param-id (cond ((member param-str '("VOL" "V") :test #'string=) 1)
                         ((member param-str '("PAN" "P") :test #'string=) 2)
                         ((member param-str '("REVERB" "RVB") :test #'string=) 3)
                         ((member param-str '("FLT" "F") :test #'string=) 4)
                         (t nil))))
    (if (null param-id)
        (progn (format t "~%[SWEEP ERROR] Unknown parameter '~A'. Use VOL, PAN, RVB, or FLT.~%" param-input) nil)
        (if (listp target)
            ;; 1. Higher-Order Mode: Target is a wrapped AST block (e.g., [ fluid 4 11 sub-pool ])
            (let* ((child-events (execute-ast target))
                   (total-dur 0.0))
              ;; Measure the exact duration of the compiled child block
              (dolist (e child-events)
                (setf total-dur (max total-dur (+ (getf e :time) (getf e :written-dur)))))
              ;; Return the sweep + child events. 
              ;; CRITICAL: We set the sweep's :written-dur to total-dur so the stitcher 
              ;; advances the local timeline cursor perfectly!
              (cons (list :type :control :time 0.0 :written-dur total-dur :dur total-dur 
                          :param param-id :start start :end end)
                    child-events))
            
            ;; 2. Standard Mode: Target is just a duration
            (let ((dur (if target (rtm target) 4.0)))
               ;; :written-dur is 0.0 so it doesn't consume time on the timeline
               (list (list :type :control :time 0.0 :written-dur 0.0 :dur dur 
                           :param param-id :start start :end end)))))))

;; --- PURE TREE TRANSFORMERS ---

(def-bogu-cmd TRANSPOSE (args)
  "Maps over an evaluated block and shifts the pitch symbols purely."
  (let* ((expanded (expand-vars args))
         (offset (if (numberp (car expanded)) (car expanded) (parse-integer (string (car expanded)))))
         (body (cdr expanded))
         (raw-events (execute-ast (if (and (listp body) (listp (car body))) body (list body)))))
    
    ;; Use mapcar to return a fresh, perfectly transformed list of data
    (mapcar (lambda (event)
              (if (eq (getf event :type) :note)
                  ;; Create a fresh copy of the event so we don't accidentally mutate shared memory!
                  (let* ((new-event (copy-list event))
                         (current-offset (or (getf new-event :transpose) 0)))
                    (setf (getf new-event :transpose) (+ current-offset offset))
                    new-event)
                  ;; If it's not a note, just pass the copy along
                  (copy-list event)))
            raw-events)))

(def-bogu-cmd LOOP (args)
  "A Generative Loop that returns a massive list of un-shifted music data."
  (let* ((expanded (expand-vars args))
         (iterations (car expanded))
         (body (cadr expanded))
         (combined-result nil))
    (dotimes (i iterations)
      ;; We append the result of each iteration into one giant 'virtual' block
      (setf combined-result (append combined-result (execute-ast body))))
    combined-result))

;; --- SYSTEM & LIVE LOOPS ---
(def-bogu-cmd REBOOT (args)
  (reboot-audio-server))

(def-bogu-cmd DELAY (args)
  "Pauses the Lisp thread. Useful for letting hardware boot up."
  (sleep (if args (car (expand-vars args)) 1.0))
  nil)

(def-bogu-cmd LIVE-LOOP (args)
  "Auto-sizing live-loop. Syntax: (LIVE-LOOP name [block]) OR (LIVE-LOOP name padding [block])"
  (let* ((expanded (expand-vars args))
         (name (car expanded))
         ;; Safely check if the second argument is a raw atom/rhythm (e.g. 4, Q, E) to act as padding
         (has-padding (and (cadr expanded) (atom (cadr expanded)) (numberp (rtm (cadr expanded)))))
         (padding-beats (if has-padding (rtm (cadr expanded)) 0.0))
         (block (if has-padding (caddr expanded) (cadr expanded)))
         (old-thread (gethash name *loop-threads*)))
    
    (when (and old-thread (sb-thread:thread-alive-p old-thread))
      (sb-thread:terminate-thread old-thread))
    
    (setf (gethash name *live-loops*) block)
    (format t "~%[LOOP] Armed live-loop '~A' (Auto-size + ~A beats padding).~%" name padding-beats)
    
    (setf (gethash name *loop-threads*)
          (sb-thread:make-thread
           (lambda ()
             (handler-case 
                 ;; Start the loop's absolute timeline EXACTLY right now
                 (let ((next-loop-start-time (get-internal-real-time)))
                   (loop
                     (let ((current-block (gethash name *live-loops*)))
                       (unless current-block (return))
                       
                       (let ((*score* '())
                             (*tracks* (clone-tracks-for-sandbox))
                             (*current-instrument* 1))
                         
                         ;; 1. Evaluate the AST and generate the timeline
                         (commit current-block)
                         (setf *score* (sort *score* #'< :key (lambda (x) (getf x :time))))
                         
                         ;; 2. Natively calculate the exact musical footprint of the AST
                         (let ((measured-beats (track-playhead (get-current-track))))
                           
                           ;; 3. Execute the timeline exactly on time
                           (let* ((current-bpm (if *bpm* (car *bpm*) 60.0))
                                  (sec-per-beat (float (/ 60.0 current-bpm))))
                             
                             (dolist (event *score*)
                               (let* ((offset-ms (round (* (* (getf event :time) sec-per-beat) internal-time-units-per-second)))
                                      (target-ms (+ next-loop-start-time offset-ms)))
                                 
                                 ;; Sleep until the exact microsecond this specific event should fire
                                 (loop while (< (get-internal-real-time) target-ms) do (sleep 0.001))
                                 
                                 (if (eq (getf event :type) :note)
                                     (osc-play (getf event :instr) (* (getf event :dur) sec-per-beat) (getf event :pch) (getf event :vel))
                                     (osc-control (getf event :instr) (getf event :param) (* (getf event :dur) sec-per-beat) (getf event :start) (getf event :end)))))
                             
                             ;; 4. Wait for the padding AND the final tail of the music!
                             (let* ((total-loop-beats (+ measured-beats padding-beats))
                                    (loop-dur-ms (round (* (* total-loop-beats sec-per-beat) internal-time-units-per-second))))
                               
                               (incf next-loop-start-time loop-dur-ms)
                               (loop while (< (get-internal-real-time) next-loop-start-time) do (sleep 0.001)))))))))
               (error (e) (format t "~%[LOOP ERROR] ~A~%" e))))
           :name (format nil "bogu-loop-~A" name)))
    nil))

(def-bogu-cmd STOP-LOOP (args)
  (let ((name (car (expand-vars args))))
    (if (eq name 'ALL)
        (progn (maphash (lambda (k th) (when (and th (sb-thread:thread-alive-p th)) (sb-thread:terminate-thread th))) *loop-threads*)
               (clrhash *loop-threads*) (clrhash *live-loops*) (format t "~%[LOOP] All terminated.~%"))
        (let ((thread (gethash name *loop-threads*)))
          (if thread
              (progn (when (sb-thread:thread-alive-p thread) (sb-thread:terminate-thread thread))
                     (remhash name *loop-threads*) (remhash name *live-loops*)
                     (format t "~%[LOOP] Terminated '~A'.~%" name))
              (format t "~%[LOOP Error] Not running.~%"))))
    nil))

;; --- NOTATION ---

(def-bogu-cmd ENGRAVE (args)
  (let* ((expanded-args (expand-vars args))
         (filename (car expanded-args))
         (instr (cadr expanded-args)))
    (if (and filename instr)
        (bogu->ly (string-downcase (string filename)) instr)
        (format t "~%[Syntax Error] engrave requires a filename and a track number.~%"))))

;; --- TRACK STATE ENCAPSULATION ---

(defstruct track
  (id 1 :type integer)
  (playhead 0.0 :type float)
  (transpose-offset 0 :type integer)
  (velocity 0.8 :type float)
  (articulation :legato :type keyword)
  (key nil :type list))

(defparameter *tracks* (make-hash-table))

(defun get-current-track ()
  "Retrieves the active track, initializing it if it doesn't exist yet."
  (or (gethash *current-instrument* *tracks*)
      (setf (gethash *current-instrument* *tracks*) 
            (make-track :id *current-instrument*))))

(defun clone-tracks-for-sandbox ()
  "Creates a perfect copy of all track states but resets playheads and pending automations."
  (let ((new-ht (make-hash-table)))
    (maphash (lambda (k v)
               (setf (gethash k new-ht)
                     (make-track :id (track-id v)
                                 :playhead 0.0  
                                 :transpose-offset (track-transpose-offset v)
                                 :velocity (track-velocity v)
                                 :articulation (track-articulation v)
                                 :key (track-key v)))) 
             *tracks*)
    new-ht))

(defun current-time ()
  "Returns the current playhead position for the active instrument."
  (track-playhead (get-current-track)))

(defun set-current-time (time)
  "Explicitly sets the playhead for the active instrument."
  (setf (track-playhead (get-current-track)) (float time)))

(defun advance-time (amount)
  "Moves the playhead forward ONLY for the active instrument."
  (incf (track-playhead (get-current-track)) (float amount)))

(defun seek (beat)
  "Teleports the playhead of the active instrument to an absolute beat."
  (let ((target-time (if (numberp beat) (float beat) (rtm beat))))
    (set-current-time target-time)
    (format t "~%[TIMELINE] Track ~A teleported to ~,3fs~%" *current-instrument* target-time)))

(defun sync ()
  "Finds the furthest playhead in the matrix and fast-forwards all tracks to catch up."
  (let ((max-time 0.0))
    (maphash (lambda (k trk) (setf max-time (max max-time (track-playhead trk)))) *tracks*)
    (maphash (lambda (k trk) (setf (track-playhead trk) max-time)) *tracks*)
    (format t "~%[TIMELINE] All tracks synchronized to ~,3fs~%" max-time)))

(defun where ()
  "Reports the current chronological position of all active track playheads."
  (format t "~%--- [TIMELINE STATUS] ---~%")
  (if (= (hash-table-count *tracks*) 0)
      (format t " All tracks at: 0.0s (Master Start)~%")
      (maphash (lambda (instr-id trk)
                 (format t " Instrument ~2a : ~,3fs ~a~%" 
                         instr-id 
                         (track-playhead trk)
                         (if (= instr-id *current-instrument*) "<-- (ACTIVE)" "")))
               *tracks*))
  (format t "-------------------------~%")
  t)

(defun del (n)
  "Deletes the chronologically latest N events from the active track and rewinds the playhead."
  (let ((track-events nil)
        (other-events nil))
    
    ;; 1. Isolate the active instrument's events from the rest of the score
    (dolist (event *score*)
      (if (= (getf event :instr) *current-instrument*)
          (push event track-events)
          (push event other-events)))
          
    ;; 2. Force sort descending by time so the newest events are always at the front of the list
    (setf track-events (sort track-events #'> :key (lambda (x) (getf x :time))))
    
    ;; 3. Safely drop the latest N events
    (setf track-events (nthcdr n track-events))
    
    ;; 4. Recombine the score
    (setf *score* (append track-events other-events))
    
    ;; 5. Find the true edge of the timeline for the remaining track events
    (let ((new-time 0.0))
      (dolist (event track-events)
        (setf new-time (max new-time (+ (getf event :time) (getf event :dur)))))
        
      ;; 6. Directly update the track state. 
      ;; We bypass 'set-current-time' here so we don't accidentally flush pending automations!
      (setf (track-playhead (get-current-track)) (float new-time))
      (format t "~%[TIMELINE] Rewound playhead. Deleted ~a events from Track ~a.~%" n *current-instrument*))))

(defun vars (&optional show-all)
  "Displays the current ledger of user-defined variables."
  (format t "~%--- BOGU VARIABLES ---~%")
  (if (= (hash-table-count *vars*) 0)
      (format t " (No custom variables defined yet)~%")
      (maphash (lambda (k v) (format t " ~a: ~a~%" k v)) *vars*))
      
  (when (eq show-all 'all)
    (format t "~%--- STANDARD LIBRARY ---~%")
    (maphash (lambda (k v) (format t " ~a: ~a~%" k v)) *stdlib-vars*))
  (format t "----------------------~%~%")
  t)

(defun synth (slot-id template-name)
  "Loads a synth template into a specific hardware rack slot."
  (let ((template (gethash template-name *synth-templates*)))
    (if template
        (progn
          (setf (gethash slot-id *synth-rack*) template)
          (format t "~%[RACK] Loaded ~A into Slot ~A~%" template-name slot-id))
        (format t "~%[RACK ERROR] No synth template named ~A found in memory.~%" template-name))))

(defun bpm (n); add optional nth bpms
  "Sets beats per minute."
  (setf *bpm* '())
  (push "t" *bpm*)
  (push 0 *bpm*)
  (push n *bpm*))
