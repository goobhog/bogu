;;stdlib/commands.lisp

(in-package :bogu)

;; =============================================================================
;; 1. SEQUENCING & COMBINATORICS
;; =============================================================================

(def-bogu-cmd SEQ (:rhythm-optional &rest :any) (args)
  "Generates a sequential list of events. If rhythm is provided, it dictates the step size."
  (let* ((expanded (expand-vars args))
         (rhythm (if (and (atom (car expanded)) (numberp (rtm (car expanded)))) (rtm (car expanded)) nil))
         (nodes (smart-unwrap (if rhythm (cdr expanded) expanded)))
         (master-events nil)
         (local-cursor 0.0))
    (dolist (node nodes)
      (let* ((evaluated-block (execute-ast (list node)))
             (block-len 0.0)
             ;; Check if this step is just a single note, or a complex generated block
             (is-single (<= (length (remove-if (lambda (x) (not (eq (getf x :type) :note))) evaluated-block)) 1)))
        
        (dolist (e evaluated-block)
          (let ((new-e (copy-list e)))
            ;; ONLY overwrite the duration if it's a simple single note!
            ;; Otherwise, let the complex block keep its internal rhythm.
            (when (and rhythm is-single)
              (setf (getf new-e :written-dur) rhythm)
              (when (getf new-e :dur) (setf (getf new-e :dur) rhythm)))
            
            ;; Place the event on the timeline relative to the cursor
            (setf (getf new-e :time) (+ local-cursor (getf e :time)))
            (push new-e master-events)
            
            ;; Measure the true footprint of the notes (The Critical Fix Area)
            (setf block-len (max block-len (+ (getf e :time) 
                                              (or (getf new-e :written-dur) 
                                                  (getf new-e :dur) 
                                                  0.0))))))
            
        ;; Advance the cursor! 
        (incf local-cursor (if rhythm rhythm block-len))))
    (reverse master-events)))

(def-bogu-cmd POLY (:rhythm-optional &rest :any) (args)
  "Simultaneous evaluation. Geometrically collapses all elements vertically to local time 0.0."
  (let* ((expanded (expand-vars args))
         (rhythm (if (and (atom (car expanded)) (numberp (rtm (car expanded)))) (rtm (car expanded)) nil))
         (nodes (smart-unwrap (if rhythm (cdr expanded) expanded)))
         (nodes-list (if (listp nodes) nodes (list nodes)))
         (master-events nil))
    (dolist (node nodes-list)
      (let ((evaluated-block (execute-ast (list node))))
        (dolist (e evaluated-block)
          (let ((new-e (copy-list e)))
            (when rhythm 
              ;; 1. Stretch the sequencer timeline footprint
              (setf (getf new-e :written-dur) rhythm)
              ;; 2. Stretch the acoustic synth gate physics!
              (when (getf new-e :dur) (setf (getf new-e :dur) rhythm)))
            
            ;; THE PHILOSOPHICAL FIX:
            ;; Project everything onto the Y-axis. Destroy the X-axis (time).
            (setf (getf new-e :time) 0.0) 
            
            (push new-e master-events)))))
    (reverse master-events)))

(def-bogu-cmd SIM (&rest :any) (args)
  "Parallel execution. Evaluates multiple blocks simultaneously at time 0.0."
  (let* ((expanded (expand-vars args))
         (master-events nil))
    (dolist (block expanded)
      ;; Safely evaluate each block in absolute isolation
      (let ((evaluated-block (execute-ast (list block))))
        (setf master-events (append master-events (mapcar #'copy-list evaluated-block)))))
    ;; Mathematically sort the combined streams chronologically before returning!
    (stable-sort master-events #'< :key (lambda (x) (getf x :time)))))

(def-bogu-cmd CELL (:rhythm :ast) (args)
  "A strict time-window. Evaluates a block, truncates anything that bleeds over both musically and acoustically."
  (let* ((expanded (expand-vars args))
         (cell-duration (rtm (car expanded)))
         (block (cadr expanded))
         (evaluated-block (execute-ast (list block)))
         (master-events nil))
    (dolist (e evaluated-block)
      ;; Only keep events that start BEFORE the cell dies
      (when (< (getf e :time) cell-duration)
        (let ((new-e (copy-list e)))
          
          ;; Ensure the note has an explicit acoustic duration to manipulate
          (unless (getf new-e :dur) (setf (getf new-e :dur) (or (getf new-e :written-dur) 0.1)))

          ;; Mathematically chop the Grid Footprint
          (when (> (+ (getf new-e :time) (getf new-e :written-dur)) cell-duration)
            (setf (getf new-e :written-dur) (max 0.0 (- cell-duration (getf new-e :time)))))
            
          ;; Mathematically chop the Acoustic Physics so the synth cuts off
          (when (> (+ (getf new-e :time) (getf new-e :dur)) cell-duration)
            (setf (getf new-e :dur) (max 0.0 (- cell-duration (getf new-e :time)))))

          (push new-e master-events))))
          
    (push (list :type :note :pitch-symbol 'RST :time cell-duration :written-dur 0.0) master-events)
    (reverse master-events)))

(def-bogu-cmd WAIT (:rhythm-optional) (args)
  "Generates a pure rest of the specified duration. Defaults to 1.0 (Q)."
  (let* ((expanded (expand-vars args))
         (dur (if expanded (rtm (car expanded)) 1.0)))
    (list (make-rest-event 0.0 dur))))

(def-bogu-cmd MAP-CONTROL (:symbol :number :ast) (args)
  "Converts a musical phrase into a control envelope."
  (let* ((param-id (resolve-param-id (car args)))
         (duration (rtm (cadr args)))
         (phrase (execute-ast (list (caddr args))))
         (master-events nil)
         (num-notes (length phrase)))
    (if (= num-notes 0)
        nil
        (let ((step-size (/ duration num-notes)))
          (dotimes (i num-notes)
            (let* ((event (nth i phrase))
                   (pch (getf event :pch))
                   (normalized-val (if pch (clamp (/ (- pch 7.0) 3.0) 0.0 1.0) 0.5)))
              (push (make-control-event param-id normalized-val normalized-val (* i step-size) step-size)
                    master-events)))
          (reverse master-events)))))

(defun clamp (val min max)
  (max min (min max val)))

;; =============================================================================
;; 2. GENERATIVE & ALEATORIC ENGINES
;; =============================================================================

(def-bogu-cmd SARP (:rhythm :rhythm &rest :any) (args)
  "Sustained arpeggio. Plays through the provided pool EXACTLY ONCE at step r."
  (let* ((expanded (expand-vars args))
         (r (rtm (car expanded)))   
         (s (rtm (cadr expanded)))  
         (nodes (smart-unwrap (cddr expanded)))
         (len (length nodes))
         (local-cursor 0.0)
         (master-events nil))         
    (dotimes (i len)
      (let ((node (nth i nodes)))        
        (let ((evaluated-block (execute-ast (list node)))
              (physics-dur (max 0.1 (- s (* r i)))))            
          (dolist (e evaluated-block)
            (let ((new-e (copy-list e)))
              (setf (getf new-e :time) (+ local-cursor (getf e :time)))
              (setf (getf new-e :written-dur) r)
              (setf (getf new-e :dur) physics-dur) 
              (push new-e master-events)))))
      (incf local-cursor r))
    (push (list :type :note :pitch-symbol 'RST :time (max s local-cursor) :written-dur 0.0) master-events)
    (reverse master-events)))

(def-bogu-cmd FLUID (:number :rhythm &rest :any) (args)
  "Generates an aleatoric cloud of notes over a specific duration (r)."
  (let* ((expanded (expand-vars args))
         (density (if (numberp (car expanded)) (car expanded) (parse-integer (string (car expanded)))))
         (r (rtm (cadr expanded)))
         (nodes (smart-unwrap (cddr expanded)))
         (len (length nodes))
         (master-events nil))
    (dotimes (i density)
      (let* ((random-node (nth (random len) nodes))
             (random-offset (* r (/ (random 1000) 1000.0))))
        (let ((evaluated-node (execute-ast (list random-node))))
          (dolist (e evaluated-node)
            (let ((new-e (copy-list e)))
              (setf (getf new-e :time) (+ random-offset (getf e :time)))
              (when (> (+ (getf new-e :time) (getf new-e :written-dur)) r)
                (setf (getf new-e :written-dur) (max 0.0 (- r (getf new-e :time)))))
              (when (getf new-e :dur)
                (when (> (+ (getf new-e :time) (getf new-e :dur)) r)
                  (setf (getf new-e :dur) (max 0.0 (- r (getf new-e :time))))))
              (when (> (getf new-e :written-dur) 0)
                (push new-e master-events)))))))
    (push (list :type :note :pitch-symbol 'RST :time r :written-dur 0.0) master-events)
    (reverse master-events)))

(def-bogu-cmd WALK (:number :rhythm &rest :any) (args)
  "Generates a random walk through a sequence of notes."
  (let* ((expanded (expand-vars args))
         (steps (if (numberp (car expanded)) (car expanded) (parse-integer (string (car expanded)))))
         (r (rtm (cadr expanded)))
         (nodes (smart-unwrap (cddr expanded)))
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

(def-bogu-cmd CHANCE (:number :ast) (args)
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

(def-bogu-cmd CHOOSE (:number :ast :ast-optional) (args)
  "Usage: (CHOOSE 0.6 BlockA BlockB). Picks BlockA 60% of the time, else BlockB."
  (let* ((expanded (expand-vars args))
         (probability (car expanded))
         (block-a (cadr expanded))
         (block-b (caddr expanded)))
    (if (<= (/ (random 100) 100.0) probability)
        (execute-ast (list block-a))
        (if block-b (execute-ast (list block-b)) nil))))

(def-bogu-cmd BEATS (&rest :any) (args)
  "Generates a stream of pure timing data from raw rhythm symbols."
  (let* ((expanded (expand-vars args))
         (rhythms (smart-unwrap expanded))
         (r-list (if (listp rhythms) rhythms (list rhythms)))
         (master-events nil)
         (local-cursor 0.0))
    (dolist (r r-list)
      (let ((dur (rtm r)))
        (push (list :type :note :pitch-symbol 'C :octave 4 :time local-cursor :written-dur dur :dur dur :instr *current-instrument*) master-events)
        (incf local-cursor dur)))
    (reverse master-events)))

(def-bogu-cmd TREAD (:number :ast :ast) (args)
  "Deterministic walker. Syntax: (TREAD start-index [intervals] [pool])"
  (let* ((expanded (expand-vars args))
         (start-idx (car expanded))
         ;; THE FIX: Flatten the parsed array so ((1) (2) (-1)) safely becomes (1 2 -1)
         (intervals (flatten (cadr expanded))) 
         (int-list (if (listp intervals) intervals (list intervals)))
         (pool (smart-unwrap (caddr expanded)))
         (pool-len (length pool))
         (master-events nil)
         (current-idx start-idx)
         (local-cursor 0.0))
         
    ;; 1. Push the starting note first
    (let* ((first-sym (nth (mod current-idx pool-len) pool))
           (first-node (execute-ast (list first-sym))))
      (dolist (e first-node)
        (let ((new-e (copy-list e)))
          (setf (getf new-e :time) local-cursor)
          (setf (getf new-e :written-dur) 1.0)
          (setf (getf new-e :dur) 1.0)
          (push new-e master-events)))
      (incf local-cursor 1.0))
      
    ;; 2. Now walk the intervals
    (dolist (step int-list)
      (incf current-idx (if (numberp step) step (parse-integer (string step))))
      (let* ((pitch-sym (nth (mod current-idx pool-len) pool))
             (evaluated-node (execute-ast (list pitch-sym))))
        (dolist (e evaluated-node)
          (let ((new-e (copy-list e)))
            (setf (getf new-e :time) local-cursor)
            (setf (getf new-e :written-dur) 1.0)
            (setf (getf new-e :dur) 1.0)
            (push new-e master-events)))
        (incf local-cursor 1.0)))
    (reverse master-events)))

;; =============================================================================
;; 3. TREE TRANSFORMERS & MATH
;; =============================================================================

(def-bogu-cmd AUGMENT (:number :ast) (args)
  "Tree transformer: Multiplies the time and duration of an entire block by a factor."
  (let* ((expanded (expand-vars args))
         (factor (float (car expanded)))
         (child-block (execute-ast (cdr expanded)))
         (master-events nil))
    (dolist (e child-block)
      (let ((new-e (copy-list e)))
        (setf (getf new-e :time) (* (getf new-e :time) factor))
        (when (getf new-e :written-dur)
          (setf (getf new-e :written-dur) (* (getf new-e :written-dur) factor)))
        (when (getf new-e :dur)
          (setf (getf new-e :dur) (* (getf new-e :dur) factor)))
        (push new-e master-events)))
    (reverse master-events)))

(def-bogu-cmd TRANSPOSE (:number :ast) (args)
  "Maps over an evaluated block and shifts the pitch symbols purely."
  (let* ((expanded (expand-vars args))
         (offset (if (numberp (car expanded)) (car expanded) (parse-integer (string (car expanded)))))
         (body (cdr expanded))
         ;; THE FIX: Safe wrapper logic so it doesn't shatter when passed an un-wrapped command node
         (safe-body (if (and (listp body) (listp (car body))) body (list body)))
         (raw-events (execute-ast safe-body)))
    
    (mapcar (lambda (event)
              (if (eq (getf event :type) :note)
                  (let* ((new-event (copy-list event))
                         (current-offset (or (getf new-event :transpose) 0)))
                    (setf (getf new-event :transpose) (+ current-offset offset))
                    new-event)
                  (copy-list event)))
            raw-events)))

(defparameter *pc-to-note* #(C DB D EB E F GB G AB A BB B))

(def-bogu-cmd INVERT (:ast) (args)
  "Purely inverts the raw pitch symbols of a block around its first note, before diatonic math is applied."
  (let* ((expanded (expand-vars args))
         (block (car expanded))
         (evaluated-stream (execute-ast (list block)))
         (first-note (find-if (lambda (x) (eq (getf x :type) :note)) evaluated-stream)))
    
    (if (null first-note)
        evaluated-stream 
        
        (let* ((axis-sym (getf first-note :pitch-symbol))
               (axis-oct (getf first-note :octave))
               (axis-semitone (+ (* axis-oct 12) (cdr (assoc axis-sym *notes*)))))
               
          (mapcar (lambda (event)
                    (if (eq (getf event :type) :note)
                        (let* ((new-event (copy-list event))
                               (sym (getf new-event :pitch-symbol))
                               (oct (getf new-event :octave)))
                          
                          (unless (or (eq sym 'R) (eq sym 'RST))
                            (let* ((current-semitone (+ (* oct 12) (cdr (assoc sym *notes*))))
                                   (diff (- current-semitone axis-semitone))
                                   (new-semitone (- axis-semitone diff))
                                   (new-oct (floor new-semitone 12))
                                   (new-pc (mod new-semitone 12))
                                   ;; THE FIX: Canonical lookup guarantees C instead of B#
                                   (new-sym (aref *pc-to-note* new-pc)))
                              
                              (setf (getf new-event :pitch-symbol) new-sym)
                              (setf (getf new-event :octave) new-oct)))
                          new-event)
                        (copy-list event)))
                  evaluated-stream)))))

(def-bogu-cmd RETRO (:ast) (args)
  "Reverses the timeline of a block."
  (let* ((expanded (expand-vars args))
         (block (car expanded))
         (evaluated-stream (execute-ast (list block)))
         (max-time 0.0))
         
    (dolist (e evaluated-stream)
       (setf max-time (max max-time (+ (getf e :time) (getf e :written-dur)))))
       
    (stable-sort (mapcar (lambda (e)
                           (let ((new-e (copy-list e)))
                             (setf (getf new-e :time) (- max-time (+ (getf e :time) (getf e :written-dur))))
                             new-e))
                         evaluated-stream)
                 #'< :key (lambda (x) (getf x :time)))))

(def-bogu-cmd RPT (:number :ast) (args)
  "Data Loop. Evaluates the AST once, then pastes exact time-shifted copies."
  (let* ((expanded (expand-vars args))
         (iterations (car expanded))
         (body (cadr expanded))
         (blueprint-events (execute-ast (list body)))
         (master-events nil)
         (local-cursor 0.0)
         (blueprint-len 0.0))
         
    (dolist (e blueprint-events)
      (setf blueprint-len (max blueprint-len (+ (getf e :time) (getf e :written-dur)))))
        
    (dotimes (i iterations)
      (dolist (e blueprint-events)
        (let ((new-event (copy-list e)))
          (setf (getf new-event :time) (+ local-cursor (getf e :time)))
          (push new-event master-events)))
      (incf local-cursor blueprint-len))
    (reverse master-events)))

(def-bogu-cmd STACCATO (:number :ast) (args)
  "Tree transformer: Shortens the absolute duration (:dur) of all child notes by a percentage, without altering their written rhythm."
  (let* ((expanded (expand-vars args))
         (percent (/ (float (car expanded)) 100.0))
         (child-block (execute-ast (cdr expanded)))
         (master-events nil))
    (dolist (e child-block)
      (let ((new-e (copy-list e)))
        (when (eq (getf new-e :type) :note)
          (let ((current-dur (or (getf new-e :dur) (getf new-e :written-dur))))
            (setf (getf new-e :dur) (* current-dur percent))))
        (push new-e master-events)))
    (reverse master-events)))

(def-bogu-cmd LOOP (:number :ast) (args)
  "A Generative Loop that returns a massive list of un-shifted music data."
  (let* ((expanded (expand-vars args))
         (iterations (car expanded))
         (body (cadr expanded))
         (combined-result nil))
    (dotimes (i iterations)
      (setf combined-result (append combined-result (execute-ast body))))
    combined-result))

(def-bogu-cmd ZIP (:ast :ast) (args)
  "Combinatoric zipper. Fuses Time from Block A with Pitch from Block B. Preserves Polyphonic Chords!"
  (let* ((expanded (expand-vars args))
         (block-a (execute-ast (list (car expanded))))
         (block-b (execute-ast (list (cadr expanded)))))

    (let* ((groups-a (group-events-by-time block-a))
           (groups-b (group-events-by-time block-b))
           (len-a (length groups-a))
           (len-b (length groups-b))
           (master-events nil))
      
      (when (and (> len-a 0) (> len-b 0))
        (dotimes (i len-a)
          (let* ((step-a (nth i groups-a))
                 (rhythm-source (car step-a)) 
                 (step-b (nth (mod i len-b) groups-b)))
            
            (dolist (event-b step-b)
              (let ((new-event (copy-list event-b)))
                (setf (getf new-event :time) (getf rhythm-source :time))
                (when (and (eq (getf rhythm-source :type) :note) (eq (getf event-b :type) :note))
                  (setf (getf new-event :dur) (getf rhythm-source :dur))
                  (setf (getf new-event :written-dur) (getf rhythm-source :written-dur)))
                (push new-event master-events))))))
      (reverse master-events))))

;; =============================================================================
;; 4. LOGIC, STATE & VARIABLES
;; =============================================================================

(def-bogu-cmd DEF (:symbol &rest :any) (args)
  (let* ((var-name (car args))
         (stored-ast (smart-unwrap (cdr args))))
    (setf (gethash var-name *vars*) stored-ast)
    nil))

(def-bogu-cmd I (:number) (args)
  (setf *current-instrument* (car (expand-vars args)))
  nil)

(def-bogu-cmd KEY (:symbol :symbol-optional) (args)
  (let* ((expanded (expand-vars args))
         (trk (get-current-track)))
    (if (or (null expanded) (eq (car expanded) 'OFF) (eq (car expanded) 'NIL))
        (setf (track-key trk) nil) 
        (setf (track-key trk) (list (car expanded) (cadr expanded))))
    nil))

(def-bogu-cmd IF (:any :symbol :any :ast :ast-optional) (args)
  "A lazy-evaluating conditional guard that prevents infinite recursion loops."
  (let* ((cond-args (subseq args 0 3))
         (expanded-cond (expand-vars cond-args))
         (val1 (car expanded-cond))
         (op-sym (cadr expanded-cond))
         (val2 (caddr expanded-cond))
         (op-fn (cond ((eq op-sym '=) #'=) ((eq op-sym '>) #'>) ((eq op-sym '<) #'<)
                      ((eq op-sym '>=) #'>=) ((eq op-sym '<=) #'<=) ((eq op-sym '!=) #'/=) (t nil)))
         (then-branch (nth 3 args))
         (else-branch (nth 4 args)))
    (if (and op-fn (numberp val1) (numberp val2))
        (if (funcall op-fn val1 val2)
            (execute-ast (list then-branch))
            (when else-branch (execute-ast (list else-branch))))
        (format t "~%[Logic Error] Invalid IF syntax.~%"))))

(def-bogu-cmd CLEF (:symbol) (args)
  "Sets the default starting clef for the active track (e.g., bass or treble)."
  (let* ((expanded (expand-vars args))
         (c (string-downcase (string (car expanded))))
         (trk (get-current-track)))
    (setf (track-clef trk) c))
  nil)

(def-bogu-cmd BREAK () (args)
  "Forces a system break in the engraved LilyPond sheet music."
  (list (list :type :meta :subtype :line-break :time 0.0 :written-dur 0.0 :dur 0.0)))

;; =============================================================================
;; 5. MIXER & AUTOMATION
;; =============================================================================

(defmacro def-mixer-cmd (name param-id)
  "Safely generates pure static control data AND sends instant top-level initialization."
  `(def-bogu-cmd ,name (:number) (args)
     (let* ((expanded (expand-vars args))
            (val (/ (float (car expanded)) 100.0)))
       (osc-control *current-instrument* ,param-id 0.01 val val)
       (list (list :type :control :time 0.0 :written-dur 0.0 :dur 0.01 :param ,param-id :start val :end val :instr *current-instrument*)))))

(def-mixer-cmd VOL 1)
(def-mixer-cmd PAN 2)
(def-mixer-cmd REVERB 3)
(def-mixer-cmd FLT 4)

(def-bogu-cmd SWEEP (:symbol :number :number :any-optional) (args)
  "Generates pure dynamic control data strictly bound to the sequencer grid."
  (let* ((expanded (expand-vars args))
         (param-input (car expanded))
         (start (/ (float (cadr expanded)) 100.0))
         (end (/ (float (caddr expanded)) 100.0))
         (target (nth 3 expanded))
         (param-str (string-upcase (string param-input)))
         (param-id (cond ((member param-str '("VOL" "V") :test #'string=) 1)
                         ((member param-str '("PAN" "P") :test #'string=) 2)
                         ((member param-str '("REVERB" "RVB") :test #'string=) 3)
                         ((member param-str '("FLT" "F") :test #'string=) 4)
                         (t nil))))
    (if (null param-id)
        (progn (format t "~%[SWEEP ERROR] Unknown parameter '~A'. Use VOL, PAN, RVB, or FLT.~%" param-input) nil)
        (if (and target (not (numberp target)))
            ;; TARGET IS AN AST BLOCK: Intelligently wrap and measure it
            (let* ((safe-target (if (and (listp target) (symbolp (car target))) (list target) target))
                   (child-events (execute-ast safe-target))
                   (grid-len 0.0))
              (dolist (e child-events)
                (setf grid-len (max grid-len (+ (getf e :time) (or (getf e :written-dur) 0.0)))))
              (cons (list :type :control :time 0.0 :written-dur grid-len :dur grid-len 
                          :param param-id :start start :end end :instr *current-instrument*)
                    child-events))
            ;; TARGET IS A NUMBER OR NIL: Sweep "in place" for standard wait blocks
            (let ((dur (if (numberp target) (float target) 4.0))) 
               (list (list :type :control :time 0.0 :written-dur 0.0 :dur dur 
                           :param param-id :start start :end end :instr *current-instrument*)))))))

;; =============================================================================
;; 6. LIVE-LOOPING & EXECUTION THREADS
;; =============================================================================

(def-bogu-cmd REBOOT () (args)
  (reboot-audio-server))

(def-bogu-cmd DELAY (:number-optional) (args)
  "Pauses the Lisp thread. Useful for letting hardware boot up."
  (sleep (if args (car (expand-vars args)) 1.0))
  nil)

(def-bogu-cmd LIVE-LOOP (:symbol :rhythm-optional :ast) (args)
  "Auto-sizing live-loop. Safely monitors a state flag to avoid mutex deadlocks."
  (let* ((expanded (expand-vars args))
         (name (car expanded))
         (instr-id *current-instrument*)
         (has-padding (and (cadr expanded) (atom (cadr expanded)) (numberp (rtm (cadr expanded)))))
         (padding-beats (if has-padding (rtm (cadr expanded)) 0.0))
         (block (if has-padding (caddr expanded) (cadr expanded))))
    
    (setf (gethash name *live-loops*) block)
    (format t "~%[LOOP] Armed live-loop '~A' (Auto-size + ~A beats padding).~%" name padding-beats)
    
    (let ((existing-thread (gethash name *loop-threads*)))
      (unless (and existing-thread (sb-thread:thread-alive-p existing-thread))
        (setf (gethash name *loop-threads*)
              (sb-thread:make-thread
               (lambda ()
                 (block thread-execution
                   (handler-case 
                       (let ((next-loop-start-time (get-internal-real-time)))
                         (loop
                           (let ((current-block (gethash name *live-loops*)))
                             (unless current-block 
                               (return-from thread-execution))
                             
                             (let ((*score* '())
                                   (*tracks* (clone-tracks-for-sandbox))
                                   (*current-instrument* instr-id))
                               
                               (commit current-block)
                               (setf *score* (sort *score* #'< :key (lambda (x) (getf x :time))))
                               
                               (let* ((measured-beats (track-playhead (get-current-track)))
                                      (current-bpm (if *bpm* (car *bpm*) 60.0))
                                      (sec-per-beat (float (/ 60.0 current-bpm))))
                                 
                                 (dolist (event *score*)
                                   (let* ((offset-ms (round (* (* (getf event :time) sec-per-beat) internal-time-units-per-second)))
                                          (target-ms (+ next-loop-start-time offset-ms)))
                                     
                                     (loop while (< (get-internal-real-time) target-ms) do
                                       (unless (gethash name *live-loops*)
                                         (return-from thread-execution))
                                       (sleep 0.001))
                                     
                                     (if (eq (getf event :type) :note)
                                         (osc-play (getf event :instr) (* (getf event :dur) sec-per-beat) (getf event :pch) (getf event :vel))
                                         (osc-control (getf event :instr) (getf event :param) (* (getf event :dur) sec-per-beat) (getf event :start) (getf event :end)))))
                                 
                                 (let* ((total-loop-beats (+ measured-beats padding-beats))
                                        (loop-dur-ms (round (* (* total-loop-beats sec-per-beat) internal-time-units-per-second))))
                                   
                                   (incf next-loop-start-time loop-dur-ms)
                                   (loop while (< (get-internal-real-time) next-loop-start-time) do
                                     (unless (gethash name *live-loops*)
                                       (return-from thread-execution))
                                     (sleep 0.001))))))))
                     (error (e) (format t "~%[LOOP ERROR] ~A~%" e)))))
               :name (format nil "bogu-loop-~A" name)))))
    nil))

(def-bogu-cmd STOP-LOOP (:symbol) (args)
  "Stops active loops by removing their state flags, ensuring graceful exits."
  (let ((name (car (expand-vars args))))
    (if (eq name 'ALL)
        (progn
          (clrhash *live-loops*)   
          (clrhash *loop-threads*) 
          (format t "~%[LOOP] Disarmed all loops. Awaiting graceful thread exits...~%"))
        (if (gethash name *live-loops*)
            (progn
              (remhash name *live-loops*)   
              (remhash name *loop-threads*) 
              (format t "~%[LOOP] Disarmed loop '~A'.~%" name))
            (format t "~%[LOOP Error] Loop '~A' is not running.~%" name)))
    nil))

;; =============================================================================
;; 7. NOTATION & EXPORT
;; =============================================================================

(def-bogu-cmd ENGRAVE (:symbol :any) (args)
  (let* ((expanded-args (expand-vars args))
         (filename (car expanded-args))
         (instr (cadr expanded-args)))
    (if (and filename instr)
        (bogu->ly (string-downcase (string filename)) instr)
        (format t "~%[Syntax Error] engrave requires a filename and a track number.~%"))))

;; =============================================================================
;; 8. TIMELINE & TRACK STATE MANAGEMENT
;; =============================================================================

(defun clear-score-only ()
  "Flushes the active composition timeline and signals loops to stop safely."
  (clrhash *live-loops*)   
  (clrhash *loop-threads*) 
  (setf *score* '())
  (clrhash *tracks*)
  (setf *master-epoch* nil)
  (format t "~%[SYSTEM] Score cleared. Live-loops signaled to terminate safely. Variables preserved.~%"))

(def-bogu-cmd CLEAR () (args)
  "Flushes the active composition timeline and stops all live loops."
  (clear-score-only)
  nil)

(defun ensure-track (id)
  "Retrieves a track by ID, initializing it if it doesn't exist yet."
  (or (gethash id *tracks*)
      (setf (gethash id *tracks*) (make-track :id id))))

(defun get-current-track ()
  "Retrieves the active track, initializing it if it doesn't exist yet."
  (ensure-track *current-instrument*))

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
                                 :key (track-key v)
                                 :clef (track-clef v))))
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
    
    (dolist (event *score*)
      (if (= (getf event :instr) *current-instrument*)
          (push event track-events)
          (push event other-events)))
          
    (setf track-events (sort track-events #'> :key (lambda (x) (getf x :time))))
    (setf track-events (nthcdr n track-events))
    (setf *score* (append track-events other-events))
    
    (let ((new-time 0.0))
      (dolist (event track-events)
        (setf new-time (max new-time (+ (getf event :time) (getf event :dur)))))
        
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

(defun bpm (n)
  "Sets beats per minute."
  (setf *bpm* '())
  (push "t" *bpm*)
  (push 0 *bpm*)
  (push n *bpm*))


;; =============================================================================
;; 9. SYSTEM COMMAND DICTIONARY WRAPPERS
;; =============================================================================

(def-bogu-cmd BPM (:number) (args) (apply #'bpm (expand-vars args)) nil)
(def-bogu-cmd PLAY (&rest :any) (args) (apply #'play (expand-vars args)) nil)
(def-bogu-cmd SAVE (&rest :any) (args) (apply #'save (expand-vars args)) nil)
(def-bogu-cmd VARS (&rest :any) (args) (apply #'vars (expand-vars args)) nil)
(def-bogu-cmd WHERE (&rest :any) (args) (apply #'where (expand-vars args)) nil)
(def-bogu-cmd HELP (&rest :any) (args) (apply #'help (expand-vars args)) nil)
(def-bogu-cmd RESET (&rest :any) (args) (apply #'reset (expand-vars args)) nil)
(def-bogu-cmd LOAD (&rest :any) (args) (apply #'bogu-load (expand-vars args)) nil)
(def-bogu-cmd DEL (:number) (args) (apply #'del (expand-vars args)) nil)
(def-bogu-cmd SEEK (:any) (args) (apply #'seek (expand-vars args)) nil)
(def-bogu-cmd SYNC (&rest :any) (args) (apply #'sync (expand-vars args)) nil)
(def-bogu-cmd BANG (&rest :any) (args) (apply #'bang (expand-vars args)) nil)
(def-bogu-cmd SYNTH (:number :symbol) (args) (apply #'synth (expand-vars args)) nil)
