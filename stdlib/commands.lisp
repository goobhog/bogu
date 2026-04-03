;; stdlib/commands.lisp
(in-package :bogu)

(defparameter *tracks* (make-hash-table))

;; =============================================================================
;; 1. ENGINE CORE & VALIDATION
;; =============================================================================

(defun validate-signature (cmd-name signature args)
  "Traverses the arguments and ensures they match the command's symbolic blueprint."
  (let* ((arg-count (length args))
         (rest-pos (position '&rest signature))
         ;; THE FIX: Slice the signature to ignore everything after &rest for the minimum calculation
         (required-sigs (if rest-pos (subseq signature 0 rest-pos) signature))
         (min-args (count-if-not (lambda (s) (search "OPTIONAL" (symbol-name s))) required-sigs))
         (max-args (if rest-pos most-positive-fixnum (length signature))))

    ;; 1. Arity Check
    (when (or (< arg-count min-args) (> arg-count max-args))
      (error "[Syntax Error] ~A expects ~A arguments, but received ~A." 
             cmd-name (if (= min-args max-args) min-args (format nil "~A to ~A" min-args (if rest-pos "infinity" max-args))) arg-count))

    ;; 2. Smart State-Machine Type Checking
    (let ((sig-idx 0))
      (loop for provided in args
            for i from 0
            do (let* ((expected (nth sig-idx signature)))
                 (when (eq expected '&rest)
                   (incf sig-idx)
                   (setf expected (nth sig-idx signature)))

                 (let* ((base-type (extract-base-type expected))
                        (is-optional (search "OPTIONAL" (symbol-name expected)))
                        (matches (cond
				  ;; ((and (symbolp provided) (not (keywordp provided))) t)
                                   ((eq base-type :NUMBER) (numberp provided))
                                   ((eq base-type :SYMBOL) (symbolp provided))
                                   ((eq base-type :RHYTHM) (or (numberp provided) (and (symbolp provided) (rtm provided))))
                                   ((eq base-type :AST) (listp provided))
                                   (t t)))) ; :ANY or unknown match automatically
                   
                   (if matches
                       ;; It matches! Advance the pointer (unless we are locked in &rest mode)
                       (unless (and rest-pos (>= sig-idx rest-pos))
                         (incf sig-idx))
                       
                       ;; It doesn't match...
                       (if is-optional
                           ;; Optional skip logic
                           (progn
                             (incf sig-idx)
                             (let* ((next-expected (nth sig-idx signature)))
                               (when (eq next-expected '&rest)
                                 (incf sig-idx)
                                 (setf next-expected (nth sig-idx signature)))
                               (let* ((next-base (extract-base-type next-expected))
                                      (next-matches (cond
                                                      ((eq next-base :NUMBER) (numberp provided))
                                                      ((eq next-base :SYMBOL) (symbolp provided))
                                                      ((eq next-base :RHYTHM) (or (numberp provided) (and (symbolp provided) (rtm provided))))
                                                      ((eq next-base :AST) (listp provided))
                                                      (t t))))
                                 (unless next-matches
                                   (error "[Type Error] ~A expects a ~A at position ~A, but got ~A." cmd-name next-base (1+ i) provided))
                                 ;; Advanced successfully
                                 (unless (and rest-pos (>= sig-idx rest-pos))
                                   (incf sig-idx)))))
                           ;; Not optional, hard error
                           (error "[Type Error] ~A expects a ~A at position ~A, but got ~A." cmd-name base-type (1+ i) provided)))))))
    t))

(defmacro def-bogu-cmd (name signature args &body body)
  "Defines a modular command, registers its signature, and extracts docstrings."
  (let* ((func-name (intern (format nil "CMD-~A" name)))
         (docstring (if (stringp (car body)) (car body) "No documentation available."))
         (actual-body (if (stringp (car body)) (cdr body) body)))
    `(progn
       (defun ,func-name ,args
         (handler-case
             (progn
               ;; THE FIX: ,(car args) extracts the symbol 'args' instead of calling it as a function!
               (validate-signature ',name ',signature ,(car args))
               ,@actual-body)
           (error (e)
             (format t "~%~A~%" e)
             nil)))
       (setf (gethash ',name *command-dictionary*) 
             (list :fn #',func-name :sig ',signature :doc ,docstring)))))

;; =============================================================================
;; 2. SEQUENCING & COMBINATORICS
;; =============================================================================

(def-bogu-cmd SEQ (:rhythm-optional &rest :any) (args)
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

(def-bogu-cmd POLY (:rhythm-optional &rest :any) (args)
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

(def-bogu-cmd SIM (&rest :any) (args)
  "Parallel execution. Evaluates multiple blocks simultaneously. Structurally identical to POLY, but semantically designed for layering larger sequences."
  (let* ((master-events nil))
    (dolist (block args)
      (let ((evaluated-block (execute-ast (list block))))
        (dolist (e evaluated-block)
          ;; Stack everything at 0.0
          (push (copy-list e) master-events))))
    (reverse master-events)))

(def-bogu-cmd CELL (:rhythm :ast) (args)
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

(def-bogu-cmd WAIT (:rhythm-optional) (args)
  "Generates a pure rest of the specified duration. Defaults to 1.0 (Q)."
  (let* ((expanded (expand-vars args))
         (dur (if expanded (rtm (car expanded)) 1.0)))
    (list (list :type :note :pitch-symbol 'RST :time 0.0 :written-dur dur))))


;; =============================================================================
;; 3. GENERATIVE & ALEATORIC ENGINES
;; =============================================================================

(def-bogu-cmd SARP (:rhythm :rhythm &rest :any) (args)
  "Sustained arpeggio. Plays through the provided pool EXACTLY ONCE at step r.
   All notes sustain such that they release together at total duration s."
  (let* ((expanded (expand-vars args))
         (r (rtm (car expanded)))   ; Step duration (when the next note fires)
         (s (rtm (cadr expanded)))  ; The 'sustain pedal' release point (first note's duration)
         (raw-nodes (cddr expanded))
         (nodes (if (and (= (length raw-nodes) 1) (listp (car raw-nodes)))
                    (car raw-nodes)
                    raw-nodes))
         (len (length nodes))
         (local-cursor 0.0)
         (master-events nil))
         
    ;; THE FIX: Loop exactly 'len' times. No modulo, no infinite looping.
    (dotimes (i len)
      (let ((node (nth i nodes)))        
        (let ((evaluated-block (execute-ast (list node)))
              ;; THE FIX: Note 1 gets 's', Note 2 gets 's - r', Note 3 gets 's - 2r'.
              ;; They all mathematically release at the exact same moment.
              ;; (We keep a 0.1 floor just so trailing notes make a tiny sound if 's' is short).
              (physics-dur (max 0.1 (- s (* r i)))))
              
          (dolist (e evaluated-block)
            (let ((new-e (copy-list e)))
              (setf (getf new-e :time) (+ local-cursor (getf e :time)))
              (setf (getf new-e :written-dur) r)
              (setf (getf new-e :dur) physics-dur) 
              (push new-e master-events)))))
      (incf local-cursor r))
      
    ;; Explicitly anchor the end of the block. 
    ;; The grid footprint is either 's' or the length of the arpeggio, whichever is longer.
    (push (list :type :note :pitch-symbol 'RST :time (max s local-cursor) :written-dur 0.0) master-events)
    (reverse master-events)))

(def-bogu-cmd FLUID (:number :rhythm &rest :any) (args)
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

(def-bogu-cmd WALK (:number :rhythm &rest :any) (args)
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


;; =============================================================================
;; 4. TREE TRANSFORMERS & MATH
;; =============================================================================

(def-bogu-cmd TRANSPOSE (:number :ast) (args)
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

(def-bogu-cmd INVERT (:ast) (args)
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

(def-bogu-cmd RETRO (:ast) (args)
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

(def-bogu-cmd RPT (:number :ast) (args)
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

(def-bogu-cmd STACCATO (:number :ast) (args)
  "Tree transformer: Shortens the absolute duration (:dur) of all child notes by a percentage, without altering their written rhythm."
  (let* ((expanded (expand-vars args))
         (percent (/ (float (car expanded)) 100.0))
         (child-block (execute-ast (cdr expanded)))
         (master-events nil))
    (dolist (e child-block)
      (let ((new-e (copy-list e)))
        ;; Only shorten actual notes, leave control data and rests alone
        (when (eq (getf new-e :type) :note)
          ;; If it doesn't have an explicit :dur yet, default to its :written-dur
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
      ;; We append the result of each iteration into one giant 'virtual' block
      (setf combined-result (append combined-result (execute-ast body))))
    combined-result))


;; =============================================================================
;; 5. LOGIC, STATE & VARIABLES
;; =============================================================================

(def-bogu-cmd DEF (:symbol :any) (args)
  (let* ((var-name (car args))
         (var-contents (cdr args))
         (stored-ast (if (and (= (length var-contents) 1) (listp (car var-contents)))
                         (car var-contents) var-contents)))
    (setf (gethash var-name *vars*) stored-ast)
    nil)) ; <-- Force a nil return so the AST stitcher ignores it!

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

(def-bogu-cmd IF (:number :symbol :number :ast :ast-optional) (args)
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


;; =============================================================================
;; 6. MIXER & AUTOMATION
;; =============================================================================

(defmacro def-mixer-cmd (name param-id)
  "Safely generates pure static control data AND sends instant top-level initialization."
  `(def-bogu-cmd ,name (:number) (args)
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

(def-bogu-cmd SWEEP (:symbol :number :number :any) (args)
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
        (if (listp target)
            ;; 1. Higher-Order Mode: Measure ONLY the mathematical grid
            (let* ((child-events (execute-ast target))
                   (grid-len 0.0))
              (dolist (e child-events)
                (setf grid-len (max grid-len (+ (getf e :time) (or (getf e :written-dur) 0.0)))))
              ;; Force :dur to equal :written-dur so the envelope never bleeds
              (cons (list :type :control :time 0.0 :written-dur grid-len :dur grid-len 
                          :param param-id :start start :end end)
                    child-events))
            
            ;; 2. Standard Mode: Target is just a duration
            (let ((dur (if target (rtm target) 4.0)))
               (list (list :type :control :time 0.0 :written-dur 0.0 :dur dur 
                           :param param-id :start start :end end)))))))

;; =============================================================================
;; 7. LIVE-LOOPING & EXECUTION THREADS
;; =============================================================================

(def-bogu-cmd REBOOT () (args)
  (reboot-audio-server))

(def-bogu-cmd DELAY (:number-optional) (args)
  "Pauses the Lisp thread. Useful for letting hardware boot up."
  (sleep (if args (car (expand-vars args)) 1.0))
  nil)

(def-bogu-cmd LIVE-LOOP (:symbol :rhythm-optional :ast) (args)
  "Auto-sizing live-loop. Syntax: (LIVE-LOOP name [block]) OR (LIVE-LOOP name padding [block])"
  (let* ((expanded (expand-vars args))
         (name (car expanded))
	 (instr-id *current-instrument*)
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
                             (*current-instrument* instr-id))
                         
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

(def-bogu-cmd STOP-LOOP (:symbol) (args)
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


;; =============================================================================
;; 8. NOTATION & EXPORT
;; =============================================================================

(def-bogu-cmd ENGRAVE (:symbol :number) (args)
  (let* ((expanded-args (expand-vars args))
         (filename (car expanded-args))
         (instr (cadr expanded-args)))
    (if (and filename instr)
        (bogu->ly (string-downcase (string filename)) instr)
        (format t "~%[Syntax Error] engrave requires a filename and a track number.~%"))))


;; =============================================================================
;; 9. TIMELINE & TRACK STATE MANAGEMENT
;; =============================================================================

(defstruct track
  (id 1 :type integer)
  (playhead 0.0 :type float)
  (transpose-offset 0 :type integer)
  (velocity 0.8 :type float)
  (articulation :legato :type keyword)
  (key nil :type list))

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
;; 10. SYSTEM COMMAND DICTIONARY WRAPPERS
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
