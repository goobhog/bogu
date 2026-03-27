(in-package :bogu)

;;(defparameter *current-project* nil)

(defmacro def-bogu-cmd (name args &body body)
  "A macro that defines a modular command and automatically registers it in the dictionary."
  (let ((func-name (intern (format nil "CMD-~A" name))))
    `(progn
       (defun ,func-name ,args
         ,@body)
       (setf (gethash ',name *command-dictionary*) #',func-name))))

(def-bogu-cmd SEQ (args)
  (let* ((expanded (flatten (expand-vars args)))
         (r (rtm (car expanded)))
         (notes (cdr expanded)))
    (dolist (n notes)
      (cond
        ((or (eq n 'R) (eq n 'RST)) nil) ; Pure rests do nothing
        ((and (symbolp n) (fboundp n)) (funcall (symbol-function n) r))
        (t (format t "~%[SEQ Error] Invalid note: ~A~%" n)))
      (advance-time r))))

(def-bogu-cmd POLY (args)
  (let* ((expanded (flatten (expand-vars args)))
         (r (rtm (car expanded)))
         (notes (cdr expanded)))
    (dolist (n notes)
      (unless (or (eq n 'R) (eq n 'RST))
        (if (and (symbolp n) (fboundp n)) 
            (funcall (symbol-function n) r))))
    (advance-time r)))

(def-bogu-cmd SARP (args)
  (let* ((expanded (flatten (expand-vars args)))
         (r (rtm (car expanded)))
         (s (rtm (cadr expanded)))
         (notes (cddr expanded))
         (len (length notes))
         (start-time (current-time)))
         
    ;; THE FIX: Iterate exactly 'len' times. No more automatic wrap-around/looping!
    (dotimes (i len)
      (set-current-time (+ start-time (* r i)))
      (let ((n (nth i notes)))
        (unless (or (eq n 'R) (eq n 'RST))
          (if (and (symbolp n) (fboundp n))
              ;; Physics duration: calculating the remaining time until 's' is reached
              (funcall (symbol-function n) (max 0.1 (+ 0.5 (- s (* r i)))))))))
              
    ;; Safely advance the playhead to the end of the total specified span
    (set-current-time (+ start-time s))))

(def-bogu-cmd FLUID (args)
  (let* ((expanded (flatten (expand-vars args)))
         (density (if (numberp (car expanded)) (car expanded) (parse-integer (string (car expanded)))))
         (r (rtm (cadr expanded)))
         (notes (cddr expanded))
         (len (length notes))
         (start-time (current-time)))
    (dotimes (i density)
      (let* ((random-note (nth (random len) notes))
             (random-offset (* r (/ (random 1000) 1000.0))))
        (set-current-time (+ start-time random-offset))
        (unless (or (eq random-note 'R) (eq random-note 'RST))
          (if (and (symbolp random-note) (fboundp random-note))
              (funcall (symbol-function random-note) r)))))
    (set-current-time (+ start-time r))))

(def-bogu-cmd CELL (args)
  (let* ((expanded (expand-vars args))
         (cell-duration (rtm (car expanded)))
         (block (cadr expanded))
         (cell-start (current-time)))
    (execute-ast block)
    (set-current-time (+ cell-start cell-duration))))

(def-bogu-cmd SIM (args)
  "Parallel execution with isolated instrument state."
  (let* ((start-time (current-time))
         (max-time start-time)
         (entry-instrument *current-instrument*))
    (dolist (block args)
      (let ((*current-instrument* entry-instrument))
        (set-current-time start-time)
        (execute-ast (list block))
        (when (> (current-time) max-time) 
          (setf max-time (current-time)))))
    (setf *current-instrument* entry-instrument)
    (set-current-time max-time)))

(def-bogu-cmd WALK (args)
  (let* ((expanded (flatten (expand-vars args)))
         (steps (if (numberp (car expanded)) (car expanded) (parse-integer (string (car expanded)))))
         (r (rtm (cadr expanded)))
         (notes (cddr expanded))
         (len (length notes))
         (current-idx (random len)))
    (dotimes (i steps)
      (let ((n (nth current-idx notes)))
        (unless (or (eq n 'R) (eq n 'RST))
          (if (and (symbolp n) (fboundp n))
              (funcall (symbol-function n) r))))
      (advance-time r)
      (let ((step (- (random 3) 1)))
        (setf current-idx (+ current-idx step))
        (setf current-idx (max 0 (min (- len 1) current-idx)))))))


;; --- SYSTEM COMMAND DICTIONARY WRAPPERS ---
;; These automatically route top-level Lisp functions into Bogu's Hash Table!
(def-bogu-cmd BPM (args) (apply #'bpm (expand-vars args)))
(def-bogu-cmd PLAY (args) (apply #'play (expand-vars args)))
(def-bogu-cmd SAVE (args) (apply #'save (expand-vars args)))
(def-bogu-cmd VARS (args) (apply #'vars (expand-vars args)))
(def-bogu-cmd WHERE (args) (apply #'where (expand-vars args)))
(def-bogu-cmd HELP (args) (apply #'help (expand-vars args)))
(def-bogu-cmd RESET (args) (apply #'reset (expand-vars args)))
(def-bogu-cmd LOAD (args) (apply #'bogu-load (expand-vars args)))
(def-bogu-cmd DEL (args) (apply #'del (expand-vars args)))
(def-bogu-cmd RPT (args) (apply #'rpt (expand-vars args)))
(def-bogu-cmd SEEK (args) (apply #'seek (expand-vars args)))
(def-bogu-cmd SYNC (args) (apply #'sync (expand-vars args)))
(def-bogu-cmd BANG (args) (apply #'bang (expand-vars args)))
(def-bogu-cmd SYNTH (args) (apply #'synth (expand-vars args)))

;; --- ARRANGEMENT & LOGIC ---
(def-bogu-cmd DEF (args)
  (let* ((var-name (car args))
         (var-contents (cdr args))
         (stored-ast (if (and (= (length var-contents) 1) (listp (car var-contents)))
                         (car var-contents) var-contents)))
    (setf (gethash var-name *vars*) stored-ast)
    (format t "~%[Bound] ~A -> ~A~%" var-name stored-ast)))

(def-bogu-cmd WAIT (args)
  (advance-time (rtm (car (expand-vars args)))))

(def-bogu-cmd LOOP (args)
  (let* ((expanded (expand-vars args)))
    (dotimes (i (car expanded)) (execute-ast (cadr expanded)))))

(def-bogu-cmd CHANCE (args)
  (let* ((expanded (expand-vars args))
         (p (car expanded)))
    (if (< (random 100) p)
        (execute-ast (cadr expanded))
        (when (caddr expanded) (execute-ast (caddr expanded))))))

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

(def-bogu-cmd I (args)
  (setf *current-instrument* (car (expand-vars args)))
  (format t "~%[TRACK] Switched to Instrument ~A~%" *current-instrument*))

;; --- MIXER MACROS ---
(defmacro def-mixer-cmd (name param-id)
  "A Lisp Macro that safely schedules Bogu mixer commands on the timeline."
  `(def-bogu-cmd ,name (args)
     (let* ((expanded (expand-vars args))
            (val (/ (float (car expanded)) 100.0)))
       ;; THE FIX: Push to the *score* instead of firing instantly!
       (push (list :type :control 
                   :instr *current-instrument* :time (current-time)
                   :dur 0.01 
                   :param ,param-id 
                   :start val 
                   :end val) 
             *score*))))

(def-mixer-cmd VOL 1)
(def-mixer-cmd PAN 2)
(def-mixer-cmd REVERB 3)
(def-mixer-cmd FLT 4)

;; --- AUTOMATION ---
(def-bogu-cmd SWEEP (args)
  "Schedules a mixer parameter automation to execute perfectly in sync."
  (let* ((expanded (expand-vars args))
         (param-sym (car expanded))
         (start (/ (float (cadr expanded)) 100.0))
         (end (/ (float (caddr expanded)) 100.0))
         (dur (rtm (cadddr expanded)))
         (param-id (case param-sym
                     ((VOL V) 1)
                     ((PAN P) 2)
                     ((REVERB RVB) 3)
                     ((FLT F) 4)
                     (otherwise 1))))
    
    ;; THE FIX: Schedule it, and DO NOT advance the playhead! 
    ;; (This allows 'wait' to safely handle the timing in your .bogu scripts)
    (push (list :type :control 
                :instr *current-instrument* :time (current-time)
                :dur dur 
                :param param-id 
                :start start 
                :end end) 
          *score*)))

(def-bogu-cmd KEY (args)
  (let* ((expanded (expand-vars args))
         (trk (get-current-track)))
    (if (or (null expanded) (eq (car expanded) 'OFF) (eq (car expanded) 'NIL))
        (progn (setf (track-key trk) nil) 
               (format t "~%[THEORY] Diatonic Mode OFF for Track ~A.~%" (track-id trk)))
        (progn (setf (track-key trk) (list (car expanded) (cadr expanded)))
               (format t "~%[THEORY] Track ~A Key set to ~A ~A.~%" 
                       (track-id trk) (car expanded) (cadr expanded))))))

(def-bogu-cmd TRANSPOSE (args)
  (let* ((expanded (expand-vars args))
         (offset (if (numberp (car expanded)) (car expanded) (parse-integer (string (car expanded)))))
         (trk (get-current-track))
         (old-offset (track-transpose-offset trk)))
    ;; Explicitly mutate the TRACK state!
    (setf (track-transpose-offset trk) (+ old-offset offset))
    (unwind-protect
        (if (and (listp (cdr expanded)) (listp (cadr expanded)))
            (execute-ast (cdr expanded)) 
            (execute-node (cdr expanded)))
      ;; Safely revert the track state when the block is done
      (setf (track-transpose-offset trk) old-offset))))

;; --- SYSTEM & LIVE LOOPS ---
(def-bogu-cmd REBOOT (args)
  (reboot-audio-server))

(def-bogu-cmd LIVE-LOOP (args)
  "Standardized live-loop that kills existing threads on update."
  (let* ((expanded (expand-vars args))
         (name (car expanded))
         (duration (rtm (cadr expanded)))
         (block (caddr expanded))
         (old-thread (gethash name *loop-threads*)))
    (when (and old-thread (sb-thread:thread-alive-p old-thread))
      (sb-thread:terminate-thread old-thread))
    (setf (gethash name *live-loops*) block)
    (format t "~%[LOOP] Armed live-loop '~A' (~A beats).~%" name duration)
    (setf (gethash name *loop-threads*)
          (sb-thread:make-thread
           (lambda ()
             (handler-case 
                 (let* ((loop-dur duration)
                        ;; The sync target must be an exact integer
                        (next-loop-start-time (calculate-sync-target duration)))
                   (loop
                     (let ((current-block (gethash name *live-loops*)))
                       (unless current-block (return))
                       
                       ;; Fresh tracks and score for each iteration!
                       (let ((*score* '())
                             (*tracks* (clone-tracks-for-sandbox))
                             (*current-instrument* 1))
                         
                         (execute-ast current-block)
                         (setf *score* (sort *score* #'< :key (lambda (x) (getf x :time))))
                         
                         (let* ((current-bpm (if *bpm* (car *bpm*) 60.0))
                                (sec-per-beat (float (/ 60.0 current-bpm))))
                           
                           ;; Execute the sorted timeline
                           (dolist (event *score*)
                             ;; THE FIX: Force the millisecond offset back into a precise integer before adding
                             (let* ((offset-ms (round (* (* (getf event :time) sec-per-beat) internal-time-units-per-second)))
                                    (target-ms (+ next-loop-start-time offset-ms)))
                               
                               ;; Wait for this specific event's exact timestamp
                               (loop while (< (get-internal-real-time) target-ms) do (sleep 0.001))
                               
                               ;; Fire OSC
                               (if (eq (getf event :type) :note)
                                   (osc-play (getf event :instr) (* (getf event :dur) sec-per-beat) (getf event :pch) (getf event :vel))
                                   (osc-control (getf event :instr) (getf event :param) (* (getf event :dur) sec-per-beat) (getf event :start) (getf event :end))))))
                         
                         ;; Safely advance the exact integer time target for the next loop
                         (let ((loop-dur-ms (round (* (* loop-dur (float (/ 60.0 (if *bpm* (car *bpm*) 60.0)))) internal-time-units-per-second))))
                           (incf next-loop-start-time loop-dur-ms))
                         
                         ;; Wait until the start of the next 32-beat block
                         (loop while (< (get-internal-real-time) next-loop-start-time) do (sleep 0.001))))))
               (error (e) (format t "~%[LOOP ERROR] ~A~%" e))))
           :name (format nil "bogu-loop-~A" name)))))

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
              (format t "~%[LOOP Error] Not running.~%"))))))

;; --- ALGORITHMIC MANIPULATION ---
(def-bogu-cmd RETRO (args)
  (let* ((block (car args))
         (sandbox-score '())
         (start-time (current-time)))
         
    (let ((*score* '())
          (*tracks* (clone-tracks-for-sandbox)))
      (set-current-time 0.0)
      
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
        (set-current-time (+ start-time max-time))))))

(def-bogu-cmd INVERT (args)
  (let* ((block (car args))
         (sandbox-score '())
         (start-time (current-time)))
         
    ;; 1. The Sandbox Capture
    (let ((*score* '())
          (*tracks* (clone-tracks-for-sandbox)))
      (set-current-time 0.0)
      
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
        (set-current-time (+ start-time max-time))))))

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
  "Creates a perfect copy of all track states (keys, velocities) but resets their playheads to 0.0 for isolated loop generation."
  (let ((new-ht (make-hash-table)))
    (maphash (lambda (k v)
               (setf (gethash k new-ht)
                     (make-track :id (track-id v)
                                 :playhead 0.0  ;; Safely zeroed out for the sandbox timeline!
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
  (let ((deleted 0)
        (new-score nil))
    (dolist (event *score*)
      (if (and (< deleted n) (= (getf event :instr) *current-instrument*))
          (incf deleted)
          (push event new-score)))
    (setf *score* (reverse new-score))
    (let ((last-track-note (find *current-instrument* *score* :key (lambda (x) (getf x :instr)))))
      (if last-track-note
          (set-current-time (+ (getf last-track-note :time) (getf last-track-note :dur)))
          (set-current-time 0.0)))
    (format t "~%[TIMELINE] Rewound playhead. Deleted ~a notes from Track ~a.~%" deleted *current-instrument*)))

(defun rpt (n &optional (s 0))
  (let ((track-notes (remove-if-not (lambda (x) (= (getf x :instr) *current-instrument*)) *score*)))
    (if (< (length track-notes) (+ n s))
        (format t "~%[ERROR] Not enough notes on Track ~A to repeat.~%" *current-instrument*)
        (let* ((chunk (subseq track-notes s (+ s n)))
               (oldest-note (car (last chunk))) 
               (start-time (getf oldest-note :time))
               (current-playhead (current-time))
               (time-offset (- current-playhead start-time)))
          (dolist (event (reverse chunk))
            (let ((instr (getf event :instr))
                  (old-time (getf event :time))
                  (dur (getf event :dur))
                  (pitch (getf event :pitch))
                  (octave (getf event :octave))
                  (pch (getf event :pch))
                  (vel (getf event :vel))) 
              (push (list :type :note :instr instr :time (+ old-time time-offset) 
                          :dur dur :pitch pitch :octave octave :pch pch :vel vel) 
                    *score*)))
          (let ((last-new-note (car *score*)))
            (set-current-time (+ (getf last-new-note :time) (getf last-new-note :dur))))))))

(defun def (name &rest body)
  "Binds a bogu command to a variable in an O(1) Hash Table."
  (setf (gethash name *vars*) body)
  (format t "~%[Bound] ~a -> ~a~%" name body))

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

(defun rst (rval)
  "A silent rest. Does nothing, allowing seq/poly to naturally advance the playhead."
  (declare (ignore rval))
  nil)

(defun bpm (n); add optional nth bpms
  "Sets beats per minute."
  (setf *bpm* '())
  (push "t" *bpm*)
  (push 0 *bpm*)
  (push n *bpm*))

(defun schedule-note (pitch-in octave rval &optional instr-override)
  "Armor-Plated: Handles pitch, DIATONIC transposition, and articulation via Track State."
  (let* ((pitch (if (numberp pitch-in) pitch-in (cdr (assoc pitch-in *notes*))))
         (instr (or instr-override *current-instrument*))
         (trk (get-current-track))
         (trk-key (track-key trk))
         (trk-transpose (track-transpose-offset trk))
         (trk-velocity (track-velocity trk))
         (trk-art (track-articulation trk)))
    
    (unless pitch (error "Music Math Error: The pitch '~A' is not defined in the *notes* dictionary." pitch-in))

    ;; --- THE DIATONIC ENGINE ---
    (let ((transposed-pitch
           (if (or (null trk-key) (= trk-transpose 0))
               (+ pitch trk-transpose) ;; Chromatic Fallback
               ;; FUZZY FIND: Ignore Lisp package and case differences completely
               (let* ((root-str (string (car trk-key)))
                      (scale-str (string (cadr trk-key)))
                      (root-assoc (find-if (lambda (x) (string-equal (string (car x)) root-str)) *notes*))
                      (intervals-assoc (find-if (lambda (x) (string-equal (string (car x)) scale-str)) *scale-intervals*)))
                 
                 (if (or (not root-assoc) (not intervals-assoc))
                     (+ pitch trk-transpose) ;; Dictionary miss fallback
                     (let* ((root-pitch (round (cdr root-assoc)))
                            (intervals (cdr intervals-assoc))
                            ;; Create absolute scale steps
                            (scale-pitches (mapcar (lambda (x) (+ root-pitch x)) intervals))
                            (normalized-pitch (mod (round pitch) 12))
                            ;; Find index matching the pitch class
                            (degree (position normalized-pitch scale-pitches :key (lambda (x) (mod x 12)))))
                       
                       (if (null degree)
                           (+ pitch trk-transpose) ;; Fallback if note isn't in scale
                           ;; DIATONIC DELTA MATH
                           (let* ((new-degree (+ degree trk-transpose))
                                  (octave-shift (floor new-degree (length scale-pitches)))
                                  (wrapped-degree (mod new-degree (length scale-pitches)))
                                  
                                  (original-absolute (nth degree scale-pitches))
                                  (new-absolute (nth wrapped-degree scale-pitches))
                                  (total-new-absolute (+ new-absolute (* octave-shift 12)))
                                  
                                  (delta (- total-new-absolute original-absolute)))
                             ;; Apply precise semitone delta
                             (+ pitch delta)))))))))

      (let ((new-pitch transposed-pitch)
            (new-octave octave))

        (loop while (> new-pitch 11) do (decf new-pitch 12) (incf new-octave))
        (loop while (< new-pitch 0) do (incf new-pitch 12) (decf new-octave))
        
        (let* ((written-rhythm (rtm rval))
               (physics-dur (if (eq trk-art :staccato) (* written-rhythm 0.5) written-rhythm))
               
               (new-event (list :type :note
                                :instr instr
                                :time (current-time)
                                :dur physics-dur          
                                :written-dur written-rhythm 
                                :pitch new-pitch
                                :octave new-octave
                                :pch (+ new-octave 4 (/ new-pitch 100.0))
                                :vel trk-velocity
                                :art trk-art))) 
          (push new-event *score*))))))
