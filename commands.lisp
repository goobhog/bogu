(in-package :bogu)

;;global variables-------------------

;; Multi-Dimensional Time: Every instrument slot gets its own independent playhead.
(defparameter *playheads* (make-hash-table))
(defparameter *bpm* '(60 0 "t"))

(defvar *master-epoch* nil "The exact microsecond the Bogu universe began.")
(defparameter *beats-per-bar* 4.0 "Standard 4/4 time.")
(defparameter *quantize-mode* :exact "Can be :exact, :free, or a decimal (e.g., 0.05) for groove slop.")
(defparameter *current-articulation* nil "Tags notes for both Csound physics and LilyPond notation.")

(defparameter *current-instrument* 1)
(defparameter *current-key* nil)
(defparameter *score* '())
(defparameter *bogu-code* '())
(defparameter *vars* (make-hash-table :test 'equal))
(defparameter *stdlib-vars* (make-hash-table :test 'equal))
(defparameter *current-project* nil)
(defparameter *transpose-offset* 0)
(defparameter *velocity* 0.8) ;; Default to 80% volume
(defparameter *live-loops* (make-hash-table :test 'equal))
(defparameter *loop-threads* (make-hash-table :test 'equal))

(defvar *play-thread* nil)

;; The Master Library of Synth Cartridges
(defparameter *synth-templates* (make-hash-table))

(setf (gethash 'SINE *synth-templates*) 
      "icps = cpspch(p4)
iamp = p5 * 0.15
Svol sprintf \"vol_%d\", int(p1)
Span sprintf \"pan_%d\", int(p1)
Srvb sprintf \"rvb_%d\", int(p1)
ivol chnget Svol
ipan chnget Span
irvb chnget Srvb
kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kvol_sm portk kvol, 0.05, ivol
kpan_sm portk kpan, 0.05, ipan
krvb_sm portk krvb, 0.05, irvb

asig poscil iamp, icps, 2
ares linen asig, .03, p3, .05
aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm")

(setf (gethash 'SAW *synth-templates*) 
      "icps = cpspch(p4)
iamp = p5 * 0.15
Svol sprintf \"vol_%d\", int(p1)
Span sprintf \"pan_%d\", int(p1)
Srvb sprintf \"rvb_%d\", int(p1)
ivol chnget Svol
ipan chnget Span
irvb chnget Srvb
kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kvol_sm portk kvol, 0.05, ivol
kpan_sm portk kpan, 0.05, ipan
krvb_sm portk krvb, 0.05, irvb

asig vco2 iamp, icps, 0
ares linen asig, .03, p3, .05
aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm")

(setf (gethash 'PLUCK *synth-templates*) 
      "icps = cpspch(p4)
iamp = p5 * 0.15
Svol sprintf \"vol_%d\", int(p1)
Span sprintf \"pan_%d\", int(p1)
Srvb sprintf \"rvb_%d\", int(p1)
ivol chnget Svol
ipan chnget Span
irvb chnget Srvb
kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kvol_sm portk kvol, 0.05, ivol
kpan_sm portk kpan, 0.05, ipan
krvb_sm portk krvb, 0.05, irvb

asig pluck iamp, icps, icps, 2, 1
ares linen asig, .01, p3, .1
aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm")

;; The Active Hardware Rack (Slots 1-16)
(defparameter *synth-rack* (make-hash-table))
;; Initialize standard defaults
(setf (gethash 1 *synth-rack*) (gethash 'SINE *synth-templates*))
(setf (gethash 2 *synth-rack*) (gethash 'SAW *synth-templates*))
(setf (gethash 3 *synth-rack*) (gethash 'PLUCK *synth-templates*))
;; Fill the remaining slots with Sine waves as fallbacks
(loop for i from 4 to 16 do
  (setf (gethash i *synth-rack*) (gethash 'SINE *synth-templates*)))

(defun current-time ()
  "Returns the current playhead position for the active instrument."
  (gethash *current-instrument* *playheads* 0.0))

(defun advance-time (amount)
  "Moves the playhead forward ONLY for the active instrument."
  (incf (gethash *current-instrument* *playheads* 0.0) amount))

(defun start-audio-engine ()
  "Initializes the network connection to the external Csound server."
  (setf *random-state* (make-random-state t))
  (format t "~%[SYSTEM] CFFI Decoupled. Bogu is running in pure Brain Mode.~%")
  (format t "[SYSTEM] Ensure you have run 'csound bogu-server.csd' in a separate terminal.~%")
  (boot-osc-bridge))

(defun bang (instr note-symbol &optional (vol 80))
  "Fires a note instantly over OSC."
  (let* ((note-str (string-downcase (string note-symbol)))
         (octave-str (remove-if-not #'digit-char-p note-str))
         (octave (if (string= octave-str "") 4 (parse-integer octave-str)))
         (pitch-str (remove-if #'digit-char-p note-str))
         (pitch-sym (intern (string-upcase pitch-str)))
         (pitch (cdr (assoc pitch-sym *notes*))))

    (unless pitch
      (format t "~%[ERROR] Invalid note syntax for bang: ~A~%" note-symbol)
      (return-from bang))

    (let* ((pch (+ octave 4 (/ pitch 100.0)))
           (vel (/ vol 100.0)))
      (osc-play instr 0.25 pch vel))))

(defun play ()
  "Standalone Brain Scheduler: Computes the score and fires OSC packets."
  (when (and *play-thread* (sb-thread:thread-alive-p *play-thread*))
    (format t "~%[WARNING] Sequence already playing.~%")
    (return-from play))

  (setf *score* (sort *score* #'< :key (lambda (x) (getf x :time))))

  (setf *play-thread*
        (sb-thread:make-thread
         (lambda ()
           (let* ((current-bpm (if *bpm* (car *bpm*) 60.0))
                  (sec-per-beat (float (/ 60.0 current-bpm)))
                  ;; Establish Lisp's internal high-res start time
                  (start-time (get-internal-real-time)))
             
             (format t "~%[BRAIN] Executing score over OSC...~%bogu> ")
             (force-output)

             (dolist (event *score*)
               (when (or (eq (getf event :type) :note)
                         (eq (getf event :type) :control))
                 (let* ((event-time-sec (* (getf event :time) sec-per-beat))
                        (event-dur-sec (* (getf event :dur) sec-per-beat))
                        (target-ms (+ start-time (* event-time-sec internal-time-units-per-second))))
                   
                   (loop while (< (get-internal-real-time) target-ms)
                         do (sleep 0.001))
                   
                   (if (eq (getf event :type) :note)
                       (osc-play (getf event :instr) event-dur-sec (getf event :pch) (getf event :vel))
                       (osc-control (getf event :instr) (getf event :param) event-dur-sec (getf event :start) (getf event :end))))))
             
             (format t "~%[BRAIN] Sequence complete.~%bogu> ")
             (force-output)))
         :name "bogu-osc-scheduler")))

(defun load-stdlib ()
  "Silently flashes the Standard Library into *stdlib-vars* ROM."
  (let ((stdlib-path (comp-path "stdlib" (bogu-folder "stdlib") "bogu")))
    (with-open-file (in stdlib-path :direction :input :if-does-not-exist nil)
      (when in
        (let ((input-string ""))
          (loop for line = (read-line in nil)
                while line do
                  (let ((trimmed-line (string-trim " " line)))
                    (unless (or (string= trimmed-line "") (char= (char trimmed-line 0) #\;))
                      (setf input-string (if (string= input-string "") line (concatenate 'string input-string " & " line)))
                      (when (= (count #\[ input-string) (count #\] input-string))
                        (handler-case
                            (let* ((tokens (lex-bogu-string input-string))
                                   (ast (parse-bogu-tokens tokens)))
                              (when ast 
                                (with-open-stream (*standard-output* (make-broadcast-stream))
                                  ;; THE LISP MAGIC: Temporarily redirect *vars* to *stdlib-vars*!
                                  (let ((*vars* *stdlib-vars*))
                                    (execute-ast ast)))))
                          (error (e) nil))
                        (setf input-string ""))))))))))

(defun reset-bogu ()
  "Resets all global variables to default, kills loops, and reloads the Standard Library."
  ;; 1. Terminate all active live-loop threads
  (maphash (lambda (k v)
             (when (and v (sb-thread:thread-alive-p v))
               (sb-thread:terminate-thread v)))
           *loop-threads*)
  (clrhash *loop-threads*)
  (clrhash *live-loops*)

  ;; 2. Standard Memory Wipe
  (bpm 60)
  (setf *current-instrument* 1)
  (setf *score* '())
  (clrhash *playheads*)
  (setf *bogu-code* '())
  (clrhash *vars*)        
  (setf *current-project* nil)
  (setf *transpose-offset* 0)
  (setf *velocity* 0.8)
  
  (load-stdlib)
  (format t "~%[SYSTEM] Memory wiped. Standard Library online.~%"))

(defun reset ()
  "Allows the 'reset' command to be evaluated directly from a .bogu script."
  (reset-bogu))

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

(defun del (n)
  "Deletes the last n notes entered on the CURRENT instrument, and rewinds its playhead."
  (let ((deleted 0)
        (new-score nil))
    
    ;; Iterate through the score (newest first). Keep notes from OTHER instruments, 
    ;; or if we've already met our deletion quota.
    (dolist (event *score*)
      (if (and (< deleted n) (= (getf event :instr) *current-instrument*))
          (incf deleted)
          (push event new-score)))
    
    ;; Reverse to restore the chronological push-order
    (setf *score* (reverse new-score))
    
    ;; Recalculate the playhead based on this track's NEW last note
    (let ((last-track-note (find *current-instrument* *score* :key (lambda (x) (getf x :instr)))))
      (if last-track-note
          (setf (gethash *current-instrument* *playheads*) 
                (+ (getf last-track-note :time) (getf last-track-note :dur)))
          ;; If no notes are left on this track, rewind all the way to 0
          (setf (gethash *current-instrument* *playheads*) 0.0)))
    
    (format t "~%[TIMELINE] Rewound playhead. Deleted ~a notes from Track ~a.~%" deleted *current-instrument*)))

(defun rpt (n &optional (s 0))
  "Repeats the last n notes of the current instrument, starting from the sth last note."
  ;; Extract ONLY the notes belonging to the active instrument
  (let ((track-notes (remove-if-not (lambda (x) (= (getf x :instr) *current-instrument*)) *score*)))
    
    (if (< (length track-notes) (+ n s))
        (format t "~%[ERROR] Not enough notes on Track ~A to repeat.~%" *current-instrument*)
        
        (let* ((chunk (subseq track-notes s (+ s n)))
               (oldest-note (car (last chunk))) 
               (start-time (getf oldest-note :time))
               (current-playhead (current-time))
               (time-offset (- current-playhead start-time)))
          
          ;; Paste the notes forward in time
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
          
          ;; Snap playhead to the end of the newly pasted block
          (let ((last-new-note (car *score*)))
            (setf (gethash *current-instrument* *playheads*) 
                  (+ (getf last-new-note :time) (getf last-new-note :dur))))))))

(defun seek (beat)
  "Teleports the playhead of the active instrument to an absolute beat."
  (let ((target-time (if (numberp beat) (float beat) (rtm beat))))
    (setf (gethash *current-instrument* *playheads*) target-time)
    (format t "~%[TIMELINE] Track ~A teleported to ~,3fs~%" *current-instrument* target-time)))

(defun sync ()
  "Finds the furthest playhead in the matrix and fast-forwards all tracks to catch up."
  (let ((max-time 0.0))
    ;; Find the maximum time
    (maphash (lambda (k v) (setf max-time (max max-time v))) *playheads*)
    ;; Set all active tracks to that time
    (maphash (lambda (k v) (setf (gethash k *playheads*) max-time)) *playheads*)
    (format t "~%[TIMELINE] All tracks synchronized to ~,3fs~%" max-time)))

(defun synth (slot-id template-name)
  "Loads a synth template into a specific hardware rack slot."
  (let ((template (gethash template-name *synth-templates*)))
    (if template
        (progn
          (setf (gethash slot-id *synth-rack*) template)
          (format t "~%[RACK] Loaded ~A into Slot ~A~%" template-name slot-id))
        (format t "~%[RACK ERROR] No synth template named ~A found in memory.~%" template-name))))

(defun fluid (density rval &rest notes)
  "Scatters an arbitrary <density> of random notes within a time boundary."
  (let* ((r (rtm rval))
         (flat-notes (flatten notes))
         (len (length flat-notes))
         (start-time (current-time)))
         
    (dotimes (i density)
      ;; 1. Pick a completely random note from the provided scale
      (let* ((random-note (nth (random len) flat-notes))
             ;; 2. Pick a random micro-millisecond inside the time bucket
             (random-offset (* r (/ (random 1000) 1000.0))))
        
        ;; 3. Teleport and strike
        (setf (gethash *current-instrument* *playheads*) (+ start-time random-offset))
        (funcall random-note r)))
        
    ;; 4. Order out of Chaos: Snap playhead to the exact END of the boundary
    (setf (gethash *current-instrument* *playheads*) (+ start-time r))))

(defun walk (steps rval &rest notes)
  "Procedurally generates a random walk melody constrained to a specific scale."
  (let* ((r (rtm rval))
         ;; Flatten the list if it came from a variable like 'penta'
         (note-list (if (and (= (length notes) 1) (listp (car notes))) (car notes) notes))
         (len (length note-list))
         ;; Pick a random starting position in the scale
         (current-idx (random len)))
         
    (dotimes (i steps)
      ;; 1. Play the current note and advance the playhead
      (funcall (nth current-idx note-list) r)
      (advance-time r)
      
      ;; 2. The Random Walk: Pick -1 (down), 0 (stay), or 1 (up)
      (let ((step (- (random 3) 1)))
        (setf current-idx (+ current-idx step))
        ;; 3. Wall Collision: If it walks off the top or bottom of the scale, bounce it back!
        (setf current-idx (max 0 (min (- len 1) current-idx)))))))

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

(defun where ()
  "Reports the current chronological position of all active track playheads."
  (format t "~%--- [TIMELINE STATUS] ---~%")
  (if (= (hash-table-count *playheads*) 0)
      (format t " All tracks at: 0.0s (Master Start)~%")
      (maphash (lambda (instr-id time)
                 (format t " Instrument ~2a : ~,3fs ~a~%" 
                         instr-id 
                         time
                         (if (= instr-id *current-instrument*) "<-- (ACTIVE)" "")))
               *playheads*))
  (format t "-------------------------~%")
  t)

(defun schedule-note (pitch-in octave rval &optional instr-override)
  "Armor-Plated: Handles pitch, DIATONIC transposition, and articulation metadata."
  (let* ((pitch (if (numberp pitch-in) pitch-in (cdr (assoc pitch-in *notes*))))
         (instr (or instr-override *current-instrument*)))
    
    (unless pitch 
      (error "Music Math Error: The pitch '~A' is not defined in the *notes* dictionary." pitch-in))

    ;; --- THE DIATONIC ENGINE ---
    (let ((transposed-pitch
           (if (or (null *current-key*) (= *transpose-offset* 0))
               ;; Opt-in bypassed: Absolute Chromatic Math
               (+ pitch *transpose-offset*)
               ;; Diatonic Math Enabled!
               (let* ((root-sym (car *current-key*))
                      (scale-sym (cadr *current-key*))
                      (root-pitch (cdr (assoc root-sym *notes*)))
                      (intervals (cdr (assoc scale-sym *scale-intervals*))))
                 
                 (if (not intervals)
                     (+ pitch *transpose-offset*) ;; Fallback if scale unknown
                     (let* ((scale-pitches (mapcar (lambda (x) (+ root-pitch x)) intervals))
                            (normalized-pitch (mod pitch 12))
                            ;; Find the note's integer degree (0-6) within the scale
                            (degree (position normalized-pitch scale-pitches :key (lambda (x) (mod x 12)))))
                       
                       (if (null degree)
                           (+ pitch *transpose-offset*) ;; If note is out of key, shift chromatically
                           ;; Calculate the Diatonic Shift!
                           (let* ((new-degree (+ degree *transpose-offset*))
                                  (octave-shift (floor new-degree (length scale-pitches)))
                                  (wrapped-degree (mod new-degree (length scale-pitches)))
                                  (new-absolute-pitch (nth wrapped-degree scale-pitches)))
                             (+ new-absolute-pitch (* octave-shift 12))))))))))

      (let ((new-pitch transposed-pitch)
            (new-octave octave))

        ;; Octave Wrapping Math...
        (loop while (> new-pitch 11) do (decf new-pitch 12) (incf new-octave))
        (loop while (< new-pitch 0) do (incf new-pitch 12) (decf new-octave))
        
        ;; --- THE UNIFIED CONCEPT MATH ---
        (let* ((written-rhythm (rtm rval))
               (physics-dur (if (eq *current-articulation* :staccato)
                                (* written-rhythm 0.5)
                                written-rhythm))
               
               (new-event (list :type :note
                                :instr instr
                                :time (current-time)
                                :dur physics-dur          
                                :written-dur written-rhythm 
                                :pitch new-pitch
                                :octave new-octave
                                :pch (+ new-octave 4 (/ new-pitch 100.0))
                                :vel *velocity*
                                :art *current-articulation*))) 
          (push new-event *score*))))))

(defun seq (rval &rest notes)
  "Sequential player. Handles both (seq h c4 e4) and (seq h my-arp-list)."
  (let ((r (rtm rval))
        ;; Flatten the list if the user passed a variable name
        (note-list (if (and (= (length notes) 1) (listp (car notes))) (car notes) notes)))
    (dolist (n note-list)
      (funcall n r)
      (advance-time r))))

(defun poly (rval &rest notes)
  "Polyphonic player. Handles both individual notes and variable lists."
  (let ((r (rtm rval))
        (note-list (if (and (= (length notes) 1) (listp (car notes))) (car notes) notes)))
    (dolist (n note-list)
      (funcall n r))
    (advance-time r)))

(defun sarp (rval sval &rest notes)
  "Sustained arpeggio. The ultimate test of the multitrack playhead."
  (let* ((r (rtm rval))
         (s (rtm sval))
         (start-time (current-time))
         ;; Extract the notes whether they are a 'rest' list or raw args
         (note-list (if (and (= (length notes) 1) (listp (car notes))) (car notes) notes)))
    
    (dotimes (i (length note-list))
      ;; Stagger the track's internal playhead for each step
      (setf (gethash *current-instrument* *playheads*) (+ start-time (* r i)))
      (funcall (elt note-list i) (max 0.01 (- s (* r i)))))
    
    ;; Finalize the playhead at the end of the total sustain
    (setf (gethash *current-instrument* *playheads*) (+ start-time s))))

(defun save (&optional filename)
  "Saves the project, prompting for a name if none exists."
  ;; 1. If they typed a name (e.g., 'save mytrack'), set it as the current project
  (when filename
    (setf *current-project* (string-downcase (string filename))))
  
  ;; 2. If there is still no project name, prompt for one!
  (unless *current-project*
    (format t "Enter a name for this new project: ")
    (finish-output)
    (let ((name (read-line)))
      (if (string= name "")
          (progn (format t "Save cancelled.~%") (return-from save))
          (setf *current-project* name))))
  
  ;; 3. Proceed with saving using *current-project*
  (let ((fname *current-project*))
    (with-open-file (out (comp-path fname (bogu-folder fname) "bogu")
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (dolist (line (reverse *bogu-code*))
          (let* ((parsed (ignore-errors (read-from-string (format nil "(~a)" line))))
                 (cmd (car parsed)))
            (cond 
              ((string= "%" line) (format out "~a~%" line))
              ((or (null parsed)
                   (member cmd '(play save help vars where bogu-load load start-audio-engine)))
               nil)
              (t (format out "~a~%" line)))))))
    (format t "saved \"compositions/~a/~a.bogu\"~%" fname fname)))

(defun bogu-load (&optional filename)
  "Loads a project using the new Lexer -> Parser -> AST Compiler Pipeline."
  ;; 1. Check if we are mid-project
  (when *score*
    (format t "Save your current project before loading a new one? (y/n): ")
    (finish-output)
    (let ((ans (read-line)))
      (when (string= (string-downcase ans) "y")
        (save))))

  ;; 2. Determine what file to load
  (let ((target (if filename 
                    (string-downcase (string filename))
                    (progn
                      (format t "Enter project name to load: ")
                      (finish-output)
                      (read-line)))))
    
    ;; 3. Load with AST Pipeline!
    (when (not (string= target ""))
      (reset-bogu)
      (setf *current-project* target)
      (with-open-file (in (comp-path target (bogu-folder target) "bogu")
                          :direction :input
                          :if-does-not-exist nil)
        (if in
            (let ((input-string ""))
              (loop for line = (read-line in nil)
                    while line do
                      (let ((trimmed-line (string-trim " " line)))
                        ;; Ignore empty lines AND lines that start with a semicolon
                        (unless (or (string= trimmed-line "")
                                    (char= (char trimmed-line 0) #\;))
                          (push line *bogu-code*)
                          ;; Glue the new line to our ongoing block (with ASI)
                          (setf input-string (if (string= input-string "") 
                                                 line 
                                                 (concatenate 'string input-string " & " line)))
                          ;; ONLY evaluate if brackets are completely balanced!
                          (when (= (count #\[ input-string) (count #\] input-string))
                            
                            ;; --- THE NEW COMPILER PIPELINE ---
                            (handler-case
                                (let* ((tokens (lex-bogu-string input-string))
                                       (ast (parse-bogu-tokens tokens)))
                                  (when ast 
                                    (execute-ast ast)))
                              (error (e) 
                                (format t "~%[Compiler Error] Could not parse block in file.~%Details: ~A~%" e)))
                            
                            ;; Reset string for the next command
                            (setf input-string ""))))))
            ;; The ELSE branch if the file is not found
            (format t "Error: File ~a.bogu not found.~%" target)))
      (format t "loaded \"compositions/~a/~a.bogu\"~%" target target))))

(defun calculate-sync-target ()
  "Determines the exact internal-time for a loop to start based on *quantize-mode*."
  ;; 1. The Big Bang: If time hasn't started, start it now.
  (unless *master-epoch*
    (setf *master-epoch* (get-internal-real-time)))
    
  (cond
    ;; MODE: FREE (Start exactly when Enter is pressed)
    ((eq *quantize-mode* :free)
     (get-internal-real-time))
     
    ;; MODE: EXACT or GROOVE SLOP
    (t
     (let* ((now (get-internal-real-time))
            (elapsed-internal (- now *master-epoch*))
            (elapsed-sec (/ elapsed-internal internal-time-units-per-second))
            (current-bpm (if *bpm* (car *bpm*) 60.0))
            (sec-per-beat (/ 60.0 current-bpm))
            (sec-per-bar (* sec-per-beat *beats-per-bar*))
            
            ;; Calculate the next clean downbeat boundary
            (current-bar (floor (/ elapsed-sec sec-per-bar)))
            (next-bar-sec (* (1+ current-bar) sec-per-bar))
            (perfect-target (+ *master-epoch* (round (* next-bar-sec internal-time-units-per-second)))))
                               
       (if (eq *quantize-mode* :exact)
           perfect-target
           ;; Apply "The Pocket": Add/subtract a random fraction of the slop window
           (let* ((slop-sec (if (numberp *quantize-mode*) *quantize-mode* 0.0))
                  (slop-internal (round (* slop-sec internal-time-units-per-second)))
                  (drift (if (> slop-internal 0)
                             (- (random (* 2 slop-internal)) slop-internal)
                             0)))
             (+ perfect-target drift)))))))

;; macros ----------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro generate-notes (pitches octaves)
    "Automatically generates bogu note functions (e.g., C4, EB3). Aligned with new scheduler!"
    `(progn
       ,@(loop for pitch in pitches
               append (loop for octave in octaves
                            for func-name = (intern (string-upcase (format nil "~a~a" pitch octave)))
                            collect `(defun ,func-name (rval)
                                       ;; The Correct Order: Pitch, Octave, Rhythm
                                       (schedule-note ',pitch ,octave rval)))))))

;; Call the macro with specific pitch list and octave range
(generate-notes (a a# bb b cb b# c c# db d d# eb e fb e# f f# gb g g# ab)
                (0 1 2 3 4 5 6 7 8))
