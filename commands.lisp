;;global variables-------------------

;; Multi-Dimensional Time: Every instrument slot gets its own independent playhead.
(defparameter *playheads* (make-hash-table))
(defparameter *bpm* '(60 0 "t"))
(defparameter *current-instrument* 1)
(defparameter *score* '())
(defparameter *bogu-code* '())
(defparameter *vars* (make-hash-table :test 'equal))
(defparameter *current-project* nil)
(defparameter *transpose-offset* 0)
(defparameter *velocity* 0.8) ;; Default to 80% volume

(defvar *play-thread* nil)
(defvar *csound-process* nil)

;; The Master Library of Synth Cartridges
(defparameter *synth-templates* (make-hash-table))

(setf (gethash 'SINE *synth-templates*) 
      "ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig")

(setf (gethash 'SAW *synth-templates*) 
      "ares linen p5, .03, p3, .02
asig vco2 ares, cpspch(p4), 0
outs asig, asig")

(setf (gethash 'PLUCK *synth-templates*) 
      "icps = cpspch(p4)
asig pluck p5, icps, icps, 0, 1
ares linen asig, .01, p3, .1
outs ares, ares")

;; The Active Hardware Rack (Pre-loaded with 16 Sine Waves for backward compatibility)
(defparameter *synth-rack* (make-hash-table))
(dotimes (i 16)
  (setf (gethash (1+ i) *synth-rack*) (gethash 'SINE *synth-templates*)))

;; bogu functions -----------------------------------------------

(defun current-time ()
  "Returns the current playhead position for the active instrument."
  (gethash *current-instrument* *playheads* 0.0))

(defun advance-time (amount)
  "Moves the playhead forward ONLY for the active instrument."
  (incf (gethash *current-instrument* *playheads* 0.0) amount))

(defun start-audio-engine ()
  "The Hot-Swap BIOS Flasher. Kills the zombie, rewrites the ROM, and reboots."
  ;; 1. The Assassination (Upgraded for strict Linux process trees)
  (ignore-errors (sb-ext:run-program "pkill" '("-9" "csound") :search t :wait t))
  (ignore-errors (sb-ext:run-program "killall" '("-9" "csound") :search t :wait t))
  
  ;; Give the ALSA/PulseAudio driver time to release!
  (sleep 0.5) 

  ;; 2. The BIOS Flash
  (with-open-file (out "compositions/live-bogu.csd" :direction :output :if-exists :supersede)
    (format out "<CsoundSynthesizer>~%<CsOptions>~%-odac -L stdin~%</CsOptions>~%<CsInstruments>~%")
    (format out "sr = 44100~%ksmps = 32~%nchnls = 2~%0dbfs = 4~%")
    (format out "giwave ftgen 2, 0, 4096, 10, 1~%") ;; Base sine table
    
    (maphash (lambda (id code)
               (format out "instr ~a~%~a~%endin~%~%" id code))
             *synth-rack*)
             
    (format out "</CsInstruments>~%<CsScore>~%f0 36000~%e~%</CsScore>~%</CsoundSynthesizer>"))

  ;; 3. The Resurrection (BLINDFOLD REMOVED: :output t)
  (setf *csound-process*
        (sb-ext:run-program "csound" '("compositions/live-bogu.csd")
                            :search t :input :stream :output nil :wait nil))
                            
  (format t "~%[Hardware] Audio Engine Rebooted. ~a Synths Online.~%" (hash-table-count *synth-rack*)))

(defun reset-bogu ()
  "Resets all global variables to their default Multi-Dimensional values."
  (bpm 60)
  (setf *current-instrument* 1)
  (setf *score* '())
  (clrhash *playheads*)
  (setf *bogu-code* '())
  (clrhash *vars*)        
  (setf *current-project* nil)
  (setf *transpose-offset* 0)
  (setf *velocity* 0.8)
  (format t "~%[SYSTEM] Memory and timelines wiped.~%"))

(defun def (name &rest body)
  "Binds a bogu command to a variable in an O(1) Hash Table."
  (setf (gethash name *vars*) body)
  (format t "~%[Bound] ~a -> ~a~%" name body))

(defun vars ()
  "Displays the current ledger of all user-defined variables."
  (if (= (hash-table-count *vars*) 0)
      (format t "~%nothing here yet...~%~%")
      (progn
        (format t "~%--- BOGU VARIABLES ---~%")
        (maphash (lambda (k v) (format t "~a: ~a~%" k v)) *vars*)
        (format t "----------------------~%~%"))))

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

(defun fluid (rval &rest notes)
  "Scatters notes randomly (unquantized) within a time boundary for the current track."
  (let* ((boundary (rtm rval))
         (start-time (current-time))
         (end-time (+ start-time boundary)))
    
    (dolist (n notes)
      ;; 1. Teleport this track's insertion cursor to a random micro-timestamp
      (setf (gethash *current-instrument* *playheads*) 
            (+ start-time (random (float boundary))))
      
      ;; 2. Fire the note! 
      (let ((grain-dur (+ 0.05 (random 0.2))))
        (funcall n grain-dur)))
        
    ;; 3. Order out of Chaos: Snap the track cursor perfectly to the end of the boundary
    (setf (gethash *current-instrument* *playheads*) end-time)))

(defun rst (rval)
  "Advances the playhead of the current instrument by the rhythm value."
  (advance-time (rtm rval)))

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
  "Armor-Plated: Handles both pitch names (C#) and raw numbers (1)."
  (let* (;; 1. Convert symbol to number, but ONLY if it's not already a number!
         ;; THE FIX: Use ASSOC to search the A-List, not GETHASH!
         (pitch (if (numberp pitch-in) 
                    pitch-in 
                    (cdr (assoc pitch-in *notes*))))
         (new-pitch pitch)
         (new-octave octave)
         (instr (or instr-override *current-instrument*)))
    
    ;; Safety check: if lookup fails, report a clear error
    (unless pitch 
      (error "Music Math Error: The pitch '~A' is not defined in the *notes* dictionary." pitch-in))

    ;; 2. Standard Lisp Octave Wrapping (Fixes the transposition math)
    (loop while (> new-pitch 11) do
          (decf new-pitch 12)
          (incf new-octave))
          
    (loop while (< new-pitch 0) do
          (incf new-pitch 12)
          (decf new-octave))
    
    ;; 3. Build the event with pure math (no more redundant lookups!)
    (let ((new-event (list :type :note
                           :instr instr
                           :time (current-time)
                           :dur (rtm rval)
                           :pitch new-pitch
                           :octave new-octave
                           ;; C4 (Octave 4) becomes 8.00 in Csound PCH
                           :pch (+ new-octave 4 (/ new-pitch 100.0))
                           :vel *velocity*)))
      (push new-event *score*))))

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
                   (member cmd '(play save help vars where bogu-load load play-live start-audio-engine)))
               nil)
              (t (format out "~a~%" line)))))))
    (format t "saved \"compositions/~a/~a.bogu\"~%" fname fname)))

(defun play (&optional filename)
  "Compiles the current state into a .csd and plays it."
  (when filename
    (setf *current-project* (string-downcase (string filename))))
    
  (unless *current-project*
    (format t "Error: This project has no name yet. Please type 'save' to name it first.~%")
    (return-from play))
    
  (let ((fname *current-project*))
    (bogu->csd fname)
    (format t "playing \"compositions/~a/~a.csd\"...~%" fname fname)
    (sb-ext:run-program "/usr/bin/csound" 
                        (list (namestring (comp-path fname (bogu-folder fname) "csd")))
                        :search t   
                        :output t   
                        :error t)))

(defun play-live ()
  "Plays the *score* queue in a non-blocking background thread, respecting BPM."
  (when (and *play-thread* (sb-thread:thread-alive-p *play-thread*))
    (format t "~%[WARNING] Sequence already playing. Type 'stop' first.~%")
    (return-from play-live))
    
  (setf *score* (sort *score* #'< :key (lambda (x) (getf x :time))))
  
  (setf *play-thread*
        (sb-thread:make-thread
         (lambda ()
           (let ((csound-in (sb-ext:process-input *csound-process*))
                 ;; Extract the BPM value. *bpm* is stored as a list like (144 0 "t")
                 (current-bpm (if *bpm* (car *bpm*) 60.0)))
             
             (format t "~%[LIVE ENGINE STARTED] Streaming at ~A BPM...~%bogu> " current-bpm)
             (force-output)
             
             (let ((start-time-internal (get-internal-real-time))
                   ;; Calculate how many seconds one beat actually takes
                   (sec-per-beat (/ 60.0 current-bpm)))
               
               (dolist (event *score*)
                 (let* ((event-time-beats (getf event :time))
                        (event-dur-beats (getf event :dur))
                        
                        ;; CONVERT BEATS TO SECONDS based on the BPM!
                        (event-time-sec (* event-time-beats sec-per-beat))
                        (event-dur-sec (* event-dur-beats sec-per-beat))
                        
                        (target-internal (+ start-time-internal 
                                            (* event-time-sec internal-time-units-per-second))))
                   (loop
                     (let ((now (get-internal-real-time)))
                       (if (>= now target-internal)
                           (return)
                           (sleep (max 0.001 (/ (- target-internal now) internal-time-units-per-second))))))
                   
                   ;; Send the SCALED duration to Csound so fast notes don't bleed together!
                   (format csound-in "i ~a 0 ~,3f ~a ~a~%" 
                           (getf event :instr) event-dur-sec 
                           (getf event :pch) (getf event :vel))
                   (force-output csound-in)))
               
               ;; Complete Memory Wipe
               (setf *score* nil)
               (clrhash *playheads*)
               (format t "~%[LIVE ENGINE STOPPED] Sequence complete.~%bogu> ")
               (force-output))))
         :name "bogu-sequencer")))

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
