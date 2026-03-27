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
         (start-time (current-time))
         (num-steps (floor (/ s r))))
    (dotimes (i num-steps)
      (setf (gethash *current-instrument* *playheads*) (+ start-time (* r i)))
      (let ((n (nth (mod i len) notes)))
        (unless (or (eq n 'R) (eq n 'RST))
          (if (and (symbolp n) (fboundp n))
              ;; THE FIX: The "Sustain Pedal" math. 
              ;; Notes fade smoothly instead of stacking into 64-voice oblivion.
              (funcall (symbol-function n) (max 0.1 (+ 0.5 (- s (* r i)))))))))
    (setf (gethash *current-instrument* *playheads*) (+ start-time s))))

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
        (setf (gethash *current-instrument* *playheads*) (+ start-time random-offset))
        (unless (or (eq random-note 'R) (eq random-note 'RST))
          (if (and (symbolp random-note) (fboundp random-note))
              (funcall (symbol-function random-note) r)))))
    (setf (gethash *current-instrument* *playheads*) (+ start-time r))))

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

(def-bogu-cmd CELL (args)
  (let* ((expanded (expand-vars args))
         (cell-duration (rtm (car expanded)))
         (block (cadr expanded))
         (cell-start (current-time)))
    (execute-ast block)
    (setf (gethash *current-instrument* *playheads*) (+ cell-start cell-duration))))

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

;; --- MIXER & SYNTHESIS ---
(def-bogu-cmd I (args)
  (setf *current-instrument* (car (expand-vars args)))
  (format t "~%[TRACK] Switched to Instrument ~A~%" *current-instrument*))

(def-bogu-cmd KEY (args)
  (let ((expanded (expand-vars args)))
    (if (or (null expanded) (eq (car expanded) 'OFF) (eq (car expanded) 'NIL))
        (progn (setf *current-key* nil) (format t "~%[THEORY] Diatonic Mode OFF.~%"))
        (progn (setf *current-key* (list (car expanded) (cadr expanded)))
               (format t "~%[THEORY] Key set to ~A ~A.~%" (car expanded) (cadr expanded))))))

(def-bogu-cmd VOL (args)
  (osc-control *current-instrument* 1 0.01 (/ (car (expand-vars args)) 100.0) (/ (car (expand-vars args)) 100.0)))

(def-bogu-cmd PAN (args)
  (osc-control *current-instrument* 2 0.01 (/ (car (expand-vars args)) 100.0) (/ (car (expand-vars args)) 100.0)))

(def-bogu-cmd REVERB (args)
  (osc-control *current-instrument* 3 0.01 (/ (car (expand-vars args)) 100.0) (/ (car (expand-vars args)) 100.0)))

(def-bogu-cmd FLT (args)
  (osc-control *current-instrument* 4 0.01 (/ (car (expand-vars args)) 100.0) (/ (car (expand-vars args)) 100.0)))

(def-bogu-cmd SWEEP (args)
  (let* ((expanded (expand-vars args))
         (param-sym (car expanded))
         (start (/ (cadr expanded) 100.0))
         (end (/ (caddr expanded) 100.0))
         (dur (rtm (cadddr expanded)))
         (param-id (cond ((eq param-sym 'VOL) 1) ((eq param-sym 'PAN) 2)
                         ((eq param-sym 'REVERB) 3) ((eq param-sym 'FLT) 4) (t 1))))
    (push (list :type :control :instr *current-instrument* :time (current-time)
                :dur dur :param param-id :start start :end end) *score*)))

(def-bogu-cmd TRANSPOSE (args)
  (let* ((expanded (expand-vars args))
         (offset (if (numberp (car expanded)) (car expanded) (parse-integer (string (car expanded))))))
    (let ((*transpose-offset* (+ *transpose-offset* offset)))
      (if (and (listp (cdr expanded)) (listp (cadr expanded)))
          (execute-ast (cdr expanded)) (execute-node (cdr expanded))))))

;; --- SYSTEM & LIVE LOOPS ---
(def-bogu-cmd REBOOT (args)
  (reboot-audio-server))

(def-bogu-cmd SIM (args)
  "Parallel execution with isolated instrument state."
  (let* ((start-time (current-time))
         (max-time start-time)
         (entry-instrument *current-instrument*))
    (dolist (block args)
      ;; Bind locally so instrument switches inside blocks don't leak!
      (let ((*current-instrument* entry-instrument))
        (setf (gethash *current-instrument* *playheads*) start-time)
        (execute-ast (list block))
        (when (> (current-time) max-time) 
          (setf max-time (current-time)))))
    (setf *current-instrument* entry-instrument)
    (setf (gethash *current-instrument* *playheads*) max-time)))

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
                 (let* ((sec-per-beat (float (/ 60.0 (if *bpm* (car *bpm*) 60.0))))
                        (loop-dur duration)
                        ;; THE FIX: Tell the sync target how long this loop actually is!
                        (next-loop-start-time (calculate-sync-target duration)))
                   (loop
                     (let ((current-block (gethash name *live-loops*)))
                       (unless current-block (return))
                       (let ((*score* '())
                             (*playheads* (make-hash-table))
                             (*current-instrument* 1)
                             (*transpose-offset* 0))
                         (execute-ast current-block)
                         (setf *score* (sort *score* #'< :key (lambda (x) (getf x :time))))
                         (let ((sec-per-beat (float (/ 60.0 (if *bpm* (car *bpm*) 60.0)))))
                           (dolist (event *score*)
                             (let* ((target-ms (+ next-loop-start-time (* (* (getf event :time) sec-per-beat) internal-time-units-per-second))))
                               (loop while (< (get-internal-real-time) target-ms) do (sleep 0.001))
                               (if (eq (getf event :type) :note)
                                   (osc-play (getf event :instr) (* (getf event :dur) sec-per-beat) (getf event :pch) (getf event :vel))
                                   (osc-control (getf event :instr) (getf event :param) (* (getf event :dur) sec-per-beat) (getf event :start) (getf event :end))))))
                         (incf next-loop-start-time (* (* loop-dur sec-per-beat) internal-time-units-per-second))
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
        (setf (gethash *current-instrument* *playheads*) (+ start-time max-time))))))

(def-bogu-cmd INVERT (args)
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
        (setf (gethash *current-instrument* *playheads*) (+ start-time max-time))))))


;; --- NOTATION ---

(def-bogu-cmd ENGRAVE (args)
  (let* ((expanded-args (expand-vars args))
         (filename (car expanded-args))
         (instr (cadr expanded-args)))
    (if (and filename instr)
        (bogu->ly (string-downcase (string filename)) instr)
        (format t "~%[Syntax Error] engrave requires a filename and a track number.~%"))))

;; The Master Library of Synth Cartridges

(setf (gethash 'SINE *synth-templates*) 
      "icps = cpspch(p4)
iamp = p5 * 0.15
Svol sprintf \"vol_%d\", int(p1)
Span sprintf \"pan_%d\", int(p1)
Srvb sprintf \"rvb_%d\", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb

;; THE FIX: Removed initial values so it glides softly from zero!
kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05

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
Sflt sprintf \"flt_%d\", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

kcutoff = cpsoct((kflt_sm * 10) + 4)
asig vco2 iamp, icps, 0
afilt moogladder asig, kcutoff, 0.25
ares linen afilt, .03, p3, .05

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
Sflt sprintf \"flt_%d\", int(p1)

kvol chnget Svol
kpan chnget Span
krvb chnget Srvb
kflt chnget Sflt

kvol_sm portk kvol, 0.05
kpan_sm portk kpan, 0.05
krvb_sm portk krvb, 0.05
kflt_sm portk kflt, 0.05

kcutoff = cpsoct((kflt_sm * 10) + 4)
asig pluck iamp, icps, icps, 2, 1
afilt moogladder asig, kcutoff, 0.1
ares linen afilt, .01, p3, .1

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm")

(defun make-sf-template (program-number)
  (format nil "
  i_prog = ~A
  i_chan = p1
  
  S_init sprintf \"sf_init_%d\", p1
  i_init chnget S_init
  if i_init == 0 then
    fluidProgramSelect gieng, i_chan, gisf, 0, i_prog
    chnset 1, S_init
  endif

  imidinn = (octpch(p4) - 3) * 12
  ivel = p5 * 127

  Svol sprintf \"vol_%d\", int(p1)
  Span sprintf \"pan_%d\", int(p1)
  Srvb sprintf \"rvb_%d\", int(p1)
  kvol chnget Svol
  kpan chnget Span
  krvb chnget Srvb
  
  fluidCCk gieng, i_chan, 7, int(kvol * 127)
  fluidCCk gieng, i_chan, 10, int(kpan * 127)
  fluidCCk gieng, i_chan, 91, int(krvb * 127)

  fluidNote gieng, i_chan, imidinn, ivel
  " program-number))

;; Load the Orchestral Templates into Bogu's Memory!
(setf (gethash 'PIANO *synth-templates*) (make-sf-template 0))
(setf (gethash 'GLOCK *synth-templates*) (make-sf-template 9))
(setf (gethash 'STRINGS *synth-templates*) (make-sf-template 48))
(setf (gethash 'TIMPANI *synth-templates*) (make-sf-template 47))
(setf (gethash 'HORNS *synth-templates*) (make-sf-template 60))
(setf (gethash 'FLUTE *synth-templates*) (make-sf-template 73))
(setf (gethash 'CELLO *synth-templates*) (make-sf-template 42))



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
  (setf *master-epoch* nil)
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
  (when *score*
    (format t "Save your current project before loading a new one? (y/n): ")
    (finish-output)
    (let ((ans (read-line)))
      (when (string= (string-downcase ans) "y")
        (save))))

  (let ((target (if filename 
                    (string-downcase (string filename))
                    (progn
                       (format t "Enter project name to load: ")
                      (finish-output)
                      (read-line)))))
    
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
                        (unless (or (string= trimmed-line "")
                                    (char= (char trimmed-line 0) #\;))
                          (push line *bogu-code*)
                          
                          ;; THE FIX: Smart Semicolon Insertion for the File Loader!
                          ;; Only use '&' if we are NOT interacting with brackets.
                          (let* ((trimmed-prev (string-trim " " input-string))
                                 (last-char-prev (if (> (length trimmed-prev) 0) 
                                                     (char trimmed-prev (1- (length trimmed-prev))) 
                                                     #\Space))
                                 (first-char-next (if (> (length trimmed-line) 0) 
                                                      (char trimmed-line 0) 
                                                      #\Space))
                                 (separator (if (or (string= input-string "")
                                                    (char= last-char-prev #\[)
                                                    (char= first-char-next #\[)
                                                    (char= first-char-next #\]))
                                                " "      ; <-- Keep arguments attached!
                                                " & "))) ; <-- Safely separate commands!
                            (setf input-string (concatenate 'string input-string separator trimmed-line)))
                          
                          (when (= (count #\[ input-string) (count #\] input-string))
                            (handler-case
                                (let* ((tokens (lex-bogu-string input-string))
                                       (ast (parse-bogu-tokens tokens)))
                                  (when ast 
                                    (execute-ast ast)))
                              (error (e) 
                                (format t "~%[Compiler Error] Could not parse block in file.~%Details: ~A~%" e)))
                            (setf input-string ""))))))
            (format t "Error: File ~a.bogu not found.~%" target)))
      (format t "loaded \"compositions/~a/~a.bogu\"~%" target target))))

(defun calculate-sync-target (&optional (grid-beats *beats-per-bar*))
  "Determines loop start time. Starts instantly on fresh boot, otherwise syncs perfectly."
  (unless *master-epoch*
    (setf *master-epoch* (get-internal-real-time)))
  (cond
    ((eq *quantize-mode* :free)
     (get-internal-real-time))
    (t
     (let* ((now (get-internal-real-time))
            (elapsed-internal (- now *master-epoch*))
            (elapsed-sec (/ elapsed-internal internal-time-units-per-second))
            (current-bpm (if *bpm* (car *bpm*) 60.0))
            (sec-per-beat (/ 60.0 current-bpm))
            (sec-per-grid (* sec-per-beat grid-beats)))
       
       ;; THE FIX: If the engine just booted, don't wait! Fire instantly.
       (if (< elapsed-sec 0.5)
           now
           (let* ((current-grid (floor (/ elapsed-sec sec-per-grid)))
                  (next-grid-sec (* (1+ current-grid) sec-per-grid))
                  (perfect-target (+ *master-epoch* (round (* next-grid-sec internal-time-units-per-second)))))
             (if (eq *quantize-mode* :exact)
                 perfect-target
                 (let* ((slop-sec (if (numberp *quantize-mode*) *quantize-mode* 0.0))
                        (slop-internal (round (* slop-sec internal-time-units-per-second)))
                        (drift (if (> slop-internal 0)
                                   (- (random (* 2 slop-internal)) slop-internal)
                                   0)))
                   (+ perfect-target drift)))))))))

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
