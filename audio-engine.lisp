(in-package :bogu)

;; --- AUDIO ENGINE & TIMING SCHEDULER ---

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
       
       ;; The Fix: Return the shared epoch so all loops start at the exact same microsecond!
       (if (< elapsed-sec 0.5)
           *master-epoch*
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
