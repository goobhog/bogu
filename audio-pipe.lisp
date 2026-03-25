(in-package :bogu)

(defvar *pipe-stream* nil "The continuous text stream to Csound.")
(defparameter *pipe-lock* (sb-thread:make-mutex :name "pipe-lock"))

(defun boot-osc-bridge ()
  "Opens the Linux Pipe and initializes the Csound Software Mixer."
  (ignore-errors (close *pipe-stream*))
  (setf *pipe-stream* (open "/tmp/bogu_pipe" :direction :output :if-exists :append))
  (format t "~%[SYSTEM] Audio Pipe connected. Network bypassed.~%")
  
  (init-mixer)
  (format t "[MIXER] Software Bus initialized to default levels.~%"))

(defun osc-play (instr dur pch vel)
  "Armor-Plated: Sends note data to Csound using the persistent pipe, guarded by a Mutex."
  (when *pipe-stream*
    (sb-thread:with-mutex (*pipe-lock*)
      (format *pipe-stream* "i ~A 0 ~A ~A ~A~%" instr dur pch vel)
      (force-output *pipe-stream*))))

(defun osc-control (track param dur start end)
  "Sends a continuous parameter envelope to the Csound Control Router."
  (when *pipe-stream*
    (sb-thread:with-mutex (*pipe-lock*)
      ;; THE FIX: Force the router to stay alive for at least 10 milliseconds!
      (let ((safe-dur (max 0.01 dur)))
        (format *pipe-stream* "i 100 0 ~A ~A ~A ~A ~A~%" safe-dur track param start end)
        (force-output *pipe-stream*)))))

(defun init-mixer ()
  "Sets default Vol (80%), Pan (Center), and Reverb (10%) for all 16 tracks."
  (dotimes (i 16)
    (osc-control (1+ i) 1 0.1 0.8 0.8)   ; Vol 
    (osc-control (1+ i) 2 0.1 0.5 0.5)   ; Pan
    (osc-control (1+ i) 3 0.1 0.1 0.1))) ; Reverb
