(in-package :bogu)

(defvar *pipe-stream* nil "The continuous text stream to Csound.")

(defun boot-osc-bridge ()
  "Opens the Linux Pipe and keeps it open so Csound never receives an EOF marker."
  (ignore-errors (close *pipe-stream*)) ; Close it if we are rebooting
  (setf *pipe-stream* (open "/tmp/bogu_pipe" :direction :output :if-exists :append))
  (format t "~%[SYSTEM] Audio Pipe connected. Network bypassed.~%"))

(defun osc-play (instr dur pitch vol)
  "Writes a standard Csound score string directly to the pipe."
  (when *pipe-stream*
    (format *pipe-stream* "i ~A 0 ~F ~F ~F~%" 
            (round instr) (float dur 1.0f0) (float pitch 1.0f0) (float vol 1.0f0))
    (force-output *pipe-stream*)))
