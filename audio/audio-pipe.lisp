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
  "Armor-Plated: Sends note data to Csound atomically, immune to thread interrupts."
  (when *pipe-stream*
    (sb-sys:without-interrupts
      (sb-thread:with-mutex (*pipe-lock*)
        (sb-sys:allow-with-interrupts
          (format *pipe-stream* "i ~A 0 ~A ~A ~A~%" instr dur pch vel)
          (force-output *pipe-stream*))))))

(defun osc-control (track param dur start end)
  "Sends a continuous parameter envelope to the Csound Control Router atomically."
  (when *pipe-stream*
    (sb-sys:without-interrupts
      (sb-thread:with-mutex (*pipe-lock*)
        (sb-sys:allow-with-interrupts
          (let ((safe-dur (max 0.01 dur)))
            (format *pipe-stream* "i 100 0 ~A ~A ~A ~A ~A~%" safe-dur track param start end)
            (force-output *pipe-stream*)))))))

(defun init-mixer ()
  "Sets default Vol (80%), Pan (Center), and Reverb (10%) and Filter (100%)."
  (dotimes (i 16)
    (osc-control (1+ i) 1 0.1 0.8 0.8)   ; Vol 
    (osc-control (1+ i) 2 0.1 0.5 0.5)   ; Pan
    (osc-control (1+ i) 3 0.1 0.1 0.1)   ; Reverb
    (osc-control (1+ i) 4 0.1 1.0 1.0))) ; Filter

(defvar *csound-process* nil "Holds the background Csound audio server.")

(defun generate-bogu-server ()
  "Compiles the current *synth-rack* into the static Csound scaffold."
  (let* ((scaffold-path (merge-pathnames "assets/bogu-scaffold.csd" *bogu-dir*))
         ;; uiop is safely built into ASDF/Quicklisp
         (scaffold-str (uiop:read-file-string scaffold-path))
         (synth-str (with-output-to-string (s)
                      (maphash (lambda (id code) 
                                 (format s "instr ~A~%~A~%endin~%~%" id code)) 
                               *synth-rack*))))
    
    (with-open-file (out "bogu-server.csd" :direction :output :if-exists :supersede)
      (write-string (cl-ppcre:regex-replace ";;;BOGU_SYNTH_RACK;;;" scaffold-str synth-str) out))))

(defun reboot-audio-server ()
  "Kills the old Csound, generates a new server file, and hot-boots it in the background."
  (format t "~%[SYSTEM] Generating new bogu-server.csd...~%")
  (generate-bogu-server)

  (format t "[SYSTEM] Terminating old Csound processes...~%")
  (ignore-errors (close *pipe-stream*))
  (ignore-errors (sb-ext:run-program "/usr/bin/killall" '("csound") :search t :wait t))
  (ignore-errors (sb-ext:run-program "/usr/bin/mkfifo" '("/tmp/bogu_pipe") :search t :wait t))

  (format t "[SYSTEM] Booting new Csound server in the background...~%")
  (setf *csound-process* 
        (sb-ext:run-program "/usr/bin/csound" '("bogu-server.csd") :search t :wait nil))

  ;; Give Csound exactly half a second to initialize its DACs
  (sleep 0.5)

  ;; THE FIX: Did Csound actually survive the boot?
  (if (sb-ext:process-alive-p *csound-process*)
      (progn
        (boot-osc-bridge)
        (format t "[SYSTEM] Reboot complete. Hardware flash successful.~%"))
      (format t "~%[FATAL ERROR] Csound crashed instantly. Check your opcodes or ensure 'orchestra.sf2' is in your folder!~%")))

(defun reboot ()
  "User-facing command to reboot the Csound server."
  (reboot-audio-server)
  t)
