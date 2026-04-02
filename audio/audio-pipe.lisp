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
  "Sets default Vol (80%), Pan (Center), and Reverb (10%) and Filter (100%)."
  (dotimes (i 16)
    (osc-control (1+ i) 1 0.1 0.8 0.8)   ; Vol 
    (osc-control (1+ i) 2 0.1 0.5 0.5)   ; Pan
    (osc-control (1+ i) 3 0.1 0.1 0.1)   ; Reverb
    (osc-control (1+ i) 4 0.1 1.0 1.0))) ; Filter

(defvar *csound-process* nil "Holds the background Csound audio server.")

(defun generate-bogu-server ()
  (with-open-file (out "bogu-server.csd" :direction :output :if-exists :supersede)
    (format out "<CsoundSynthesizer>~%<CsOptions>~%-odac -m0 -d -L /tmp/bogu_pipe~%</CsOptions>~%<CsInstruments>~%")
    (format out "sr = 44100~%ksmps = 128~%nchnls = 2~%0dbfs = 1.0~%")
    (format out "ga_master_L init 0~%ga_master_R init 0~%ga_rvb_L init 0~%ga_rvb_R init 0~%~%")
    
    (format out "gieng fluidEngine~%")
    (format out "gisf fluidLoad \"orchestra.sf2\", gieng, 1~%~%")
    (maphash (lambda (id code) (format out "instr ~A~%~A~%endin~%~%" id code)) *synth-rack*)
    
    (format out "instr 97 ; SOUNDFONT BUS~%")
    (format out "aL, aR fluidOut gieng~%")
    (format out "vincr ga_master_L, aL~%vincr ga_master_R, aR~%endin~%~%")
    
    (format out "instr 98 ; STEREO REVERB~%")
    ;; THE FIX: Protect the reverb feedback loop from exploding!
    (format out "aSafeL = tanh(ga_rvb_L)~%")
    (format out "aSafeR = tanh(ga_rvb_R)~%")
    (format out "aL, aR reverbsc aSafeL, aSafeR, 0.85, 7000~%")
    (format out "vincr ga_master_L, aL~%vincr ga_master_R, aR~%endin~%~%")
    
    (format out "instr 99 ; MASTER BUS~%aOutL = 0.95 * tanh(ga_master_L)~%aOutR = 0.95 * tanh(ga_master_R)~%")
    (format out "outs aOutL, aOutR~%ga_master_L = 0~%ga_master_R = 0~%ga_rvb_L = 0~%ga_rvb_R = 0~%endin~%~%")
    
    (format out "instr 100 ; CONTROL ROUTER~%itrack = p4~%iparam = p5~%istart = p6~%iend = p7~%")
    (format out "kval linseg istart, p3, iend~%")
    (format out "if iparam == 1 then~%chnset kval, sprintf(\"vol_%d\", itrack)~%")
    (format out "elseif iparam == 2 then~%chnset kval, sprintf(\"pan_%d\", itrack)~%")
    (format out "elseif iparam == 3 then~%chnset kval, sprintf(\"rvb_%d\", itrack)~%")
    (format out "elseif iparam == 4 then~%chnset kval, sprintf(\"flt_%d\", itrack)~%endif~%endin~%")
    
    (format out "</CsInstruments>~%<CsScore>~%")
    (format out "f 2 0 4096 10 1~%")
    (format out "i 99 0 86400~%i 98 0 86400~%i 97 0 86400~%")
    (format out "</CsScore>~%</CsoundSynthesizer>~%")))

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
