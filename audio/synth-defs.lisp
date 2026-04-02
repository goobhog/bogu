(in-package :bogu)


;; The Master Library of Synth Cartridges

(defun build-bogu-synth (oscillator-core &key (use-filter t) (attack 0.03) (release 0.05))
  "Automatically wraps any Csound oscillator in Bogu's standard mixer routing."
  (format nil "
icps = cpspch(p4)
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

~A  

~A  

ares linen asig, ~A, p3, ~A

aL, aR pan2 (ares * kvol_sm), kpan_sm
vincr ga_master_L, aL
vincr ga_master_R, aR
vincr ga_rvb_L, aL * krvb_sm
vincr ga_rvb_R, aR * krvb_sm"
    
    oscillator-core 
    (if use-filter 
        ;; THE FIX: An actual multi-line string instead of \n
        "kcutoff = cpsoct((kflt_sm * 10) + 4)
asig moogladder asig_raw, kcutoff, 0.25" 
        "asig = asig_raw")
    attack release))

;; 1. SINE (No filter needed)
(setf (gethash 'SINE *synth-templates*) 
      (build-bogu-synth "asig_raw poscil iamp, icps, 2" :use-filter nil))

;; 2. SAW (Uses Moog filter)
(setf (gethash 'SAW *synth-templates*) 
      (build-bogu-synth "asig_raw vco2 iamp, icps, 0" :use-filter t))

;; 3. PLUCK (Uses Moog filter, needs sharper attack/release)
(setf (gethash 'PLUCK *synth-templates*) 
      (build-bogu-synth "asig_raw pluck iamp, icps, icps, 2, 1" 
                        :use-filter t :attack 0.01 :release 0.1))

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

  ;; FIX 1: Force rounding to prevent float-truncation from flattening notes!
  imidinn = round((octpch(p4) - 3) * 12)
  ivel = p5 * 127

  Svol sprintf \"vol_%d\", int(p1)
  Span sprintf \"pan_%d\", int(p1)
  Srvb sprintf \"rvb_%d\", int(p1)
  kvol chnget Svol
  kpan chnget Span
  krvb chnget Srvb
  
  ;; FIX 2: Only send CCs if they change. Prevents engine choking during polyphony.
  kvol_cc = int(kvol * 127)
  kpan_cc = int(kpan * 127)
  krvb_cc = int(krvb * 127)

  if changed(kvol_cc) == 1 then
    fluidCCk gieng, i_chan, 7, kvol_cc
  endif
  if changed(kpan_cc) == 1 then
    fluidCCk gieng, i_chan, 10, kpan_cc
  endif
  if changed(krvb_cc) == 1 then
    fluidCCk gieng, i_chan, 91, krvb_cc
  endif

  fluidNote gieng, i_chan, imidinn, ivel
  krelease release
  if krelease == 1 then
    fluidNote gieng, i_chan, imidinn, 0
  endif
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
