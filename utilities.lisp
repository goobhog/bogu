(in-package :bogu)

(defun flatten (l)
  "Removes parentheses from a multi-dimensional list."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun stringem (&rest items)
  "Adjoins items as one lowercase string."
  (string-downcase (format nil "~{~a~^~}" items)))

(defun bogu-folder (name)
  "Checks for a specified directory in compositions/ and creates one if it doesn't exist."
  (ensure-directories-exist (stringem 'compositions/ name #\/)))

(defun comp-path (filename directory type)
  "Creates a pathname with specified name of specified type in specified directory."
  (make-pathname :name filename
                 :type type
                 :defaults (parse-namestring directory)))

(defun note-p (sym)
  "Checks if a symbol follows the bogu note pattern (e.g., c4, f#3, bb2)."
  (let ((str (string-downcase (symbol-name sym))))
    (cl-ppcre:scan "^[a-g][#b]?[0-8]$" str)))

(defun expand-vars (args)
  "Recursively expands variables, checking both user and stdlib memory banks."
  (loop for arg in args
        for user-lookup = (and (symbolp arg) (gethash arg *vars*))
        for std-lookup = (and (symbolp arg) (gethash arg *stdlib-vars*))
        for var-lookup = (or user-lookup std-lookup)
        
        if var-lookup
          append (expand-vars var-lookup)
        else
          collect arg))

(defun bogu->csd (filename)
  "Prints bogu score data to a static csound .csd file, including the Master Limiter and Reverb Bus."
  (with-open-file (out (comp-path filename (bogu-folder filename) "csd")
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      ;; 1. The Header
      (format out "<CsoundSynthesizer>~%<CsOptions>~%-odac~%</CsOptions>~%<CsInstruments>~%")
      (format out "sr = 44100~%ksmps = 32~%nchnls = 2~%0dbfs = 4~%")
      
      ;; 2. Global Bus Headers
      (format out "ga_master init 0~%ga_rvb init 0~%gk_reverb init 0~%")
      (format out "giwave ftgen 2, 0, 4096, 10, 1~%") 
      
      ;; 3. The FX Instruments (Identical to Live Engine)
      (format out "instr 98~%gk_reverb = p4~%endin~%~%")
      
      ;; INSTR 99: The Master Console
      (format out "instr 99~%")
      (format out "aWetL, aWetR reverbsc ga_rvb, ga_rvb, 0.85, 12000~%")
      
      (format out "aWetL butterhp aWetL, 150~%")
      (format out "aWetR butterhp aWetR, 150~%")
      
      (format out "aMixL = ga_master + (aWetL * gk_reverb)~%")
      (format out "aMixR = ga_master + (aWetR * gk_reverb)~%")
      (format out "aLimitL = 3.9 * tanh(aMixL / 3.9)~%")
      (format out "aLimitR = 3.9 * tanh(aMixR / 3.9)~%")
      (format out "outs aLimitL, aLimitR~%")
      (format out "clear ga_master, ga_rvb~%endin~%~%")

      ;; 4. The Synths
      (maphash (lambda (id code) (format out "instr ~a~%~a~%endin~%~%" id code)) *synth-rack*)

      (format out "</CsInstruments>~%<CsScore>~%")

      ;; 5. Tempo & Turn on the Master Reverb Console
      (if *bpm* (format out "t 0 ~a~%" (car *bpm*)) (format out "t 0 60~%"))
      (format out "i 99 0 36000~%")

      ;; 6. The Exporter Bridge
      (let ((sorted-score (sort (copy-list *score*) #'< :key (lambda (x) (getf x :time)))))
        (dolist (event sorted-score)
          (format out "i ~a ~,3f ~,3f ~,3f ~,2f~%" 
                  (getf event :instr) 
                  (getf event :time) 
                  (getf event :dur) 
                  (getf event :pch) 
                  (getf event :vel))))

      ;; 7. The Terminator
      (format out "e~%</CsScore>~%</CsoundSynthesizer>~%"))))

;; --- LILYPOND TRANSLATION LAYER ---

(defun pch->lily (pch)
  "Translates Csound pitch decimals (8.00) to LilyPond strings (c')."
  (let* ((octave (floor pch))
         (pc (round (* (- pch octave) 100)))
         ;; LilyPond uses Dutch note names (cis = C#, bes = Bb)
         (notes #("c" "cis" "d" "dis" "e" "f" "fis" "g" "gis" "a" "ais" "b"))
         (note-name (aref notes (mod pc 12)))
         ;; LilyPond Octaves: Middle C (8.00) is c'
         (octave-marks (cond ((>= octave 9) "''")
                             ((= octave 8) "'")
                             ((= octave 7) "")   ;; C3
                             ((= octave 6) ",")  ;; C2
                             ((<= octave 5) ",,") ;; C1
                             (t "'"))))
    (format nil "~A~A" note-name octave-marks)))

(defun dur->lily (dur)
  "Translates absolute beat durations into LilyPond rhythm strings."
  ;; Basic Quantizer: Snaps physics durations to nearest musical fraction
  (cond ((>= dur 4.0) "1")
        ((>= dur 3.0) "2.")
        ((>= dur 2.0) "2")
        ((>= dur 1.5) "4.")
        ((>= dur 1.0) "4")
        ((>= dur 0.75) "8.")
        ((>= dur 0.5) "8")
        ((>= dur 0.25) "16")
        (t "32")))

;; --- THE MASTER COMPILER ---

(defun bogu->ly (filename target-instr)
  "Compiles the timeline of a specific instrument into a LilyPond PDF and PNG."
  (let* ((ly-path (comp-path filename (bogu-folder filename) "ly"))
         (raw-score (remove-if-not (lambda (x) (= (getf x :instr) target-instr)) *score*))
         ;; Sort chronologically so we can calculate rests correctly
         (sorted-score (sort (copy-list raw-score) #'< :key (lambda (x) (getf x :time))))
         (current-time 0.0))
         
    (unless sorted-score
      (format t "~%[ENGRAVER Error] No notes found for Instrument ~A.~%" target-instr)
      (return-from bogu->ly nil))

    ;; 1. Generate the LilyPond Source Code
    (with-open-file (out ly-path :direction :output :if-exists :supersede)
      (format out "\\version \"2.24.0\"~%")
      (format out "\\header { title = \"Bogu Score: ~A\" composer = \"Live Coded in Lisp\" }~%" filename)
      (format out "\\score {~%  \\new Staff {~%    \\clef treble~%    \\time 4/4~%    ")
      
      (dolist (event sorted-score)
        (let* ((note-time (getf event :time))
               (note-dur (getf event :dur))
               (note-pch (getf event :pch))
               (rest-time (- note-time current-time)))
          
          ;; Insert a rest if there is a gap (quantized to 32nd note threshold)
          (when (>= rest-time 0.125)
            (format out "r~A " (dur->lily rest-time)))
            
          ;; Translate and write the note
          (format out "~A~A " (pch->lily note-pch) (dur->lily note-dur))
          
          ;; Advance the internal playhead
          (setf current-time (+ note-time note-dur))))
          
      (format out "~%  }~%  \\layout {}~%}~%"))
      
    (format t "~%[ENGRAVER] LilyPond source generated at ~A~%" ly-path)
    
    ;; 2. The System Call: Explicitly using /usr/bin/lilypond
    (handler-case
        (let ((lily-proc (sb-ext:run-program "/usr/bin/lilypond" 
                                            (list "--png" "--pdf" 
                                                  "--output" (namestring (uiop:pathname-directory-pathname ly-path))
                                                  (namestring ly-path)) 
                                            :search nil 
                                            :wait t)))
          (if (zerop (sb-ext:process-exit-code lily-proc))
              (format t "[ENGRAVER] Success! PDF and PNG generated in the project folder.~%")
              (format t "[ERROR] LilyPond found the file but failed to compile. Check your syntax.~%")))
      (error (e)
        (format t "[ERROR] Could not execute /usr/bin/lilypond.~%Details: ~A~%" e)))))
