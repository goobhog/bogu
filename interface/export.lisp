;; interface/export.lisp
(in-package :bogu)

(defun bogu->csd (filename)
  "Prints bogu score data to a static csound .csd file, including the Master Limiter and Reverb Bus."
  (with-open-file (out (comp-path filename (bogu-folder filename) "csd")
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      ;; 1. The Header
      (format out "<CsoundSynthesizer>~%<CsOptions>~%-odac~%</CsOptions>~%<CsInstruments>~%")
      (format out "sr = 44100~%ksmps = 32~%nchnls = 2~%0dbfs = 4~%")
      
      ;; 2. Global Bus Headers (UPDATED TO STEREO)
      (format out "ga_master_L init 0~%ga_master_R init 0~%")
      (format out "ga_rvb_L init 0~%ga_rvb_R init 0~%")
      (format out "gk_reverb init 0~%")
      (format out "giwave ftgen 2, 0, 4096, 10, 1~%") 
      
      ;; SoundFont Engine Support (REQUIRED for PIANO/STRINGS/etc)
      (format out "gieng fluidEngine~%")
      (format out "gisf fluidLoad \"orchestra.sf2\", gieng, 1~%")
      
      ;; 3. The FX Instruments
      (format out "instr 98~%gk_reverb = p4~%endin~%~%")

      ;; INSTR 99: The Master Console (UPDATED TO STEREO)
      (format out "instr 99~%")
      (format out "aSafeL = tanh(ga_rvb_L)~%")  
      (format out "aSafeR = tanh(ga_rvb_R)~%")
      (format out "aWetL, aWetR reverbsc aSafeL, aSafeR, 0.85, 12000~%")
      
      (format out "aWetL butterhp aWetL, 150~%")
      (format out "aWetR butterhp aWetR, 150~%")
      
      (format out "aMixL = ga_master_L + (aWetL * gk_reverb)~%")
      (format out "aMixR = ga_master_R + (aWetR * gk_reverb)~%")
      (format out "aLimitL = 3.9 * tanh(aMixL / 3.9)~%")
      (format out "aLimitR = 3.9 * tanh(aMixR / 3.9)~%")
      (format out "outs aLimitL, aLimitR~%")
      (format out "clear ga_master_L, ga_master_R, ga_rvb_L, ga_rvb_R~%endin~%~%")

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

(defun bogu->ly (filename target-instr)
  "Compiles the timeline into a LilyPond PDF. Handles individual parts or ALL tracks."
  (let* ((ly-path (comp-path filename (bogu-folder filename) "ly"))
         ;; 1. THE CONDUCTOR: Detect all active tracks if 'ALL is passed!
         (all-instrs (if (string-equal (format nil "~A" target-instr) "ALL")
                         (remove-duplicates (mapcar (lambda (x) (getf x :instr)) *score*))
                         (list (if (numberp target-instr) target-instr (parse-integer (string target-instr))))))
         ;; Sort them so Track 1 is always at the top of the page
         (sorted-instrs (sort (copy-list all-instrs) #'<)))

    (with-open-file (out ly-path :direction :output :if-exists :supersede)
      (format out "\\version \"2.24.0\"~%")
      (format out "\\header { title = \"~A\" }~%" filename)
      (format out "\\score {~%")
      
      ;; 2. THE BINDER: Wrap everything in a StaffGroup for the Conductor Bracket
      (format out "  \\new StaffGroup <<~%")

      ;; 3. THE LOOP: Build a separate staff for every active instrument
      (dolist (instr sorted-instrs)
        (let* ((raw-score (remove-if-not (lambda (x) (= (getf x :instr) instr)) *score*))
               (sorted-score (sort (copy-list raw-score) #'< :key (lambda (x) (getf x :time))))
               (current-time 0.0)
               (grouped-score nil)
               (current-group nil)
	       (trk-obj (gethash instr *tracks*))
               (current-clef (if trk-obj (track-clef trk-obj) "treble")))

          ;; Open the Staff and print the Margin Label
          (format out "    \\new Staff {~%")
          (format out "      \\set Staff.instrumentName = \"Track ~A\"~%" instr)
	  (format out "      \\clef \"~A\"~%" current-clef)

          (if (null sorted-score)
              (format t "~%[ENGRAVER Warning] Track ~A is completely empty.~%" instr)
              (progn
                ;; Grouping Engine (Chords vs Notes)
                (dolist (event sorted-score)
                  (if (null current-group)
                      (push event current-group)
                      (if (= (getf event :time) (getf (car current-group) :time))
                          (push event current-group)
                          (progn
                            (push (reverse current-group) grouped-score)
                            (setf current-group (list event))))))
                (when current-group (push (reverse current-group) grouped-score))
                (setf grouped-score (reverse grouped-score))

                ;; Printing Engine
                (dolist (group grouped-score)
                  (let* ((first-event (car group))
                         (event-time (getf first-event :time))
                         (rest-time (- event-time current-time))
                         (meta-events (remove-if-not (lambda (x) (eq (getf x :type) :meta)) group))
                         (note-events (remove-if-not (lambda (x) (eq (getf x :type) :note)) group)))

                    ;; Rests
                    (when (and note-events (>= rest-time 0.125))
                      (format out "r~A " (dur->lily rest-time)))

                    ;; Metadata (Clefs, Cadenzas)
                    (dolist (m meta-events)
                      (cond
                        ((eq (getf m :subtype) :clef) (format out "\\clef \"~A\" " (getf m :val)))
                        ((eq (getf m :subtype) :cadenza-on) (format out "\\cadenzaOn \\omit Stem "))
                        ((eq (getf m :subtype) :cadenza-off) (format out "\\cadenzaOff \\undo \\omit Stem \\bar \"|\" "))
			((eq (getf m :subtype) :line-break) (format out "\\bar \"||\" \\break "))))

                    ;; Auto-Clef & Notes
                    (when note-events
                      (let ((sum-pitch 0.0))
                        (dolist (n note-events) (incf sum-pitch (getf n :pch)))
                        (let ((avg-pitch (/ sum-pitch (length note-events))))
                          (cond
                            ((and (< avg-pitch 7.07) (string= current-clef "treble"))
                             (format out "\\clef bass ")
                             (setf current-clef "bass"))
                            ((and (>= avg-pitch 8.00) (string= current-clef "bass"))
                             (format out "\\clef treble ")
                             (setf current-clef "treble")))))

                      (let ((note-dur (or (getf (car note-events) :written-dur) (getf (car note-events) :dur))))
                        (if (= 1 (length note-events))
                            (format out "~A~A~A " 
                                    (pch->lily (getf (car note-events) :pch)) 
                                    (dur->lily note-dur)
                                    (if (eq (getf (car note-events) :art) :staccato) "-." ""))
                            (progn
                              (format out "<")
                              (dolist (note note-events)
                                (format out "~A " (pch->lily (getf note :pch))))
                              (format out ">~A~A " 
                                      (dur->lily note-dur)
                                      (if (eq (getf (car note-events) :art) :staccato) "-." ""))))
                        (setf current-time (+ event-time note-dur))))))
                ))
          ;; Close individual staff
          (format out "~%    }~%")))

      ;; Close StaffGroup and apply global layout rules
      (format out "  >>~%")
      (format out "  \\layout { \\context { \\Voice \\remove \"Note_heads_engraver\" \\consists \"Completion_heads_engraver\" } }~%}~%"))

    (format t "~%[ENGRAVER] LilyPond source generated at ~A~%" ly-path)

    ;; 4. THE SYSTEM CALL
    (handler-case
        (let ((lily-proc (sb-ext:run-program "/usr/bin/lilypond" 
                                             (list "--png" "--pdf" 
                                                   "--output" (namestring (uiop:pathname-directory-pathname ly-path))
                                                   (namestring ly-path)) 
                                             :search nil :wait t)))
          (if (zerop (sb-ext:process-exit-code lily-proc))
              (format t "[ENGRAVER] Success! PDF generated.~%")
              (format t "[ERROR] LilyPond failed to compile.~%")))
      (error (e) (format t "[ERROR] Could not execute lilypond.~%~A~%" e)))))
