(in-package :bogu)

(defun flatten (l)
  "Removes parentheses from a multi-dimensional list."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun split-by-amp (lst)
  "Slices a flat list into sub-lists wherever an & symbol appears."
  (let ((chunks nil) (current nil))
    (dolist (item lst)
      (if (eq item '&)
          (progn (when current (push (reverse current) chunks))
                 (setf current nil))
          (push item current)))
    (when current (push (reverse current) chunks))
    (reverse chunks)))

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

(defun bogu->ly (filename target-instr)
  "Compiles the timeline into a LilyPond PDF. Handles individual parts or ALL tracks."
  (let* ((ly-path (comp-path filename (bogu-folder filename) "ly"))
         ;; 1. THE CONDUCTOR: Detect all active tracks if 'ALL is passed!
         (all-instrs (if (string-equal (string target-instr) "ALL")
                         (remove-duplicates (mapcar (lambda (x) (getf x :instr)) *score*))
                         (list (if (numberp target-instr) target-instr (parse-integer (string target-instr))))))
         ;; Sort them so Track 1 is always at the top of the page
         (sorted-instrs (sort (copy-list all-instrs) #'<)))

    (with-open-file (out ly-path :direction :output :if-exists :supersede)
      (format out "\\version \"2.24.0\"~%")
      (format out "\\header { title = \"Bogu Score: ~A\" }~%" filename)
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
               (current-clef "treble"))

          ;; Open the Staff and print the Margin Label
          (format out "    \\new Staff {~%")
          (format out "      \\set Staff.instrumentName = \"Track ~A\"~%" instr)

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
                        ((eq (getf m :subtype) :cadenza-off) (format out "\\cadenzaOff \\undo \\omit Stem \\bar \"|\" "))))

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
