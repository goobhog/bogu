;; tests.lisp
(in-package :bogu) 

;; --- TEST HARNESS & GLOBALS ---
(defparameter *test-failures* 0)
(defparameter *test-passes* 0)

(defun reset-test-counters ()
  (setf *test-failures* 0)
  (setf *test-passes* 0))

(defun assert-equal (expected actual test-name)
  "The fundamental unit of falsifiability. Now handles floating-point jitter."
  (let ((passed (if (and (numberp expected) (numberp actual))
                    (< (abs (- expected actual)) 0.0001) ; Epsilon check
                    (equal expected actual))))
    (if passed
        (progn
          (incf *test-passes*)
          (format t " [PASS] ~a~%" test-name))
        (progn
          (incf *test-failures*)
          (format t " [FAIL] ~a~%  Expected: ~a~%  Got:      ~a~%" test-name expected actual)))))

(defmacro assert-ast (test-name input-string expected-ast)
  "Parses a raw Bogu string and asserts the resulting AST matches the expected Lisp list perfectly."
  `(let* ((tokens (lex-bogu-string ,input-string))
          (actual-ast (parse-bogu-tokens tokens)))
     (assert-equal ',expected-ast actual-ast ,test-name)))

(defun pluck (property event-list)
  "Helper: Extracts a specific property from a list of note events."
  (mapcar (lambda (e) (getf e property)) event-list))

(defun measure-written-length (event-list)
  "Measures the footprint of a block exactly as the SEQ command sees it."
  (let ((max-t 0.0))
    (dolist (e event-list)
      (setf max-t (max max-t (+ (getf e :time) (or (getf e :written-dur) 0.0)))))
    max-t))

(defun measure-absolute-length (event-list)
  "Measures the actual audio footprint exactly as the COMMITTER sees it."
  (let ((max-t 0.0))
    (dolist (e event-list)
      (setf max-t (max max-t (+ (getf e :time) (or (getf e :dur) (getf e :written-dur) 0.0)))))
    max-t))

(defun measure-ast-duration (ast-string)
  "Helper for string-based spillover tests: compiles and measures absolute maximum time+dur."
  (let* ((tokens (lex-bogu-string ast-string))
         (ast (parse-bogu-tokens tokens))
         (events (execute-ast ast)))
    (measure-written-length events)))

;; --- THE MASTER TEST SUITE ---
(defun run-bogu-tests ()
  "Executes the entire Bogu diagnostic suite sequentially."
  (reset-test-counters)
  (format t "~%========================================~%")
  (format t "      RUNNING BOGU MASTER DIAGNOSTICS    ~%")
  (format t "========================================~%")

  ;; ---------------------------------------------------------
  ;; 1. LEXER & PARSER
  ;; ---------------------------------------------------------
  (format t "~%--- 1. Lexer & Parser ---~%")
  (assert-equal '([ SEQ Q C4 ]) (lex-bogu-string "[ seq q c4 ]") "Lexer separates brackets")
  (assert-ast "Basic Command Parsing" "bpm 120" ((BPM 120)))
  (assert-ast "Nested Combinatoric Blocks" "seq q [ poly q c4 e4 g4 ]" ((SEQ Q (POLY Q C4 E4 G4))))
  (assert-ast "Variable Definition Brackets" "def my-chord [ c4 e4 g4 ]" ((DEF MY-CHORD ((C4) (E4) (G4)))))
  (let* ((raw-str "seq q c4 fluid 4 h sub-pool wait 2.0")
         (tokens (lex-bogu-string raw-str))
         (ast (parse-bogu-tokens tokens)))
    (assert-equal '((SEQ Q C4) (FLUID 4 H SUB-POOL) (WAIT 2.0)) ast "Parser intelligently groups symbols into sequential AST nodes without ampersands"))
  
  (let* ((raw-str "sarp st w my-arp")
         (tokens (lex-bogu-string raw-str))
         (ast (parse-bogu-tokens tokens)))
    (assert-equal '((SARP ST W MY-ARP)) ast "Parser should group variable arguments without shattering the command"))
  
  ;; ---------------------------------------------------------
  ;; 2. LOGIC, VARIABLES, & TURING COMPLETENESS
  ;; ---------------------------------------------------------
  (format t "~%--- 2. Logic & State (IF / DEF) ---~%")
  (reset-bogu)
  (let* ((if-true (execute-ast '((IF 5 > 3 (C4) (D4)))))
         (if-false (execute-ast '((IF 5 < 3 (C4) (D4)))))
         (if-eq (execute-ast '((IF 10 = 10 (E4) (F4))))))
    (assert-equal '(C) (pluck :pitch-symbol if-true) "IF evaluates true branch properly (>)")
    (assert-equal '(D) (pluck :pitch-symbol if-false) "IF bypasses to false branch properly (<)")
    (assert-equal '(E) (pluck :pitch-symbol if-eq) "IF evaluates equality properly (=)"))

  (execute-node '(DEF TEST-VAR (C4 D4)))
  (assert-equal '(C D) (pluck :pitch-symbol (execute-ast '((TEST-VAR)))) "AST recursively expands DEF variables")

  (reset-bogu)
  (execute-node '(DEF MY-SUB-MEL (E4 G4)))
  (let ((expanded (expand-vars '((SEQ Q [ MY-SUB-MEL ])))))
    (assert-equal '((SEQ Q (E4 G4))) expanded "expand-vars: Recursively resolves defined variables inside bracketed sublists"))

  ;; ---------------------------------------------------------
  ;; 3. MUSIC MATH & TRANSLATORS
  ;; ---------------------------------------------------------
  (format t "~%--- 3. Math & Translators ---~%")
  (assert-equal "cis'" (pch->lily 8.01) "LilyPond translates C#4 (8.01) to cis'")
  (assert-equal "b" (pch->lily 7.11) "LilyPond translates B3 (7.11) to b")
  (assert-equal 1.0 (rtm 'q) "RTM translates 'q to 1.0 beats")
  (assert-equal 0.375 (rtm 's.) "RTM translates 's. to 0.375 beats")

  (reset-bogu)
  (let ((trk (get-current-track)))
    ;; Set key to C Major
    (setf (track-key trk) '(C MAJOR))
    (multiple-value-bind (p oct) (calculate-diatonic-pitch 0 2 trk) ; C4 (0) + 2 diatonic steps -> E4 (4)
      (assert-equal 4 p "Diatonic Transposition: C4 + 2 degrees in C Major -> E (pitch class 4)")
      (assert-equal 4 oct "Diatonic Transposition: C4 + 2 degrees in C Major -> Octave 4"))
    
    (multiple-value-bind (p oct) (calculate-diatonic-pitch 0 -1 trk) ; C4 (0) - 1 diatonic step -> B3 (11)
      (assert-equal 11 p "Diatonic Transposition: C4 - 1 degree in C Major -> B (pitch class 11)")
      (assert-equal 3 oct "Diatonic Transposition: C4 - 1 degree in C Major -> Octave 3")))
  (reset-bogu)
  (let ((trk (get-current-track)))
    ;; Set key to C MINOR
    (setf (track-key trk) '(C MINOR))
    (multiple-value-bind (p oct) (calculate-diatonic-pitch 0 2 trk) ; C4 + 2 degrees in C Minor -> Eb4 (3)
      (assert-equal 3 p "Diatonic Transposition: C4 + 2 degrees in C Minor -> Eb (pitch class 3)"))
    
    ;; Set key to OFF (Chromatic Fallback)
    (setf (track-key trk) nil)
    (multiple-value-bind (p oct) (calculate-diatonic-pitch 0 2 trk) ; C4 + 2 semitones -> D4 (2)
      (assert-equal 2 p "Chromatic Transposition: C4 + 2 steps with no key -> D (pitch class 2)")))

  ;; ---------------------------------------------------------
  ;; 4. SEQUENCING & COMBINATORICS
  ;; ---------------------------------------------------------
  (format t "~%--- 4. Sequencing & Combinatorics (SEQ/POLY/SIM/CELL) ---~%")
  (let* ((bare-note (execute-ast '(C4)))
         (seq-block (execute-ast '((SEQ Q C4 E4 G4))))
         (poly-block (execute-ast '((POLY W C4 E4 G4))))
         (sim-block (execute-ast '((SEQ (SIM (SEQ C4 D4) E4) F4))))
         (cell-block (execute-ast '((CELL 1.5 (SEQ C4 D4 E4)))))) 
    
    (assert-equal '(1.0) (pluck :written-dur bare-note) "Bare note defaults to 1.0 duration")
    (assert-equal '(0.0 1.0 2.0) (pluck :time seq-block) "SEQ advances time (0.0, 1.0, 2.0)")
    (assert-equal '(0.0 0.0 0.0) (pluck :time poly-block) "POLY stacks all elements at time 0.0")
    (assert-equal '(0.0 0.0 1.0 2.0) (pluck :time sim-block) "SIM isolates timelines; SEQ correctly advances past the longest internal branch")
    
    (assert-equal 1.5 (measure-written-length cell-block) "CELL strictly enforces mathematical time boundaries")
    (assert-equal 2 (length (remove-if (lambda (e) (eq (getf e :pitch-symbol) 'RST)) cell-block)) "CELL automatically chops notes that bleed past the boundary"))

  (let* ((zip-block (execute-ast '((ZIP (BEATS Q H) (C4 E4))))))
    (assert-equal '(C E) (pluck :pitch-symbol zip-block) "ZIP: Correctly matches pitch sequence")
    (assert-equal '(1.0 2.0) (pluck :written-dur zip-block) "ZIP: Correctly applies rhythms from the rhythm block")
    (assert-equal '(0.0 1.0) (pluck :time zip-block) "ZIP: Correctly schedules absolute timeline offsets"))

  (let* ((zip-chords (execute-ast '((ZIP (BEATS Q Q) ((POLY C4 E4 G4) (POLY D4 F4 A4)))))))
    (assert-equal '(C E G D F A) (pluck :pitch-symbol zip-chords) "ZIP Polyphony: Flattens and preserves polyphonic chord structures")
    (assert-equal '(0.0 0.0 0.0 1.0 1.0 1.0) (pluck :time zip-chords) "ZIP Polyphony: Assigns identical times to fused chord members"))

  (let* ((tread-block (execute-ast '((TREAD 0 (1 2 -1) (C4 D4 E4 G4))))))
    (assert-equal '(C D G E) (pluck :pitch-symbol tread-block) "TREAD: Correctly walks through note pool using intervals")
    (assert-equal '(0.0 1.0 2.0 3.0) (pluck :time tread-block) "TREAD: Advances default 1.0 beat per step"))

  ;; ---------------------------------------------------------
  ;; 5. TREE TRANSFORMERS
  ;; ---------------------------------------------------------
  (format t "~%--- 5. Tree Transformers ---~%")
  (let* ((retro-seq (execute-ast '((RETRO (SEQ C4 D4 E4)))))
         (trans-seq (execute-ast '((TRANSPOSE 7 (SEQ C4 D4)))))
         (rpt-seq (execute-ast '((RPT 3 (SEQ Q C4)))))
         (loop-seq (execute-ast '((LOOP 2 (SEQ Q C4 D4)))))
         (stac-seq (execute-ast '((STACCATO 50 (SEQ Q C4))))))
    
    (assert-equal '(E D C) (pluck :pitch-symbol retro-seq) "RETRO mathematically reverses the note sequence")
    (assert-equal '(7 7) (pluck :transpose trans-seq) "TRANSPOSE tags nodes purely without state mutation")
    (assert-equal 3.0 (measure-written-length rpt-seq) "RPT stamps out exact chronological copies")
    (assert-equal 4 (length loop-seq) "LOOP completely flattens un-shifted data into the AST stream")
    
    (assert-equal '(0.5) (pluck :dur stac-seq) "STACCATO perfectly halves absolute audio duration")
    (assert-equal '(1.0) (pluck :written-dur stac-seq) "STACCATO preserves original sequencer rhythm"))

    (let* ((inv-seq (execute-ast '((INVERT (SEQ C4 E4 G4)))))
         (aug-seq (execute-ast '((AUGMENT 2.0 (SEQ Q C4))))))
    
    (assert-equal '(C AB F) (pluck :pitch-symbol inv-seq) "INVERT symmetrically reflects pitches around the first note (C -> C, E -> Ab, G -> F)")
    (assert-equal '(2.0) (pluck :written-dur aug-seq) "AUGMENT scales rhythmic written duration flawlessly")
    (assert-equal '(2.0) (pluck :dur aug-seq) "AUGMENT scales absolute acoustic duration flawlessly"))

  ;; ---------------------------------------------------------
  ;; 6. PROBABILISTIC & GENERATIVE
  ;; ---------------------------------------------------------
  (format t "~%--- 6. Generative Arrays & Spillover Constraints ---~%")
  (let* ((zero-chance (execute-ast '((CHANCE 0.0 (SEQ C4)))))
         (choose-test (execute-ast '((CHOOSE 1.0 (C4) (D4)))))
         (walk-test (execute-ast '((WALK 5 Q (C4 D4 E4)))))
         (sarp-len (measure-ast-duration "[ sarp 1 15 [ f3 ab3 c4 ] ]"))
         (fluid-len (measure-ast-duration "[ fluid 4 11 [ db1 ab1 ] ]")))
    
    (assert-equal nil zero-chance "CHANCE drops all events at 0.0 probability")
    (assert-equal '(C) (pluck :pitch-symbol choose-test) "CHOOSE securely branches logic based on probability")
    (assert-equal 5.0 (measure-written-length walk-test) "WALK generates exactly the requested step duration")
    (assert-equal 15.0 sarp-len "SARP loops its pool and flawlessly hits its target boundary")
    (assert-equal 11.0 fluid-len "FLUID mathematically clamps random bleeds to hit its exact boundary"))

  ;; ---------------------------------------------------------
  ;; 7. AUTOMATION & HIGHER-ORDER WRAPPERS
  ;; ---------------------------------------------------------
  (format t "~%--- 7. Control Data & Automation ---~%")
  (let* ((vol-cmd (execute-ast '((VOL 80))))
         (sweep-seq (execute-ast '((SEQ C4 (SWEEP VOL 0 100 4) D4))))
         (sweep-wrap-ast (execute-ast '((SWEEP PAN 20 80 (SEQ Q C4 D4)))))
         (ctrl-event (car sweep-wrap-ast)))
    
    (assert-equal 0.8 (getf (car vol-cmd) :start) "Mixer macros yield pure mathematical control data (0.0 to 1.0)")
    (assert-equal '(0.0 1.0 1.0) (pluck :time sweep-seq) "Standard SWEEP consumes 0.0 sequencer time (Ghost Time)")
    
    (assert-equal :control (getf ctrl-event :type) "Higher-Order SWEEP accurately prepends a master control event")
    (assert-equal 2.0 (getf ctrl-event :dur) "SWEEP dynamically calculates matching duration for a 2-beat child sequence")
    (assert-equal 0.2 (getf ctrl-event :start) "SWEEP perfectly parses starting amplitude")
    (assert-equal 0.8 (getf ctrl-event :end) "SWEEP perfectly parses ending amplitude")
    (assert-equal 3 (length sweep-wrap-ast) "SWEEP successfully preserves and evaluates all nested child notes"))

  (let* ((tail-sweep-ast (execute-ast '((SWEEP FLT 0 100 (STACCATO 50 (C4))))))
         (tail-ctrl (car tail-sweep-ast)))
    (assert-equal 1.0 (getf tail-ctrl :dur) "SWEEP mathematically locks automation duration to the grid, ignoring child audio physics"))

  (let* ((massive-block 
           (execute-ast '((SWEEP PAN 0 100 
                           (SWEEP FLT 75 35 
                            (TRANSPOSE -2 
                             (RETRO 
                              (SARP ST W EB2 EB3 BB3))))))))
         (outer-pan-ctrl (nth 0 massive-block))
         (inner-flt-ctrl (nth 1 massive-block))
         (first-note (nth 2 massive-block)))
    
    (assert-equal 2 (getf outer-pan-ctrl :param) "Master Integration: Outer wrapper resolves to PAN")
    (assert-equal 4.0 (getf outer-pan-ctrl :dur) "Master Integration: Outer PAN correctly locks to the 4.0 W grid")
    (assert-equal 4 (getf inner-flt-ctrl :param) "Master Integration: Inner wrapper resolves to FLT")
    (assert-equal 4.0 (getf inner-flt-ctrl :dur) "Master Integration: Inner FLT correctly locks to the 4.0 W grid")
    
    (assert-equal -2 (getf first-note :transpose) "Master Integration: TRANSPOSE correctly passes through the nested envelopes")
    (assert-equal :note (getf first-note :type) "Master Integration: Audio events remain structurally intact deep inside the nest"))
  
  (let* ((map-ast (execute-ast '((MAP-CONTROL PAN 4.0 (C4 E4 G4 C5))))))
    (assert-equal 4 (length map-ast) "MAP-CONTROL generates exactly one control event per child note")
    (assert-equal '(0.0 1.0 2.0 3.0) (pluck :time map-ast) "MAP-CONTROL spreads envelopes evenly across the target duration")
    (assert-equal :control (getf (car map-ast) :type) "MAP-CONTROL strictly outputs pure control data"))
  
  ;; ---------------------------------------------------------
  ;; 8. SYSTEM STATE & LIVE-LOOP REGISTRY
  ;; ---------------------------------------------------------
  (format t "~%--- 8. Live Engine & Threads ---~%")
  (execute-node '(LIVE-LOOP MAIN (SEQ C4)))
  (assert-equal t (not (null (gethash 'MAIN *loop-threads*))) "LIVE-LOOP registers an active thread via the engine")
  (assert-equal '(SEQ C4) (gethash 'MAIN *live-loops*) "LIVE-LOOP saves the AST blueprint for hot-swapping")
  
  (execute-node '(STOP-LOOP MAIN))
  (assert-equal nil (gethash 'MAIN *loop-threads*) "STOP-LOOP clears the thread registry via the engine")
  (assert-equal nil (gethash 'MAIN *live-loops*) "STOP-LOOP clears the active blueprint")

  (reset-bogu)
  (let ((initial-time (track-playhead (get-current-track))))
    (execute-node '(FLT 45))
    (execute-node '(REVERB 60))
    (assert-equal initial-time (track-playhead (get-current-track)) "Mixer commands operate independently of the sequencer timeline"))

  (reset-bogu)
  (let ((initial-time (track-playhead (get-current-track))))
    (execute-node '(LIVE-LOOP LEAK-TEST (SEQ W C4)))
    (sleep 0.1) 
    (execute-node '(STOP-LOOP LEAK-TEST))
    (assert-equal initial-time (track-playhead (get-current-track)) "LIVE-LOOP strictly sandboxes its playhead and does not leak into the global timeline"))

  ;; ---------------------------------------------------------
  ;; 9. AST VALIDATION & TYPE CHECKING
  ;; ---------------------------------------------------------
  (format t "~%--- 9. AST Validation Engine ---~%")
  
  (let ((caught-arity-error nil))
    (handler-case 
        (validate-signature 'CHOOSE '(:number :ast :ast-optional) '(0.5)) ; Missing Block A
      (error (e) (setf caught-arity-error t)))
    (assert-equal t caught-arity-error "Validation engine intercepts and halts on missing required arguments"))

  (let ((caught-type-error nil))
    (handler-case 
        (validate-signature 'FLUID '(:number :rhythm &rest :any) '(C4 Q E4 G4))
      (error (e) (setf caught-type-error t)))
    (assert-equal t caught-type-error "Validation engine enforces strict type checking (e.g. rejecting a note symbol in a :number slot)"))
  
  (let ((caught-rhythm-error nil))
    (handler-case 
        (validate-signature 'CELL '(:rhythm :ast) '(BANANA (C4)))
      (error (e) (setf caught-rhythm-error t)))
    (assert-equal t caught-rhythm-error "Validation engine correctly identifies and rejects invalid Bogu rhythms"))
  
  (let ((validation-passed nil))
    (handler-case 
        (progn
          (validate-signature 'FLUID '(:number :rhythm &rest :any) '(4 Q C4 E4 G4))
          (setf validation-passed t))
      (error (e) 
        (format t "~%[DEBUG] Validation failed with error: ~A~%" e)
        (setf validation-passed nil)))
    (assert-equal t validation-passed "Validation engine allows mathematically correct signatures to pass through to execution"))

  ;; ---------------------------------------------------------
  ;; 10. METADATA & NOTATION Directives
  ;; ---------------------------------------------------------
  (format t "~%--- 10. Notation & Margin Metadata ---~%")
  (reset-bogu)
  (let ((trk (get-current-track)))
    (execute-node '(CLEF BASS))
    (assert-equal "bass" (track-clef trk) "CLEF: Successfully changes the track's default clef to bass")
    
    (let ((break-events (execute-ast '((BREAK)))))
      (assert-equal :meta (getf (car break-events) :type) "BREAK: Generates a system meta event")
      (assert-equal :line-break (getf (car break-events) :subtype) "BREAK: Sets the correct :line-break subtype")))
  
  ;; ---------------------------------------------------------
  ;; 11. BEATS, RESTS, & PURE TIMING DATA
  ;; ---------------------------------------------------------
  (format t "~%--- 11. Timing Data (BEATS, RST, WAIT) ---~%")
  (let* ((rest-ast (execute-ast '((SEQ C4 (WAIT H) E4))))
         (inline-rest-ast (execute-ast '((SEQ C4 RST E4))))
         (beats-ast (execute-ast '((BEATS Q E. S)))))
    
    (assert-equal '(0.0 1.0 3.0) (pluck :time rest-ast) "WAIT safely injects exact duration footprints into sequential chains")
    (assert-equal '(0.0 2.0) (pluck :time (remove-if (lambda (e) (eq (getf e :pitch-symbol) 'RST)) inline-rest-ast)) "RST natively increments the timeline footprint without creating an acoustic note")
    (assert-equal '(1.0 0.75 0.25) (pluck :written-dur beats-ast) "BEATS translates raw rhythm symbols into pure data timing structures"))

  ;; ---------------------------------------------------------
  ;; 12. L-SYSTEM REWRITE RULES
  ;; ---------------------------------------------------------
  (format t "~%--- 12. L-System Rewrite Rules ---~%")
  (reset-bogu)
  
  ;; Define a custom functional rule: [ RULE [ MIRROR ?x ] [ SEQ ?x [ INVERT ?x ] ] ]
  (execute-node '(RULE (MIRROR ?x) (SEQ ?x (INVERT ?x))))
  
  (let* ((rule-res (execute-ast '((MIRROR (SEQ C4 E4)))))
         (multi-var-rule (progn 
                           ;; Define: [ RULE [ WRAP ?x ?y ] [ SEQ ?x ?y ?x ] ]
                           (execute-node '(RULE (WRAP ?x ?y) (SEQ ?x ?y ?x)))
                           (execute-ast '((WRAP C4 G4))))))
    
    (assert-equal '(C E C AB) (pluck :pitch-symbol rule-res) "L-System custom RULE isolates variables, expands, and evaluates recursively")
    (assert-equal '(C G C) (pluck :pitch-symbol multi-var-rule) "L-System securely handles multi-variable binding (?x, ?y)"))
  
  ;; ---------------------------------------------------------
  ;; 13. TRACK STATE, SYNC & TIMELINE OPERATIONS
  ;; ---------------------------------------------------------
  (format t "~%--- 13. Track State & Timeline Operations ---~%")
  (reset-bogu)
  (commit '((I 1) (SEQ Q C4 D4) (I 2) (SEQ Q E4)))
  
  (assert-equal 2.0 (track-playhead (gethash 1 *tracks*)) "Track 1 playhead advances independently")
  (assert-equal 1.0 (track-playhead (gethash 2 *tracks*)) "Track 2 playhead advances independently")

  (execute-node '(SYNC))
  (assert-equal 2.0 (track-playhead (gethash 1 *tracks*)) "SYNC preserves max playhead for leading tracks")
  (assert-equal 2.0 (track-playhead (gethash 2 *tracks*)) "SYNC mathematically advances lagging tracks to the global maximum")

  (execute-node '(SEEK 5.0))
  (assert-equal 5.0 (track-playhead (gethash 2 *tracks*)) "SEEK teleports the active track playhead explicitly")

  (reset-bogu)
  (commit '((SEQ Q C4 D4 E4)))
  (execute-node '(DEL 1))
  (assert-equal 2 (length *score*) "DEL physically removes exactly N events from the global score array")
  (assert-equal 2.0 (track-playhead (get-current-track)) "DEL mathematically rewinds the active track playhead based on remaining events")

  (format t "~%----------------------------------------~%")
  (if (= *test-failures* 0)
      (format t " SUCCESS: All ~A tests passed! Bogu is computationally perfect.~%" *test-passes*)
      (format t " FAILURE: ~A passed, ~A failed. Fix the engine!~%" *test-passes* *test-failures*))
  (format t "========================================~%~%"))
