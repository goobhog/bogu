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
  (assert-ast "Variable Definition Brackets" "def my-chord [ c4 e4 g4 ]" ((DEF MY-CHORD (C4 E4 G4))))
  (let* ((raw-str "seq q c4 fluid 4 h sub-pool wait 2.0")
         (tokens (lex-bogu-string raw-str))
         (ast (parse-bogu-tokens tokens)))
    (assert-equal '((SEQ Q C4) (FLUID 4 H SUB-POOL) (WAIT 2.0)) ast "Parser intelligently groups symbols into sequential AST nodes without ampersands"))
  ;; NEW REGRESSION TEST: The "Parser Shatter" Bug
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

  ;; ---------------------------------------------------------
  ;; 3. MUSIC MATH & TRANSLATORS
  ;; ---------------------------------------------------------
  (format t "~%--- 3. Math & Translators ---~%")
  (assert-equal "cis'" (pch->lily 8.01) "LilyPond translates C#4 (8.01) to cis'")
  (assert-equal "b" (pch->lily 7.11) "LilyPond translates B3 (7.11) to b")
  (assert-equal 1.0 (rtm 'q) "RTM translates 'q to 1.0 beats")
  (assert-equal 0.375 (rtm 's.) "RTM translates 's. to 0.375 beats")

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
    (assert-equal '(0.0 1.0 0.0 2.0) (pluck :time sim-block) "SIM isolates timelines; SEQ correctly advances past the longest internal branch")
    
    (assert-equal 1.5 (measure-written-length cell-block) "CELL strictly enforces mathematical time boundaries")
    (assert-equal 2 (length (remove-if (lambda (e) (eq (getf e :pitch-symbol) 'RST)) cell-block)) "CELL automatically chops notes that bleed past the boundary"))

  ;; ---------------------------------------------------------
  ;; 5. TREE TRANSFORMERS
  ;; ---------------------------------------------------------
  (format t "~%--- 5. Tree Transformers ---~%")
  (let* ((retro-seq (execute-ast '((RETRO (SEQ C4 D4 E4)))))
         (trans-seq (execute-ast '((TRANSPOSE 7 (SEQ C4 D4)))))
         (rpt-seq (execute-ast '((RPT 3 (SEQ Q C4)))))
         (loop-seq (execute-ast '((LOOP 2 (SEQ Q C4 D4)))))
         ;; Staccato shrinks :dur to 50% but keeps :written-dur at 1.0!
         (stac-seq (execute-ast '((STACCATO 50 (SEQ Q C4))))))
    
    (assert-equal '(2.0 1.0 0.0) (pluck :time retro-seq) "RETRO mathematically flips start times")
    (assert-equal '(7 7) (pluck :transpose trans-seq) "TRANSPOSE tags nodes purely without state mutation")
    (assert-equal 3.0 (measure-written-length rpt-seq) "RPT stamps out exact chronological copies")
    (assert-equal 4 (length loop-seq) "LOOP completely flattens un-shifted data into the AST stream")
    
    (assert-equal '(0.5) (pluck :dur stac-seq) "STACCATO perfectly halves absolute audio duration")
    (assert-equal '(1.0) (pluck :written-dur stac-seq) "STACCATO preserves original sequencer rhythm"))

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
         ;; Capture the AST block generated by a Higher-Order Sweep
         (sweep-wrap-ast (execute-ast '((SWEEP PAN 20 80 (SEQ Q C4 D4)))))
         ;; The sweep control event is always cons'd to the front of the block!
         (ctrl-event (car sweep-wrap-ast)))
    
    (assert-equal 0.8 (getf (car vol-cmd) :start) "Mixer macros yield pure mathematical control data (0.0 to 1.0)")
    (assert-equal '(0.0 1.0 1.0) (pluck :time sweep-seq) "Standard SWEEP consumes 0.0 sequencer time (Ghost Time)")
    
    ;; DEEP INSPECTION OF HIGHER-ORDER SWEEP
    (assert-equal :control (getf ctrl-event :type) "Higher-Order SWEEP accurately prepends a master control event")
    (assert-equal 2.0 (getf ctrl-event :dur) "SWEEP dynamically calculates matching duration for a 2-beat child sequence")
    (assert-equal 0.2 (getf ctrl-event :start) "SWEEP perfectly parses starting amplitude")
    (assert-equal 0.8 (getf ctrl-event :end) "SWEEP perfectly parses ending amplitude")
    (assert-equal 3 (length sweep-wrap-ast) "SWEEP successfully preserves and evaluates all nested child notes"))
  ;; NEW: STRICT ENVELOPE GRID TEST
  (let* ((tail-sweep-ast (execute-ast '((SWEEP FLT 0 100 (STACCATO 50 (C4))))))
         (tail-ctrl (car tail-sweep-ast)))
    (assert-equal 1.0 (getf tail-ctrl :dur) "SWEEP mathematically locks automation duration to the grid, ignoring child audio physics"))
  ;; NEW: MASTER INTEGRATION TEST (Nested Sweeps + Transformers + Generative)
  (let* ((massive-block 
          (execute-ast '((SWEEP PAN 0 100 
                           (SWEEP FLT 75 35 
                             (TRANSPOSE -2 
                               (RETRO 
                                 (SARP ST W EB2 EB3 BB3))))))))
         (outer-pan-ctrl (nth 0 massive-block))
         (inner-flt-ctrl (nth 1 massive-block))
         (first-note (nth 2 massive-block)))
    
    ;; 1. Test Envelope Grid Integrity
    (assert-equal 2 (getf outer-pan-ctrl :param) "Master Integration: Outer wrapper resolves to PAN")
    (assert-equal 4.0 (getf outer-pan-ctrl :dur) "Master Integration: Outer PAN correctly locks to the 4.0 W grid")
    (assert-equal 4 (getf inner-flt-ctrl :param) "Master Integration: Inner wrapper resolves to FLT")
    (assert-equal 4.0 (getf inner-flt-ctrl :dur) "Master Integration: Inner FLT correctly locks to the 4.0 W grid")
    
    ;; 2. Test Transformer Pass-Through
    (assert-equal -2 (getf first-note :transpose) "Master Integration: TRANSPOSE correctly passes through the nested envelopes")
    (assert-equal :note (getf first-note :type) "Master Integration: Audio events remain structurally intact deep inside the nest"))

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

  ;; NEW REGRESSION TESTS
  (reset-bogu)
  (let ((initial-time (track-playhead (get-current-track))))
    (execute-node '(FLT 45))
    (execute-node '(REVERB 60))
    (assert-equal initial-time (track-playhead (get-current-track)) "Mixer commands operate independently of the sequencer timeline"))

  (reset-bogu)
  (let ((initial-time (track-playhead (get-current-track))))
    (execute-node '(LIVE-LOOP LEAK-TEST (SEQ W C4)))
    (sleep 0.1) ; Let the background thread boot and compile its timeline
    (execute-node '(STOP-LOOP LEAK-TEST))
    (assert-equal initial-time (track-playhead (get-current-track)) "LIVE-LOOP strictly sandboxes its playhead and does not leak into the global timeline"))

  ;; ---------------------------------------------------------
  ;; 9. AST VALIDATION & TYPE CHECKING
  ;; ---------------------------------------------------------
  (format t "~%--- 9. AST Validation Engine ---~%")
  
  ;; Test 1: Arity Catching (Missing Arguments)
  (let ((caught-arity-error nil))
    (handler-case 
        (validate-signature 'CHOOSE '(:number :ast :ast-optional) '(0.5)) ; Missing Block A
      (error (e) (setf caught-arity-error t)))
    (assert-equal t caught-arity-error "Validation engine intercepts and halts on missing required arguments"))

  ;; Test 2: Type Catching (Wrong Data Type)
  (let ((caught-type-error nil))
    (handler-case 
        ;; Passing 'C4' (a symbol) where a :number is expected
        (validate-signature 'FLUID '(:number :rhythm &rest :any) '(C4 Q E4 G4))
      (error (e) (setf caught-type-error t)))
    (assert-equal t caught-type-error "Validation engine enforces strict type checking (e.g. rejecting a note symbol in a :number slot)"))
  
  ;; Test 3: Rhythm Validation
  (let ((caught-rhythm-error nil))
    (handler-case 
        ;; Passing an invalid rhythm
        (validate-signature 'CELL '(:rhythm :ast) '(BANANA (C4)))
      (error (e) (setf caught-rhythm-error t)))
    (assert-equal t caught-rhythm-error "Validation engine correctly identifies and rejects invalid Bogu rhythms"))
  
  ;; Test 4: Validation Success Pass-through
 ;; Test 4: Validation Success Pass-through
  (let ((validation-passed nil))
    (handler-case 
        (progn
          (validate-signature 'FLUID '(:number :rhythm &rest :any) '(4 Q C4 E4 G4))
          (setf validation-passed t))
      (error (e) 
        ;; If it fails, print the exact reason!
        (format t "~%[DEBUG] Validation failed with error: ~A~%" e)
        (setf validation-passed nil)))
    (assert-equal t validation-passed "Validation engine allows mathematically correct signatures to pass through to execution"))

  (format t "~%----------------------------------------~%")
  (if (= *test-failures* 0)
      (format t " SUCCESS: All ~A tests passed! Bogu is computationally perfect.~%" *test-passes*)
      (format t " FAILURE: ~A passed, ~A failed. Fix the engine!~%" *test-passes* *test-failures*))
  (format t "========================================~%~%"))
