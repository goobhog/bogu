;; tests.lisp
(in-package :cl-user)

(defun assert-equal (expected actual test-name)
  "The fundamental unit of falsifiability."
  (if (equal expected actual)
      (format t "[PASS] ~a~%" test-name)
      (format t "[FAIL] ~a~%  Expected: ~a~%  Got:      ~a~%" test-name expected actual)))

(defun run-bogu-tests ()
  "The Automated Test Suite."
  (format t "~%--- RUNNING BOGU DIAGNOSTICS ---~%")
  
  ;; 1. The Lexer Bracket Test
  (assert-equal '([ SEQ Q C4 ]) 
                (lex-bogu-string "[ seq q c4 ]") 
                "Lexer correctly separates brackets into standalone tokens")
                
  ;; 2. The Transposition Octave Wrap Test
  (reset-bogu) ;; <--- WIPE MEMORY FIRST!
  (let ((*transpose-offset* 14))
    ;; Play a C4 (Pitch 0, Octave 4) transposed up 14 semitones. 
    ;; It MUST wrap around to a D5 (Pitch 2, Octave 5).
    (schedule-note 0 4 'q) 
    (let ((generated-note (car *score*)))
      (assert-equal 2 (getf generated-note :pitch) "Transposition calculates correct pitch class")
      (assert-equal 5 (getf generated-note :octave) "Transposition correctly wraps into the next octave")))
  
  ;; 3. The Negative Time Armor Test
  (reset-bogu)
  ;; Try to play a sustained arpeggio where the notes take longer than the total sustain time.
  ;; The duration MUST be clamped to 0.01 to prevent a Csound memory corruption crash.
  (sarp 'q 'e (lambda (r) (schedule-note 0 4 r)) (lambda (r) (schedule-note 2 4 r)))
  (let ((last-note (car *score*)))
    (assert-equal 0.01 (getf last-note :dur) "Sarp safely clamps negative durations to 0.01s"))
      
  (format t "--------------------------------~%")
  (reset-bogu))
