;; i want code to look something like this

;; (composition-start)
;; Welcome to bogu5's composition REPL.
;; Type 'help' to see a comprehensive
;; list of commands.

;; seq(.16 rest c3 d3 e3) seq(.16 f3 d3 e3 c3) g3(8)
;; play
;; playing composition...

(defparameter *allowed-commands* '(seq   play  rpt   rst  save bpm help
				   i     reset chord sarp del  %   defchord
				   chords
				   a0  a1  a2  a3  a4  a5  a6  a7  a8
				   a#0 a#1 a#2 a#3 a#4 a#5 a#6 a#7 a#8
				   bb0 bb1 bb2 bb3 bb4 bb5 bb6 bb7 bb8
				   b0  b1  b2  b3  b4  b5  b6  b7  b8
				   b#0 b#1 b#2 b#3 b#4 b#5 b#6 b#7 b#8
				   cb0 cb1 cb2 cb3 cb4 cb5 cb6 cb7 cb8
				   c0  c1  c2  c3  c4  c5  c6  c7  c8
				   c#0 c#1 c#2 c#3 c#4 c#5 c#6 c#7
				   db0 db1 db2 db3 db4 db5 db6 db7
				   d0  d1  d2  d3  d4  d5  d6  d7
				   d#0 d#1 d#2 d#3 d#4 d#5 d#6 d#7
				   eb0 eb1 eb2 eb3 eb4 eb5 eb6 eb7
				   e0  e1  e2  e3  e4  e5  e6  e7
				   e#0 e#1 e#2 e#3 e#4 e#5 e#6 e#7
				   fb0 fb1 fb2 fb3 fb4 fb5 fb6 fb7
				   f0  f1  f2  f3  f4  f5  f6  f7
				   f#0 f#1 f#2 f#3 f#4 f#5 f#6 f#7
				   gb0 gb1 gb2 gb3 gb4 gb5 gb6 gb7
				   g0  g1  g2  g3  g4  g5  g6  g7
				   g#0 g#1 g#2 g#3 g#4 g#5 g#6 g#7
				   ab0 ab1 ab2 ab3 ab4 ab5 ab6 ab7))

(defun composition-eval (sexp)
  "Tests commands against allowed commands list and evaluates them."
  (if (member (car sexp) *allowed-commands*)
      (progn
	(eval sexp)
	t)
      '()))

(defun composition-read ()
  "Formats bogu commands for lisp reader."
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (if (or (and (eql (car cmd) 'chord) (member (cdr cmd) *allowed-commands*))
	    (eql (car cmd) 'seq)) ; seq and chord take a number and the rest note functions 
	(append (list (car cmd) (quote-it (cadr cmd))) (mapcar #'fn-it (cddr cmd)))
	; e.g. seq 1 c3 d3 e3 f3 becomes (seq 1 #'c3 #'d3 #'e3 #'f3)
	(if (eql (car cmd) 'sarp) ; sarp takes 2 numbers and the rest note functions
	    (append (list (elt cmd 0) (quote-it (elt cmd 1)) (quote-it (elt cmd 2)))
		    (mapcar #'fn-it (cdddr cmd)))
	    (cons (car cmd) (mapcar #'quote-it (cdr cmd)))))))
	    ; other commands' parameters quoted e.g. rst .25 becomes (rst '0.25)		

(defun composition-repl ()
  "REPL interface for bogu. Accepts only allowed commands."
  (let ((cmd (composition-read)))
    (unless (eq (car cmd) 'quit)
      (if (eq (car cmd) 'reset)
	  (progn
	    (bogu-reset)
	    (composition-repl))
	  (if (composition-eval cmd)
	      (composition-repl)
	      (progn
		(format t "unkown symbol~%")
		(composition-repl)))))))

(defun bogu ()
   (format t "Welcome to bogu
Type 'help' for a comprehensive list of commands.~%~%")
   (bogu-reset)
   (composition-repl))
