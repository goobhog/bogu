;(in-package :bogu)

(defparameter *itime* 0)
(defparameter *bpm* '(60 0 "t"))
(defparameter *instruments* '((1 . 0)))
(defparameter *current-instrument* 1)
(defparameter *score* '())
(defparameter *last-sequence* '())
(defparameter *chords* '())

(defun bogu-reset ()
  "Resets all global variables to their default values."
  (setf *itime* 0)
  (bpm 60)
  (setf *instruments* '((1 . 0)))
  (setf *current-instrument* 1)
  (setf *score* '())
  (setf *chords* '()))

(defun i (n)
  "Changes the instrument receiving input."
  (if (not (assoc n *instruments*))
	(push `(,n . 0) *instruments*)
	nil)
  (setf (cdr (assoc *current-instrument* *instruments*)) *itime*)
  (setf *itime* (cdr (assoc n *instruments*)))
  (setf *current-instrument* n))

(defun sarp (rval sval &rest notes)
  "Pushes a sustained arpeggio to the score list. Parameters specify the arpeggio's rhythm and length of sustain."
  (dotimes (i (length notes))
    (incf *current-instrument* 0.01)
    (funcall (elt notes i) (- (rtm sval) (* (rtm rval) i)))
    (decf *itime* (- (rtm sval) (* (rtm rval) (1+ i)))))
  (setf *current-instrument* (floor *current-instrument*))
  (incf *itime* (+ (rtm sval) (* (rtm rval) (length notes)))))

(defun defchord (name &rest notes)
  "Pushes a user defined chord to chords list."
  (push (cons name notes) *chords*))

(defun chords ()
  "Displays current ledger of chord definitions."
  (if *chords*
      (format t "~%~{~a ~%~}~%" *chords*)
      (format t "~%nothing here yet...~%~%")))
  
(defun chord (rval &rest notes)
  "Pushes a chord to the score list."
  (let ((r (rtm rval)))
    (if (assoc (car notes) *chords*)
	(eval (append `(chord ,r) (mapcar #'fn-it (cdr (assoc (car notes) *chords*)))))
	(progn
	  (dolist (i notes)
	    (incf *current-instrument* 0.01)
	    (funcall i r)
	  (decf *itime* r))
	(setf *current-instrument* (floor *current-instrument*))
	(incf *itime* r)))))

(defun del (n)
  "Deletes n notes beginning with the last note entered."
  (dotimes (i (* 5 n))
    (pop *score*))
  (setf *itime* (+ (elt *score* 1) (elt *score* 2))))

(defun seq (rval &rest notes)
  "Pushes a sequence of notes or rests with the same rhythmic value to score list and replaces last sequence list with a list of a call to seq."
  (setf *last-sequence* '())
  (dolist (i notes)
    (funcall i rval))
  (setf *last-sequence* (flatten `(seq ,rval ,notes))))

(defun % ()
  "Evaluates last sequence list."
  (eval (cons (car *last-sequence*)
	 (cons (quote-it (cadr *last-sequence*)) (cddr *last-sequence*)))))

(defun rtm (rval)
  "Returns rhythm quantity for corresponding rhythm symbol. If given a number, it simply returns that number."
  (cond ((numberp rval) rval)
	((eq 'q rval) 1.0)
	((eq 'e rval) 0.5)
	((eq 's rval) 0.25)
	((eq 'h rval) 2.0)
	((eq 'w rval) 4.0)
	((eq 't rval) 0.125)
	((eq 'q. rval) 1.5)
	((eq 'q.. rval) 1.75)
	((eq 'e. rval) 0.75)
	((eq 'e.. rval) 0.875)
	((eq 's. rval) 0.375)
	((eq 's.. rval) 0.4375)
	((eq 'h. rval) 3.0)
	((eq 'h.. rval) 3.5)
	((eq 'qt rval) (/ 2.0 3.0))
	((eq 'et rval) (/ 1.0 3.0))
	((eq 'st rval) (/ 0.5 3.0))
	((eq 'tt rval) (/ 0.25 3.0))
	((eq 'ht rval) (/ 4.0 3.0))))

(defun rst (rval)
  "Increases the itime of next note."
  (incf *itime* (rtm rval)))

(defun bpm (n)
  "Sets beats per minute."
  (setf *bpm* '())
  (push "t" *bpm*)
  (push 0 *bpm*)
  (push n *bpm*))

(defun note (i rval nval oval)
  "Pushes note data to score list."
  (push "i" *score*)
  (push i *score*)
  (push *itime* *score*)
  (push (rtm rval) *score*)
  (push (note->pch nval oval) *score*)
  (incf *itime* (rtm rval)))

(defun save (filename)
  "Saves bogu score data as csound .csd file."
  (format t "saved \"~a\"~%" filename)
  (bogu->csd filename))

(defun play (filename)
  "Plays csound .csd file."
  (format t "playing \"~a\"...~%" filename)
  (sb-ext:run-program "/usr/local/bin/csound" (list filename)))
            

;; note functions ----------------------------

;; a -------------

(defun a0 (rval)
  (note *current-instrument* rval 'a 4))

(defun a1 (rval)
  (note *current-instrument* rval 'a 5))

(defun a2 (rval)
  (note *current-instrument* rval 'a 6))

(defun a3 (rval)
  (note *current-instrument* rval 'a 7))

(defun a4 (rval)
  (note *current-instrument* rval 'a 8))

(defun a5 (rval)
  (note *current-instrument* rval 'a 9))

(defun a6 (rval)
  (note *current-instrument* rval 'a 10))

(defun a7 (rval)
  (note *current-instrument* rval 'a 11))

(defun a8 (rval)
  (note *current-instrument* rval 'a 12))

;; asharp/bflat -----------------------

(defun a#0 (rval)
  (note *current-instrument* rval 'asharp/bflat 4))

(defun a#1 (rval)
  (note *current-instrument* rval 'asharp/bflat 5))

(defun a#2 (rval)
  (note *current-instrument* rval 'asharp/bflat 6))

(defun a#3 (rval)
  (note *current-instrument* rval 'asharp/bflat 7))

(defun a#4 (rval)
  (note *current-instrument* rval 'asharp/bflat 8))

(defun a#5 (rval)
  (note *current-instrument* rval 'asharp/bflat 9))

(defun a#6 (rval)
  (note *current-instrument* rval 'asharp/bflat 10))

(defun a#7 (rval)
  (note *current-instrument* rval 'asharp/bflat 11))

(defun a#8 (rval)
  (note *current-instrument* rval 'asharp/bflat 12))

(defun bb0 (rval)
  (note *current-instrument* rval 'asharp/bflat 4))

(defun bb1 (rval)
  (note *current-instrument* rval 'asharp/bflat 5))

(defun bb2 (rval)
  (note *current-instrument* rval 'asharp/bflat 6))

(defun bb3 (rval)
  (note *current-instrument* rval 'asharp/bflat 7))

(defun bb4 (rval)
  (note *current-instrument* rval 'asharp/bflat 8))

(defun bb5 (rval)
  (note *current-instrument* rval 'asharp/bflat 9))

(defun bb6 (rval)
  (note *current-instrument* rval 'asharp/bflat 10))

(defun bb7 (rval)
  (note *current-instrument* rval 'asharp/bflat 11))

(defun bb8 (rval)
  (note *current-instrument* rval 'asharp/bflat 12))

;;b/cflat ------------------------------

(defun b0 (rval)
  (note *current-instrument* rval 'b/cflat 4))

(defun b1 (rval)
  (note *current-instrument* rval 'b/cflat 5))

(defun b2 (rval)
  (note *current-instrument* rval 'b/cflat 6))

(defun b3 (rval)
  (note *current-instrument* rval 'b/cflat 7))

(defun b4 (rval)
  (note *current-instrument* rval 'b/cflat 8))

(defun b5 (rval)
  (note *current-instrument* rval 'b/cflat 9))

(defun b6 (rval)
  (note *current-instrument* rval 'b/cflat 10))

(defun b7 (rval)
  (note *current-instrument* rval 'b/cflat 11))

(defun b8 (rval)
  (note *current-instrument* rval 'b/cflat 12))

(defun cb0 (rval)
  (note *current-instrument* rval 'b/cflat 4))

(defun cb1 (rval)
  (note *current-instrument* rval 'b/cflat 5))

(defun cb2 (rval)
  (note *current-instrument* rval 'b/cflat 6))

(defun cb3 (rval)
  (note *current-instrument* rval 'b/cflat 7))

(defun cb4 (rval)
  (note *current-instrument* rval 'b/cflat 8))

(defun cb5 (rval)
  (note *current-instrument* rval 'b/cflat 9))

(defun cb6 (rval)
  (note *current-instrument* rval 'b/cflat 10))

(defun cb7 (rval)
  (note *current-instrument* rval 'b/cflat 11))

(defun cb8 (rval)
  (note *current-instrument* rval 'b/cflat 12))

;;bsharp/c ------------------------------

(defun b#0 (rval)
  (note *current-instrument* rval 'bsharp/c 5))

(defun b#1 (rval)
  (note *current-instrument* rval 'bsharp/c 6))

(defun b#2 (rval)
  (note *current-instrument* rval 'bsharp/c 7))

(defun b#3 (rval)
  (note *current-instrument* rval 'bsharp/c 8))

(defun b#4 (rval)
  (note *current-instrument* rval 'bshar/c 9))

(defun b#5 (rval)
  (note *current-instrument* rval 'bsharp/c 10))

(defun b#6 (rval)
  (note *current-instrument* rval 'bsharp/c 11))

(defun b#7 (rval)
  (note *current-instrument* rval 'bsharp/c 12))

(defun b#8 (rval)
  (note *current-instrument* rval 'bsharp/c 13))

(defun c0 (rval)
  (note *current-instrument* rval 'bsharp/c 5))

(defun c1 (rval)
  (note *current-instrument* rval 'bsharp/c 6))

(defun c2 (rval)
  (note *current-instrument* rval 'bsharp/c 7))

(defun c3 (rval)
  (note *current-instrument* rval 'bsharp/c 8))

(defun c4 (rval)
  (note *current-instrument* rval 'bsharp/c 9))

(defun c5 (rval)
  (note *current-instrument* rval 'bsharp/c 10))

(defun c6 (rval)
  (note *current-instrument* rval 'bsharp/c 11))

(defun c7 (rval)
  (note *current-instrument* rval 'bsharp/c 12))

(defun c8 (rval)
  (note *current-instrument* rval 'bsharp/c 13))

;;csharp/dflat ------------------------

(defun c#0 (rval)
  (note *current-instrument* rval 'csharp/dflat 5))

(defun c#1 (rval)
  (note *current-instrument* rval 'csharp/dflat 6))

(defun c#2 (rval)
  (note *current-instrument* rval 'csharp/dflat 7))

(defun c#3 (rval)
  (note *current-instrument* rval 'csharp/dflat 8))

(defun c#4 (rval)
  (note *current-instrument* rval 'csharp/dflat 9))

(defun c#5 (rval)
  (note *current-instrument* rval 'csharp/dflat 10))

(defun c#6 (rval)
  (note *current-instrument* rval 'csharp/dflat 11))

(defun c#7 (rval)
  (note *current-instrument* rval 'csharp/dflat 12))

(defun db0 (rval)
  (note *current-instrument* rval 'csharp/dflat 5))

(defun db1 (rval)
  (note *current-instrument* rval 'csharp/dflat 6))

(defun db2 (rval)
  (note *current-instrument* rval 'csharp/dflat 7))

(defun db3 (rval)
  (note *current-instrument* rval 'csharp/dflat 8))

(defun db4 (rval)
  (note *current-instrument* rval 'csharp/dflat 9))

(defun db5 (rval)
  (note *current-instrument* rval 'csharp/dflat 10))

(defun db6 (rval)
  (note *current-instrument* rval 'csharp/dflat 11))

(defun db7 (rval)
  (note *current-instrument* rval 'csharp/dflat 12))

;;d --------------------------

(defun d0 (rval)
  (note *current-instrument* rval 'd 5))

(defun d1 (rval)
  (note *current-instrument* rval 'd 6))

(defun d2 (rval)
  (note *current-instrument* rval 'd 7))

(defun d3 (rval)
  (note *current-instrument* rval 'd 8))

(defun d4 (rval)
  (note *current-instrument* rval 'd 9))

(defun d5 (rval)
  (note *current-instrument* rval 'd 10))

(defun d6 (rval)
  (note *current-instrument* rval 'd 11))

(defun d7 (rval)
  (note *current-instrument* rval 'd 12))

;; dsharp/eflat --------------------

(defun d#0 (rval)
  (note *current-instrument* rval 'dsharp/eflat 5))

(defun d#1 (rval)
  (note *current-instrument* rval 'dsharp/eflat 6))

(defun d#2 (rval)
  (note *current-instrument* rval 'dsharp/eflat 7))

(defun d#3 (rval)
  (note *current-instrument* rval 'dsharp/eflat 8))

(defun d#4 (rval)
  (note *current-instrument* rval 'dsharp/eflat 9))

(defun d#5 (rval)
  (note *current-instrument* rval 'dsharp/eflat 10))

(defun d#6 (rval)
  (note *current-instrument* rval 'dsharp/eflat 11))

(defun d#7 (rval)
  (note *current-instrument* rval 'dsharp/eflat 12))

(defun eb0 (rval)
  (note *current-instrument* rval 'dsharp/eflat 5))

(defun eb1 (rval)
  (note *current-instrument* rval 'dsharp/eflat 6))

(defun eb2 (rval)
  (note *current-instrument* rval 'dsharp/eflat 7))

(defun eb3 (rval)
  (note *current-instrument* rval 'dsharp/eflat 8))

(defun eb4 (rval)
  (note *current-instrument* rval 'dsharp/eflat 9))

(defun eb5 (rval)
  (note *current-instrument* rval 'dsharp/eflat 10))

(defun eb6 (rval)
  (note *current-instrument* rval 'dsharp/eflat 11))

(defun eb7 (rval)
  (note *current-instrument* rval 'dsharp/eflat 12))

;; e/fflat ------------------

(defun e0 (rval)
  (note *current-instrument* rval 'e/fflat 5))

(defun e1 (rval)
  (note *current-instrument* rval 'e/fflat 6))

(defun e2 (rval)
  (note *current-instrument* rval 'e/fflat 7))

(defun e3 (rval)
  (note *current-instrument* rval 'e/fflat 8))

(defun e4 (rval)
  (note *current-instrument* rval 'e/fflat 9))

(defun e5 (rval)
  (note *current-instrument* rval 'e/fflat 10))

(defun e6 (rval)
  (note *current-instrument* rval 'e/fflat 11))

(defun e7 (rval)
  (note *current-instrument* rval 'e/fflat 12))

(defun fb0 (rval)
  (note *current-instrument* rval 'e/fflat 5))

(defun fb1 (rval)
  (note *current-instrument* rval 'e/fflat 6))

(defun fb2 (rval)
  (note *current-instrument* rval 'e/fflat 7))

(defun fb3 (rval)
  (note *current-instrument* rval 'e/fflat 8))

(defun fb4 (rval)
  (note *current-instrument* rval 'e/fflat 9))

(defun fb5 (rval)
  (note *current-instrument* rval 'e/fflat 10))

(defun fb6 (rval)
  (note *current-instrument* rval 'e/fflat 11))

(defun fb7 (rval)
  (note *current-instrument* rval 'e/fflat 12))

;;esharp/f ----------------------------

(defun e#0 (rval)
  (note *current-instrument* rval 'esharp/f 5))

(defun e#1 (rval)
  (note *current-instrument* rval 'esharp/f 6))

(defun e#2 (rval)
  (note *current-instrument* rval 'esharp/f 7))

(defun e#3 (rval)
  (note *current-instrument* rval 'esharp/f 8))

(defun e#4 (rval)
  (note *current-instrument* rval 'esharp/f 9))

(defun e#5 (rval)
  (note *current-instrument* rval 'esharp/f 10))

(defun e#6 (rval)
  (note *current-instrument* rval 'esharp/f 11))

(defun e#7 (rval)
  (note *current-instrument* rval 'esharp/f 12))

(defun f0 (rval)
  (note *current-instrument* rval 'esharp/f 5))

(defun f1 (rval)
  (note *current-instrument* rval 'esharp/f 6))

(defun f2 (rval)
  (note *current-instrument* rval 'esharp/f 7))

(defun f3 (rval)
  (note *current-instrument* rval 'esharp/f 8))

(defun f4 (rval)
  (note *current-instrument* rval 'esharp/f 9))

(defun f5 (rval)
  (note *current-instrument* rval 'esharp/f 10))

(defun f6 (rval)
  (note *current-instrument* rval 'esharp/f 11))

(defun f7 (rval)
  (note *current-instrument* rval 'esharp/f 12))

;; fsharp/gflat ----------------------

(defun f#0 (rval)
  (note *current-instrument* rval 'fsharp/gflat 5))

(defun f#1 (rval)
  (note *current-instrument* rval 'fsharp/gflat 6))

(defun f#2 (rval)
  (note *current-instrument* rval 'fsharp/gflat 7))

(defun f#3 (rval)
  (note *current-instrument* rval 'fsharp/gflat 8))

(defun f#4 (rval)
  (note *current-instrument* rval 'fsharp/gflat 9))

(defun f#5 (rval)
  (note *current-instrument* rval 'fsharp/gflat 10))

(defun f#6 (rval)
  (note *current-instrument* rval 'fsharp/gflat 11))

(defun f#7 (rval)
  (note *current-instrument* rval 'fsharp/gflat 12))

(defun gb0 (rval)
  (note *current-instrument* rval 'fsharp/gflat 5))

(defun gb1 (rval)
  (note *current-instrument* rval 'fsharp/gflat 6))

(defun gb2 (rval)
  (note *current-instrument* rval 'fsharp/gflat 7))

(defun gb3 (rval)
  (note *current-instrument* rval 'fsharp/gflat 8))

(defun gb4 (rval)
  (note *current-instrument* rval 'fsharp/gflat 9))

(defun gb5 (rval)
  (note *current-instrument* rval 'fsharp/gflat 10))

(defun gb6 (rval)
  (note *current-instrument* rval 'fsharp/gflat 11))

(defun gb7 (rval)
  (note *current-instrument* rval 'fsharp/gflat 12))

;; g ----------------------

(defun g0 (rval)
  (note *current-instrument* rval 'g 5))

(defun g1 (rval)
  (note *current-instrument* rval 'g 6))

(defun g2 (rval)
  (note *current-instrument* rval 'g 7))

(defun g3 (rval)
  (note *current-instrument* rval 'g 8))

(defun g4 (rval)
  (note *current-instrument* rval 'g 9))

(defun g5 (rval)
  (note *current-instrument* rval 'g 10))

(defun g6 (rval)
  (note *current-instrument* rval 'g 11))

(defun g7 (rval)
  (note *current-instrument* rval 'g 12))

;; gsharp/aflat -------------------

(defun g#0 (rval)
  (note *current-instrument* rval 'gsharp/aflat 5))

(defun g#1 (rval)
  (note *current-instrument* rval 'gsharp/aflat 6))

(defun g#2 (rval)
  (note *current-instrument* rval 'gsharp/aflat 7))

(defun g#3 (rval)
  (note *current-instrument* rval 'gsharp/aflat 8))

(defun g#4 (rval)
  (note *current-instrument* rval 'gsharp/aflat 9))

(defun g#5 (rval)
  (note *current-instrument* rval 'gsharp/aflat 10))

(defun g#6 (rval)
  (note *current-instrument* rval 'gsharp/aflat 11))

(defun g#7 (rval)
  (note *current-instrument* rval 'gsharp/aflat 12))

(defun ab0 (rval)
  (note *current-instrument* rval 'gsharp/aflat 5))

(defun ab1 (rval)
  (note *current-instrument* rval 'gsharp/aflat 6))

(defun ab2 (rval)
  (note *current-instrument* rval 'gsharp/aflat 7))

(defun ab3 (rval)
  (note *current-instrument* rval 'gsharp/aflat 8))

(defun ab4 (rval)
  (note *current-instrument* rval 'gsharp/aflat 9))

(defun ab5 (rval)
  (note *current-instrument* rval 'gsharp/aflat 10))

(defun ab6 (rval)
  (note *current-instrument* rval 'gsharp/aflat 11))

(defun ab7 (rval)
  (note *current-instrument* rval 'gsharp/aflat 12))
