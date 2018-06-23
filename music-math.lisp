;(in-package :bogu)

(defparameter *notes* '((bsharp/c . 0) (csharp/dflat . 1) (d . 2)
			(dsharp/eflat . 3) (e/fflat . 4)
			(esharp/f . 5) (fsharp/gflat . 6)
			(g . 7) (gsharp/aflat . 8) (a . 9)
			(asharp/bflat . 10) (b/cflat . 11)))


(defun get-note-value (n)
  "Converts note number (number of semitones above c) to note value."
  (dolist (i *notes*)
    (if (= (cdr i) (mod n 12))
	(return (car i)))))

(defun get-note-number (v)
  "Converts note value to note number (number of semitones above c)."
  (dolist (i *notes*)
    (if (eql (car i) v)
	(return (cdr i)))))

(defun note->pch (note octave)
  "Converts note value to csound's pch value."
  (+ (* 0.01 (get-note-number note)) octave))

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
	((eq 'ht rval) (/ 4.0 3.0))
	((eq 'qq rval) (/ 4.0 5.0))
	((eq 'eq rval) (/ 2.0 5.0))
	((eq 'sq rval) (/ 1 5.0))
	((eq 'tq rval) (/ 0.5 5.0))
	((eq 'hq rval) (/ 8.0 5.0))))
