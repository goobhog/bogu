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


