 (in-package :bogu)

(defparameter *notes* '((b# . 0) (c . 0)
    (c# . 1) (db . 1)
    (d . 2)
    (d# . 3) (eb . 3)
    (e . 4) (fb . 4)
    (e# . 5) (f . 5)
    (f# . 6) (gb . 6)
    (g . 7)
    (g# . 8) (ab . 8)
    (a . 9)
    (a# . 10) (bb . 10)
    (b . 11) (cb . 11)))

(defparameter *scale-intervals*
  '((MAJOR . (0 2 4 5 7 9 11))
    (MINOR . (0 2 3 5 7 8 10))
    (DORIAN . (0 2 3 5 7 9 10))
    (PHRYGIAN . (0 1 3 5 7 8 10))
    (LYDIAN . (0 2 4 6 7 9 11))
    (MIXOLYDIAN . (0 2 4 5 7 9 10))))

(defun get-note-value (n)
  "Converts note number (semitones above C) to a note symbol."
  (let ((semitone (mod n 12)))
    (car (find-if (lambda (pair) (= (cdr pair) semitone)) *notes*))))

(defun get-note-number (v)
  "Converts note symbol to a note number. Safely throws an error if the note is missing."
  (let ((found (assoc v *notes*)))
    (if found
        (cdr found)
        (error "Music Math Error: The pitch '~A' is not defined in the *notes* dictionary." v))))

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

(defun calculate-diatonic-pitch (base-pitch ast-transpose trk)
  "Applies Track Key and Transposition rules to a base pitch, returning (VALUES pitch octave)."
  (let* ((trk-key (track-key trk))
         (trk-transpose (track-transpose-offset trk))
         (total-transpose (+ ast-transpose trk-transpose))
         ;; THE FIX: Safely default to Chromatic Transposition if there is no Key!
         (transposed-pitch (+ base-pitch total-transpose))) 

    ;; Run your Diatonic Delta Math here if a key exists
    (when (and trk-key (not (= total-transpose 0)))
      (let* ((root-str (string (car trk-key)))
             (scale-str (string (cadr trk-key)))
             (root-assoc (find-if (lambda (x) (string-equal (string (car x)) root-str)) *notes*))
             (intervals-assoc (find-if (lambda (x) (string-equal (string (car x)) scale-str)) *scale-intervals*)))
             
        (when (and root-assoc intervals-assoc)
          (let* ((root-pitch (round (cdr root-assoc)))
                 (intervals (cdr intervals-assoc))
                 (scale-pitches (mapcar (lambda (x) (+ root-pitch x)) intervals))
                 (normalized-pitch (mod (round base-pitch) 12))
                 (degree (position normalized-pitch scale-pitches :key (lambda (x) (mod x 12)))))
                 
            (if degree
                (let* ((new-degree (+ degree total-transpose))
                       (octave-shift (floor new-degree (length scale-pitches)))
                       (wrapped-degree (mod new-degree (length scale-pitches)))
                       (original-absolute (nth degree scale-pitches))
                       (new-absolute (nth wrapped-degree scale-pitches))
                       (total-new-absolute (+ new-absolute (* octave-shift 12)))
                       (delta (- total-new-absolute original-absolute)))
                  ;; OVERRIDE with Diatonic Math!
                  (setf transposed-pitch (+ base-pitch delta)))
                ;; Note isn't in scale? The chromatic fallback is already applied!
                )))))

    ;; Calculate Octave wrapping
    (let ((new-pitch transposed-pitch)
          (new-octave 4)) ;; Assume octave 4 base
      (loop while (> new-pitch 11) do (decf new-pitch 12) (incf new-octave))
      (loop while (< new-pitch 0) do (incf new-pitch 12) (decf new-octave))
      (values new-pitch new-octave))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro generate-notes (pitches octaves)
    "Automatically generates bogu note functions (e.g., C4, EB3). Aligned with new scheduler!"
    `(progn
       ,@(loop for pitch in pitches
               append (loop for octave in octaves
                            for func-name = (intern (string-upcase (format nil "~a~a" pitch octave)))
                            collect `(defun ,func-name (rval)
                                       ;; The Correct Order: Pitch, Octave, Rhythm
                                       (schedule-note ',pitch ,octave rval)))))))

(generate-notes (a a# bb b cb b# c c# db d d# eb e fb e# f f# gb g g# ab)
                (0 1 2 3 4 5 6 7 8))
