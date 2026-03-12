;;global variables-------------------

(defparameter *itime* 0)
(defparameter *bpm* '(60 0 "t"))
(defparameter *instruments* '((1 . 0)))
(defparameter *current-instrument* 1)
(defparameter *score* '())
(defparameter *last-sequence* '())
(defparameter *polys* '())
(defparameter *sequences* '())
(defparameter *bogu-code* '())
(defparameter *passages* '())
(defparameter *pas* nil)

;;bogu functions----------------------

(defun reset-bogu ()
  "Resets all global variables to their default values."
  (setf *itime* 0)
  (bpm 60)
  (setf *instruments* '((1 . 0)))
  (setf *current-instrument* 1)
  (setf *score* '())
  (setf *last-sequence* '())
  (setf *polys* '())
  (setf *sequences* '())
  (setf *bogu-code* '())
  (setf *pas* nil)
  (setf *passages* '()))

(defun pas ()
  "Sets pas (passage) variable to t if nil, nil if t."
  (if (null *pas*)
      (progn
	(setf *pas* t)
	(let ((n 1))
	  (if (not (assoc n *passages*))
	      (push (cons n nil) *passages*)
	      (progn
		(if (cdr (assoc n *passages*))
		    (push (cons (1+ (first (first *passages*))) nil) *passages*))))))
      (progn
	(setf (cdr (assoc (length *passages*) *passages*))
	      (reverse (cdr (assoc (length *passages*) *passages*))))
	(setf *pas* nil)))
  (format t "passage ~:[end~;start~]~%" *pas*))

(defun psgs ()
  "Displays all written passages."
  (if *passages*
      (format t "~%~{~{~a~%~}~}~%" (reverse *passages*))
      (format t "~%nothing here yet...~%~%")))

(defun i (n)
  "Changes the instrument receiving input."
  (if (not (assoc n *instruments*))
      (push `(,n . 0) *instruments*))
  (setf (cdr (assoc *current-instrument* *instruments*)) *itime*)
  (setf *itime* (cdr (assoc n *instruments*)))
  (setf *current-instrument* n))

(defun defpoly (name &rest notes)
  "Pushes a user defined poly to polys list."
  (push (cons name notes) *polys*))

(defun polys ()
  "Displays current ledger of poly definitions."
  (if *polys*
      (format t "~%~{~a ~%~}~%" (reverse *polys*))
      (format t "~%nothing here yet...~%~%")))
  
(defun poly (rval &rest notes)
  "Pushes a poly to the score list."
  (let ((r (rtm rval)))
    (if (assoc (car notes) *polys*)
	(eval (append `(poly ,r)
		      (mapcar #'fn-it
			      (cdr (assoc (car notes) *polys*)))))
	(progn
	  (dolist (i notes)
	    (incf *current-instrument* 0.01)
	    (funcall i r)
	  (decf *itime* r))
	(setf *current-instrument* (floor *current-instrument*))
	(incf *itime* r))))) 

(defun del (n)
  "Deletes n notes beginning with the last note entered."
  (dotimes (i n)
    (pop *score*))
  ;; Reset *itime* to the end of whatever note is now at the top of the list
  (if *score*
      (let ((last-note (car *score*)))
        ;; last-note looks like: ("i" instr itime dur pch)
        ;; indices:               0    1      2     3    4
        (setf *itime* (+ (nth 2 last-note) (nth 3 last-note))))
      (setf *itime* 0)))

(defun defseq (name &rest notes)
  "Pushes a user defined sequence to the sequences list."
  (push (cons name notes) *sequences*))

(defun seqs ()
  "Displays current ledger of user defined sequences."
  (if *sequences*
      (format t "~%~{~a ~%~}~%" (reverse *sequences*))
      (format t "~%nothing here yet...~%~%")))

(defun seq (rval &rest notes)
  "Pushes sequence of notes to score list, replaces last sequence list with said sequence."
  (setf *last-sequence* '())
  (let ((r (rtm rval)))
    (dolist (i notes)
      (cond ((assoc i *polys*)
	     (eval (append `(poly ,r)
			   (mapcar #'fn-it
				   (cdr (assoc i *polys*))))))
	    ((assoc i *sequences*)
	     (eval (append `(seq ,r)
			   (mapcar #'fn-it
				   (cdr (assoc i *sequences*))))))
	    (t (funcall i r))))
    (setf *last-sequence* (flatten `(seq ,r ,notes)))))

(defun sarp (rval sval &rest notes)
  "Pushes a sustained arpeggio to the score list. Parameters specify the arpeggio's rhythm and length of sustain."
  (let ((r (rtm rval))
	(s (rtm sval)))
    (if (assoc (car notes) *sequences*)
	(eval (append `(sarp ,r ,s)
		      (mapcar #'fn-it
			      (cdr (assoc (car notes) *sequences*)))))
	(progn
	  (dotimes (i (length notes))
	    (incf *current-instrument* 0.01)
	    (funcall (elt notes i) (- s (* r i)))
	    (decf *itime* (- s (* r (1+ i)))))
	  (setf *current-instrument* (floor *current-instrument*))
	  (incf *itime* (- s (* r (length notes))))))))

(defun % ()
  "Evaluates last sequence list."
  (if (not (assoc (third *last-sequence*) *sequences*))
      (eval (cons (car *last-sequence*)
		  (mapcar #'quote-it (cdr *last-sequence*))))
      (eval (cons (car *last-sequence*)
		  (cons (quote-it (cadr *last-sequence*))
			(cddr *last-sequence*))))))

(defun psg (n)
  "Repeat a specified passage."
  (dolist (i (cdr (assoc n *passages*)))
    (eval (bogu-reader i))))

(defun rpt (n &optional (s 0))
  "Repeats the last n notes, starting from the sth last note, preserving chords."
  (let* ((chunk (subseq *score* s (+ s n)))
         ;; The oldest note in the chunk we are copying
         (oldest-note (car (last chunk))) 
         (start-time (nth 2 oldest-note))
         ;; Calculate the exact time difference to shift the copied block to *itime*
         (time-offset (- *itime* start-time)))
    
    ;; Iterate through the chunk in chronological order
    (dolist (event (reverse chunk))
      (let ((instr (nth 1 event))
            (old-itime (nth 2 event))
            (dur (nth 3 event))
            (pch (nth 4 event)))
        ;; Push the copied note with its new offset time
        (push (list "i" instr (+ old-itime time-offset) dur pch) *score*)))
    
    ;; Update the global *itime* to the end of the newly pasted block
    (let ((last-new-note (car *score*)))
      (setf *itime* (+ (nth 2 last-new-note) (nth 3 last-new-note))))))

(defun rst (rval)
  "Increases the itime of next note."
  (incf *itime* (rtm rval)))

(defun bpm (n); add optional nth bpms
  "Sets beats per minute."
  (setf *bpm* '())
  (push "t" *bpm*)
  (push 0 *bpm*)
  (push n *bpm*))

(defun where (div)
  "Given a divisor, displays the current measure and beat as the itime divided by the divisor and their remainder respectively."
  (format t "~%~%Measure: ~d~%Beat:    ~d~%~%"
	  (1+ (floor (/ *itime* div)))
	  (mod *itime* div)))

(defun note (i rval nval oval)
  "Pushes note data to score list as a structured sub-list."
  ;; We group the 5 Csound p-fields into their own list before pushing.
  (push (list "i" i *itime* (rtm rval) (note->pch nval oval)) *score*)
  (incf *itime* (rtm rval)))

(defun save (filename)
  "Saves the bogu code data as a .bogu file and the composition data as a csound .csd file to the compositions folder."
  (with-open-file (out (comp-path filename (bogu-folder filename) "bogu")
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (dolist (i (reverse *bogu-code*))
	(cond ((string= "%" i)
	       (format out "~{~a~}~%"  (list i))))
	(cond ((not (or (string= "%" i) ;;this command's single character length messes up checks to string= below
			(null (coerce i 'list))
			(string= (stringem "load " #\" filename #\") i);;preventing endless calls to load
			(string= "pla" i :start2 0 :end2 3)
			(string= "sav" i :start2 0 :end2 3)))
	       (format out "~{~a~%~}" (list i)))))))
  (bogu->csd filename)
  (format t "saved \"compositions/~a/~a.bogu\"~%" filename filename)
  (format t "saved \"compositions/~a/~a.csd\"~%" filename filename))

(defun bogu-load (filename)
  "Reads input from a .bogu file, safely handling errors on a line-by-line basis."
  (reset-bogu)
  (with-open-file (in (comp-path filename (bogu-folder filename) "bogu")
                      :direction :input
                      :if-does-not-exist nil)
    (if in
        (loop for line = (read-line in nil)
              while line do
                (unless (string= line "") ; Skip empty lines
                  (push line *bogu-code*)
                  ;; Trap reading errors line-by-line
                  (let ((cmd (handler-case (bogu-reader line)
                               (error (e)
                                 (format t "[File Reader Error] Skipping line: ~A~%Details: ~A~%" line e)
                                 nil))))
                    ;; Trap evaluation errors line-by-line
                    (when cmd
                      (composition-eval cmd)))))
        (format t "Error: File ~a.bogu not found.~%" filename)))
  (format t "loaded \"compositions/~a/~a.bogu\"~%" filename filename))

(defun play (filename)
  "Creates and plays a csound .csd file and streams output to the REPL."
  (format t "playing \"compositions/~a/~a.csd\"...~%" filename filename)
  (sb-ext:run-program "/usr/bin/csound" ;; <-- Updated path (verify with 'which csound' if it still fails)
                      (list (namestring (comp-path filename (bogu-folder filename) "csd")))
                      :search t   ;; Tells SBCL to search your system PATH just in case
                      :output t   ;; Streams Csound's terminal output into SLIME
                      :error t))  ;; Streams any Csound errors into SLIME
  
;; note macro ----------------------------

(defmacro generate-notes (pitches octaves)
  "Automatically generates bogu note functions (e.g., c4, eb3) based on pitch names and octaves."
  `(progn
     ,@(loop for pitch in pitches
             append (loop for octave in octaves
                          for func-name = (intern (string-downcase (format nil "~a~a" pitch octave)))
                          collect `(defun ,func-name (rval)
                                     (note *current-instrument* rval ',pitch ,octave))))))

;; Call the macro with specific pitch list and octave range
(generate-notes (a a# bb b cb b# c c# db d d# eb e fb e# f f# gb g g# ab)
                (0 1 2 3 4 5 6 7 8))

