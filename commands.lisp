;;global variables-------------------

(defparameter *itime* 0)
(defparameter *bpm* '(60 0 "t"))
(defparameter *instruments* '((1 . 0)))
(defparameter *current-instrument* 1)
(defparameter *score* '())
(defparameter *last-sequence* '())
(defparameter *bogu-code* '())
(defparameter *passages* '())
(defparameter *pas* nil)
(defparameter *vars* '())
(defparameter *current-project* nil)

;;bogu functions----------------------

(defun reset-bogu ()
  "Resets all global variables to their default values."
  (setf *itime* 0)
  (bpm 60)
  (setf *instruments* '((1 . 0)))
  (setf *current-instrument* 1)
  (setf *score* '())
  (setf *last-sequence* '())
  (setf *bogu-code* '())
  (setf *pas* nil)
  (setf *passages* '())
  (setf *vars* '())
  (setf *current-project* nil))

(defun def (name &rest body)
  "Binds a bogu command to a variable, overwriting it if it already exists."
  (let ((existing (assoc name *vars* :test #'equal)))
    (if existing
        ;; If it exists, overwrite the old memory with the new notes
        (setf (cdr existing) body) 
        ;; If it's new, push it to the memory bank
        (push (cons name body) *vars*)))
  (format t "~%[Bound] ~a -> ~a~%~%" name body))

(defun vars ()
  "Displays the current ledger of all user-defined variables."
  (if *vars*
      (format t "~%~{~a ~%~}~%" (reverse *vars*))
      (format t "~%nothing here yet...~%~%")))

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

(defun save (&optional filename)
  "Saves the project, prompting for a name if none exists."
  ;; 1. If they typed a name (e.g., 'save mytrack'), set it as the current project
  (when filename
    (setf *current-project* (string-downcase (string filename))))
    
  ;; 2. If there is still no project name, prompt for one!
  (unless *current-project*
    (format t "Enter a name for this new project: ")
    (finish-output)
    (let ((name (read-line)))
      (if (string= name "")
          (progn (format t "Save cancelled.~%") (return-from save))
          (setf *current-project* name))))
          
  ;; 3. Proceed with saving using *current-project*
  (let ((fname *current-project*))
    (with-open-file (out (comp-path fname (bogu-folder fname) "bogu")
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (dolist (line (reverse *bogu-code*))
          (let* ((parsed (ignore-errors (read-from-string (format nil "(~a)" line))))
                 (cmd (car parsed)))
            (cond 
              ((string= "%" line) (format out "~a~%" line))
              ((or (null parsed)
                   (member cmd '(play save help vars where seqs polys psgs bogu-load)))
               nil)
              (t (format out "~a~%" line)))))))
    (format t "saved \"compositions/~a/~a.bogu\"~%" fname fname)))

(defun play (&optional filename)
  "Compiles the current state into a .csd and plays it."
  (when filename
    (setf *current-project* (string-downcase (string filename))))
    
  (unless *current-project*
    (format t "Error: This project has no name yet. Please type 'save' to name it first.~%")
    (return-from play))
    
  (let ((fname *current-project*))
    (bogu->csd fname)
    (format t "playing \"compositions/~a/~a.csd\"...~%" fname fname)
    (sb-ext:run-program "/usr/bin/csound" 
                        (list (namestring (comp-path fname (bogu-folder fname) "csd")))
                        :search t   
                        :output t   
                        :error t)))

(defun bogu-load (&optional filename)
  "Loads a project, prompting to save if there is unsaved work."
  ;; 1. Check if we are mid-project. If *score* has data, prompt to save!
  (when *score*
    (format t "Save your current project before loading a new one? (y/n): ")
    (finish-output)
    (let ((ans (read-line)))
      (when (string= (string-downcase ans) "y")
        (save)))) ;; Calls our newly upgraded save function

  ;; 2. Determine what file to load
  (let ((target (if filename 
                    (string-downcase (string filename))
                    (progn
                      (format t "Enter project name to load: ")
                      (finish-output)
                      (read-line)))))
                      
    ;; 3. If a valid name was given, wipe the slate and load it
    (when (not (string= target ""))
      (reset-bogu)
      (setf *current-project* target) ;; Lock in the loaded project name
      (with-open-file (in (comp-path target (bogu-folder target) "bogu")
                          :direction :input
                          :if-does-not-exist nil)
        (if in
            (loop for line = (read-line in nil)
                  while line do
                    (unless (string= line "")
                      (push line *bogu-code*)
                      (let ((cmd (handler-case (bogu-reader line)
                                   (error (e) nil))))
                        (when cmd (composition-eval cmd)))))
            (format t "Error: File ~a.bogu not found.~%" target)))
      (format t "loaded \"compositions/~a/~a.bogu\"~%" target target))))
  
;; macros ----------------------------

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

