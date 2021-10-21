(defun all-positions (item list)
  "Returns a new list of all positions an item appears at in a list."
  (let ((l nil))
    (dotimes (i (length list))
      (if (equal item (elt list i))
	  (push i l)))
    (nreverse l)))

(defun flatten (l)
  "Removes parentheses from a multi-dimensional list."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun fn-it (fn)
  "Calls function on an item."
  (list 'function fn))

(defun quote-it (p)
  "Quotes an item."
  (list 'quote p))

(defun stringem (&rest items)
  "Adjoins items as one string."
  (format nil "~{~a~^~}" items))

(defun bogu-folder (name)
  "Checks for a specified directory in bogu/compositions and creates one if it doesn't exist."
  (ensure-directories-exist (stringem 'bogu/compositions/ name #\/)))

(defun bogu-reader (code)
  "Formats bogu code for lisp reader."
  (if *pas*
      (if (not (string= code "pas"))
	  (push code (cdr (assoc (length *passages*) *passages*)))))
  (let ((cmd (read-from-string
	      (concatenate 'string
			   "(" (remove #\; code) ")"))))
 ;;if a user enters ';' by accident, it would be interpreted as a comment, signaling an end-of-file error
    (cond ((or (and (eql (car cmd) 'poly)
		    (member (cdr cmd) *allowed-commands*))
	       (and (eq (car cmd) 'seq)
		    (member (cdr cmd) *allowed-commands*)))
	   (append (list (car cmd) (quote-it (cadr cmd)))
		   (mapcar #'fn-it (cddr cmd))))
	  ((and (eql (car cmd) 'sarp)
		(member (cddr cmd) *allowed-commands*))
	   (append (list (elt cmd 0)
			 (quote-it (elt cmd 1))
			 (quote-it (elt cmd 2)))
		   (mapcar #'fn-it (cdddr cmd))))
	  ((eq (car cmd) 'load) (append '(bogu-load) (cdr cmd)))
	  (t (cons (car cmd) (mapcar #'quote-it (cdr cmd)))))))

(defun comp-path (filename directory type)
  "Creates a pathname with specified name of specified type in specified directory."
  (make-pathname
   :directory `(:relative ,directory)
   :type type
   :name filename))

(defun bogu->csd (filename)
  "Prints bogu score data to csound .csd file."
  (with-open-file (out (comp-path filename (bogu-folder filename) "csd")
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (format out "<CsoundSynthesizer>
<CsOptions>

-odac

</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 4

giwave	ftgen	2, 0, 4096, 10, 0.216, 0.130, 0.043, 0.026, 0.016, 0.011, 0.008, 0.007, 0.004, 0.001, 0.002, 0.003, 0.001, 0.001

~{instr ~d
ares linen .4, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin~%~%~}
</CsInstruments>
<CsScore>

~{~a ~d ~d~}

~{~a ~d ~d ~d ~d~%~}
e
</CsScore>
</CsoundSynthesizer>"
	      (loop for i from 1 to (length *instruments*) collecting i)
	      (reverse *bpm*) (reverse *score*)))))
