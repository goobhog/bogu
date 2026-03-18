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
  "Adjoins items as one lowercase string."
  (string-downcase (format nil "~{~a~^~}" items)))

(defun bogu-folder (name)
  "Checks for a specified directory in compositions/ and creates one if it doesn't exist."
  (ensure-directories-exist (stringem 'compositions/ name #\/)))

(defun comp-path (filename directory type)
  "Creates a pathname with specified name of specified type in specified directory."
  (make-pathname :name filename
                 :type type
                 :defaults (parse-namestring directory)))

(defun note-p (sym)
  "Checks if a symbol follows the bogu note pattern (e.g., c4, f#3, bb2)."
  (let ((str (string-downcase (symbol-name sym))))
    (cl-ppcre:scan "^[a-g][#b]?[0-8]$" str)))

(defun expand-vars (args)
  "Recursively expands variables in an argument list. No quote-peeling required!"
  (loop for arg in args
        for var-lookup = (and (symbolp arg) (gethash arg *vars*))
        if var-lookup
          append (expand-vars var-lookup)
        else
          collect arg))

(defun bogu->csd (filename)
  "Prints bogu score data to a static csound .csd file, including velocity."
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

giwave  ftgen   2, 0, 4096, 10, 0.216, 0.130, 0.043, 0.026, 0.016, 0.011, 0.008, 0.007, 0.004, 0.001, 0.002, 0.003, 0.001, 0.001

~{instr ~d
ares linen p5, .03, p3, .02
asig poscil ares, cpspch(p4), 2
outs asig, asig
endin~%~%~}
</CsInstruments>
<CsScore>

~{~a ~d ~d~}

~{~a ~d ~d ~d ~d ~d~%~}
e
</CsScore>
</CsoundSynthesizer>"
              (loop for i from 1 to (length *instruments*) collecting i)
              (reverse *bpm*) 
              ;; The Exporter Bridge: translates Plist to Csound format
              (loop for event in (reverse *score*)
                    append (list "i" 
                                 (getf event :instr) 
                                 (getf event :time) 
                                 (getf event :dur) 
                                 (getf event :pch)
                                 (getf event :vel)))))))
