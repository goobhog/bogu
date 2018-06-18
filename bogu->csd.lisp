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
      (format out  "<CsoundSynthesizer>~%<CsOptions>~%~%-odac~%~%</CsOptions>~%<CsInstruments>~%~%sr = 44100~%ksmps = 32~%nchnls = 2~%0dbfs = 4~%~%~{instr ~d~%~%ares linen .5, .03, p3, .02~%asig oscil ares, cpspch(p4)~%     outs asig,asig~%~%endin~%~%~}</CsInstruments>~%<CsScore>~%~%~{~a ~d ~d~}~%~%~{~a ~d ~d ~d ~d~%~}~%e~%</CsScore>~%</CsoundSynthesizer>" (loop for i from 1 to (length *instruments*) collecting i)
	      (reverse *bpm*) (reverse *score*)))))
