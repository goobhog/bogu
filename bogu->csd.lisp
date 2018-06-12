(defun bogu->csd (filename)
  "Prints bogu score data to csound .csd file."
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (format out  "<CsoundSynthesizer>~%<CsOptions>~%~%-odac~%~%</CsOptions>~%<CsInstruments>~%~%sr = 44100~%ksmps = 32~%nchnls = 2~%0dbfs = 4~%~%~{instr ~d~%~%asig oscil .6, cpspch(p4)~%     outs asig,asig~%~%endin~%~%~}</CsInstruments>~%<CsScore>~%~%~{~a ~d ~d~}~%~%~{~a ~d ~d ~d ~d~%~}~%e~%</CsScore>~%</CsoundSynthesizer>" (loop for i from 1 to (length *instruments*) collecting i)
	      (nreverse *bpm*) (nreverse *score*)))))
