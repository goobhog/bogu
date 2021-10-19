;; CURRENTLY NOT COMPLETE OR CORRECT!
(defun help-loop ()
  (let* ((txt (read)))
    (cond ((eq 'n txt) (format t "~%Each note of each octave is its own symbol, 
e.g. c's third octave is written c3, g's lowest octave is written g0

Notes range from a0 to c8.
Sharps and flats are written # and b respectively, 
e.g. b flat's 3rd octave is bb3, c sharp's 5th octave is written c#5
Type del followed by a number to delete the last n notes
e.g. del 4

Rests are written rst

Each note or rest is followed by its rhythmic value, 
q - quarter note
e - eighth note
s - sixteenth note
h - half note
w - whole note

Write . or t with each symbol to make it dotted
or of triplet value respectively.

e.g. c sharp's fourth octave dotted half-note
c#4 h.

or f's third octave eigth note triplet
f3 et

The user may also type a number in place of a rhythmic symbol.
e.g. f3 .5~%~%") (help-loop))
	  ((eq 's txt) (format t "~%To write a sequence of notes and rests with the same rhythmic value,
type seq rval nval nval...etc.
e.g. seq q c3 e3 g3 bb4 rst f2 a3 c3 f3
seq will take as many notes and rests as you give it.
To rewrite the last sequence simply type %~%~%") (help-loop))
	  ((eq 'p txt) (format t "~%To write a chord in one instrument
type poly rval nval nval...etc.
e.g. poly 2 d3 f#3 c4 f4 bb5
A sustained arpeggio is written by typing sarp followed by
the rhythmic value and length of whole arpeggio
e.g. sarp .5 4 a3 e3 g3 b4 c4 e4~%~%") (help-loop))
	  ((eq 'b txt) (format t "~%Typing bpm followed by a number will set the beats per minute,
e.g. bpm 140
The default is 60.~%~%") (help-loop))
	  ((eq 'd txt) (format t "~%The user may define her own chords or sequences by typing 
defpoly name nval nval nval...etc. or defseq name nval nval nval...etc. respectively.
e.g. defpoly bbmaj9 bb2 f2 d3 a4 c4
     defseq bbscale bb3 c3 d3 eb3 f3 g3 a4 bb4
Chords and sequences can then be given as parameters to the poly and seq functions,
e.g. poly h. bbmaj9
     seq et bbscale 
To view the current ledger of chord or sequence definitions, type polys or seqs~%~%") (help-loop))
	  ((eq 'i txt) (format t "~%Typing i followed by a number
will change which instrument you're writing to,
setting the itime to where you left off with that instrument;
or, if an instrument with that number does not yest exist,
will create a new instrument with that number,
setting the itime to 0. The default instrument is 1.~%~%") (help-loop))
	  ((eq 'l txt) (format t "~%To save your composition, type save \"name\" (in quotes)
This will save a .bogu file in the bogu/compositions folder.
To play your composition, type play \"name\" (in quotes)
This will create and play a csound .csd file in the bogu/compositions foler.

Typing reset will reset the composition status.

To quit, type quit~%~%") (help-loop))
	  ((eq 'e txt) (format t "~%exit~%~%")))))

(defun help ()
  (format t "~%Help Menu~%(n)otes and rests~%(s)equences~%(p)olys and sustained arpeggios~%(b)pm~%(i)nstruments~%(d)efinitions~%(l)oad, save, play, reset and quit~%(e)xit help menu~%~%")
  (help-loop))
