(defun help ()
  (format t "~%Each note of each octave is its own symbol, 
e.g. c's third octave is written c3, g's lowest octave is written g0

Notes range from a0 to c8.
Sharps and flats are written # and b respectively, 
e.g. b flat's 3rd octave is bb3, c sharp's 5th octave is written c#5

Rests are written rst
Each note or rest is followed by its rhythmic value, 
1 being a quarter note, .5 is an eighth note, 
.25 sixteenth, 2 half, 4 whole, etc.
Triplets are then various divisions of beats into 3,
.66 being quarter note triplet, .33 eighth,
.165 sixteenth, 1.33 half

To write a sequence of notes and rests with the same rhythmic value,
type seq rval nval nval...etc.
e.g. seq .5 c3 e3 g3 bb4 rst f2 a3 c3 f3
seq will take as many notes and rests as you give it.

To write a chord in one instrument
type chord rval nval nval...etc.
e.g. chord 2 d3 f#3 c4 f4 bb5

Typing bpm followed by a number will set the beats per minute,
e.g. bpm 140
The default is 60.

Typing instrument followed by a number
will change which instrument you're writing to,
setting the itime to where you left off with that instrument;
or, if an instrument with that number does not yest exist,
will create a new instrument with that number,
setting the itime to 0. The default instrument is 1.

To save your composition, type save \"filepathname\" (in quotes)
This will save a csound .csd file at that location.
To play your composition, type play \"filepathname\" (in quotes)
To quit, type quit~%~%"))    
