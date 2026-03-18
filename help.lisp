(defun help ()
  "Prints the Bogu command cheat sheet."
  (format t "~%===========================================================~%")
  (format t "                       BOGU HELP MENU                        ~%")
  (format t "===========================================================~%~%")
  
  (format t " [ NOTES & BASIC ENTRY ]~%")
  (format t "  <note> <rval> : Enter a note (e.g., c4 q, eb3 .5, f#5 qt)~%")
  (format t "  rst <rval>    : Enter a rest of the specified rhythm.~%")
  (format t "  del <n>       : Delete the last <n> notes and rewind time.~%")
  (format t "  rpt <n>       : Repeat the last <n> notes.~%~%")
  
  (format t " [ SEQUENCES & CHORDS ]~%")
  (format t "  seq <rval> <notes>  : Sequence of notes (e.g., seq q c4 e4 g4)~%")
  (format t "  poly <rval> <notes> : Enter a chord (preserves global time).~%")
  (format t "  sarp <rval> <sval> <notes> : Sustained arpeggio (rhythm & sustain length).~%")
  (format t "  fluid <rval> <notes>: Scatters notes randomly within a time boundary.~%~%")
  
  (format t " [ TIME & ENVIRONMENT ]~%")
  (format t "  bpm <n>        : Set beats per minute (Default: 60).~%")
  (format t "  i <n>          : Change active instrument (e.g., i 1, i 2).~%")
  (format t "  where <div>    : Display current measure and beat based on divisor.~%")
  (format t "  cell <rval> [commands] : Creates a protected micro-timeline bucket.~%")
  (format t "  reset          : Wipe score and reset all global variables to 0.~%~%")

  (format t " [ DEFINITIONS & MEMORY ]~%")
  (format t "  def <name> <commands> : Bind notes or blocks to a variable (O(1) lookup).~%")
  (format t "  vars                  : View all saved variables in memory.~%~%")

  (format t " [ SYSTEM & PLAYBACK ]~%")
  (format t "  save \"name\"      : Save composition to a .bogu and .csd file.~%")
  (format t "  bogu-load \"name\" : Load and evaluate a saved .bogu file.~%")
  (format t "  play \"name\"      : Render and play the saved .csd file.~%")

  (format t "~%--- LOGIC & ITERATION -------------------------------------~%")
  (format t " loop <n> [ <commands> ]~%")
  (format t "   Repeats the commands inside the brackets N times.~%")
  (format t "   Ex: loop 4 [ sarp et h c4 ]~%~%")
  
  (format t " chance <%> [ <true-commands> ] [ <optional-false> ]~%")
  (format t "   Executes the first block with a % probability.~%")
  (format t "   Ex: chance 75 [ seq q c4 ] [ seq q g4 ]~%~%")
  
  (format t " if (<condition>) [ <true-commands> ] [ <optional-false> ]~%")
  (format t "   Executes a block based on a Lisp mathematical condition.~%")
  (format t "   Ex: if (= *current-instrument* 1) [ seq q c4 ]~%~%")

  (format t "~%--- CONDITIONS & MATH (Inside Parentheses) --------------~%")
  (format t "   =  (Equal)           /= (Not Equal)~%")
  (format t "   <  (Less than)       >  (Greater than)~%")
  (format t "   <= (Less or equal)   >= (Greater or equal)~%")
  (format t "   evenp (Is even?)     oddp (Is odd?)~%")
  (format t "   (random N) (Generates a number from 0 to N-1)~%~%")

  (format t "~%--- BOGU ENVIRONMENT VARIABLES ----------------------------~%")
  (format t "   *current-instrument* : The ID of the active instrument~%")
  (format t "   *itime* : The current timeline position in beats~%")
  (format t "   *bpm* : The current global tempo~%")
  (format t "   *current-project* : The name of the loaded file~%")
  (format t "   *vars* : List of all your 'def' variables~%")
  
  (format t "===========================================================~%~%"))
