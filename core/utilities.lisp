;;core/utilities.lisp
(in-package :bogu)

(defun flatten (l)
  "Removes parentheses from a multi-dimensional list."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

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

(defun parse-note-symbol (sym)
  "Splits a symbol like C#4 into pitch C# and octave 4."
  (let* ((str (string-upcase (symbol-name sym)))
         ;; The last character is the octave
         (octave-char (char str (1- (length str))))
         ;; Everything before the last character is the pitch class
         (pitch-str (subseq str 0 (1- (length str)))))
    (values (intern pitch-str "BOGU")
            (digit-char-p octave-char))))

(defun expand-vars (args)
  "Recursively expands variables, checking both user and stdlib memory banks."
  (loop for arg in args
        for user-lookup = (and (symbolp arg) (gethash arg *vars*))
        for std-lookup = (and (symbolp arg) (gethash arg *stdlib-vars*))
        for var-lookup = (or user-lookup std-lookup)
        
        if var-lookup
          ;; THE FIX: Force the target to remain a safe list structure during splicing.
          ;; We removed the aggressive (car expanded) truncation that was deleting
          ;; all elements of a musical array except the first note!
          append (let* ((is-single-cmd (and (listp var-lookup)
                                            (symbolp (car var-lookup))
                                            (or (gethash (car var-lookup) *command-dictionary*)
                                                (gethash (car var-lookup) *rewrite-rules*))))
                        (target (if is-single-cmd 
                                    (list var-lookup) 
                                    (if (listp var-lookup) var-lookup (list var-lookup)))))
                   (expand-vars target))
        else if (listp arg)                 
          collect (expand-vars arg)         
        else
          collect arg))

(defun smart-unwrap (raw-nodes)
  (if (and (= (length raw-nodes) 1) 
           (listp (car raw-nodes))
           (not (and (symbolp (car (car raw-nodes)))
                     (or (gethash (car (car raw-nodes)) *command-dictionary*)
                         (gethash (car (car raw-nodes)) *rewrite-rules*))))) ;; <- ADDED
      (car raw-nodes) 
      raw-nodes))

;; =============================================================================
;; THE EVENT FACTORY & DATA HELPERS
;; =============================================================================

(defun make-note-event (pitch-sym time written-dur &key octave dur pch vel (instr *current-instrument*) transpose)
  "Axiomatic constructor for all Bogu Note events."
  (list :type :note 
        :pitch-symbol pitch-sym 
        :octave octave 
        :time (float time) 
        :written-dur (float written-dur)
        :dur (if dur (float dur) (float written-dur))
        :pch pch 
        :vel vel 
        :instr instr 
        :transpose transpose))

(defun make-rest-event (time dur &key (instr *current-instrument*))
  "Axiomatic constructor for pure time/silence."
  (make-note-event 'RST time dur :instr instr))

(defun make-control-event (param start end time dur &key (instr *current-instrument*))
  "Axiomatic constructor for automation data."
  (list :type :control :param param :start (float start) :end (float end)
        :time (float time) :written-dur (float dur) :dur (float dur) :instr instr))

(defun make-meta-event (subtype time &key (dur 0.0) val (instr *current-instrument*))
  "Axiomatic constructor for notation/system metadata."
  (list :type :meta :subtype subtype :val val :time (float time) :written-dur (float dur) :dur (float dur) :instr instr))

(defun resolve-param-id (sym)
  "Converts a user symbol (e.g., 'VOL) into its Csound hardware ID."
  (let* ((str (string-upcase (string sym)))
         (entry (assoc (intern str) *mixer-params*)))
    (if entry (cdr entry)
        (error "[Mixer Error] Unknown parameter '~A'." sym))))

(defun shift-events (events offset-time)
  "Returns a new list of events, shifting their absolute time by offset-time."
  (mapcar (lambda (e)
            (let ((new-e (copy-list e)))
              (incf (getf new-e :time) (float offset-time))
              new-e))
          events))

(defun group-events-by-time (events)
  "Groups events by exact timestamp so chords stay fused vertically (Used by ZIP and LilyPond)."
  (let ((groups nil) (current-group nil) (current-time nil))
    (dolist (e (stable-sort (copy-list events) #'< :key (lambda (x) (getf x :time))))
      (if (or (null current-time) (< (abs (- (getf e :time) current-time)) 0.001))
          (progn
            (push e current-group)
            (unless current-time (setf current-time (getf e :time))))
          (progn
            (push (reverse current-group) groups)
            (setf current-group (list e))
            (setf current-time (getf e :time)))))
    (when current-group
      (push (reverse current-group) groups))
    (reverse groups)))

;; --- LILYPOND TRANSLATION LAYER ---

(defun pch->lily (pch)
  "Translates Csound pitch decimals (8.00) to LilyPond strings (c')."
  (let* ((octave (floor pch))
         (pc (round (* (- pch octave) 100)))
         ;; LilyPond uses Dutch note names (cis = C#, bes = Bb)
         (notes #("c" "cis" "d" "dis" "e" "f" "fis" "g" "gis" "a" "ais" "b"))
         (note-name (aref notes (mod pc 12)))
         ;; LilyPond Octaves: Middle C (8.00) is c'
         (octave-marks (cond ((>= octave 9) "''")
                             ((= octave 8) "'")
                             ((= octave 7) "")   ;; C3
                             ((= octave 6) ",")  ;; C2
                             ((<= octave 5) ",,") ;; C1
                             (t "'"))))
    (format nil "~A~A" note-name octave-marks)))

(defun dur->lily (dur)
  "Translates absolute beat durations into LilyPond rhythm strings."
  ;; Basic Quantizer: Snaps physics durations to nearest musical fraction
  (cond ((>= dur 4.0) "1")
        ((>= dur 3.0) "2.")
        ((>= dur 2.0) "2")
        ((>= dur 1.5) "4.")
        ((>= dur 1.0) "4")
        ((>= dur 0.75) "8.")
        ((>= dur 0.5) "8")
        ((>= dur 0.25) "16")
        (t "32")))

(defun musical-data-p (obj)
  "Checks if a result is a list of note/control plists."
  (and (listp obj) (listp (car obj)) (getf (car obj) :type)))

(defun extract-base-type (expected-sym)
  "Purely extracts the base keyword, fixing trailing hyphens from OPTIONAL tags."
  (let* ((sym-str (string-upcase (string expected-sym)))
         (pos (search "-OPTIONAL" sym-str)))
    (intern (if pos (subseq sym-str 0 pos) sym-str) "KEYWORD")))

(defun strip-comments (line)
  "Removes inline Lisp comments from a string before parsing."
  (let ((pos (position #\; line)))
    (if pos (string-trim " " (subseq line 0 pos)) (string-trim " " line))))
