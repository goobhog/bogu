;; project.lisp
(in-package :bogu)

(defun strip-comments (line)
  "Removes inline Lisp comments from a string before parsing."
  (let ((pos (position #\; line)))
    (if pos (string-trim " " (subseq line 0 pos)) (string-trim " " line))))

(defun save (&optional filename)
  "Saves the project, prompting for a name if none exists."
  (when filename
    (setf *current-project* (string-downcase (string filename))))
  
  (unless *current-project*
    (format t "Enter a name for this new project: ")
    (finish-output)
    (let ((name (read-line)))
      (if (string= name "")
          (progn (format t "Save cancelled.~%") (return-from save))
          (setf *current-project* name))))
  
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
                   (member cmd '(play save help vars where bogu-load load start-audio-engine)))
               nil)
              (t (format out "~a~%" line)))))))
    (format t "saved \"compositions/~a/~a.bogu\"~%" fname fname)))

(defun bogu-load (&optional filename)
  "Loads a project using the new Lexer -> Parser -> AST Compiler Pipeline."
  (when *score*
    (format t "Save your current project before loading a new one? (y/n): ")
    (finish-output)
    (let ((ans (read-line)))
      (when (string= (string-downcase ans) "y")
        (save))))

  (let ((target (if filename 
                    (string-downcase (string filename))
                    (progn
                      (format t "Enter project name to load: ")
                      (finish-output)
                      (read-line)))))
    
    (when (not (string= target ""))
      (reset-bogu)
      (setf *current-project* target)
      (with-open-file (in (comp-path target (bogu-folder target) "bogu")
                          :direction :input
                          :if-does-not-exist nil)
        (if in
            (let ((input-string ""))
              (loop for raw-line = (read-line in nil)
                    while raw-line do
                      ;; 1. Strip inline comments first!
                      (let ((trimmed-line (strip-comments raw-line)))
                        ;; 2. Only process if there's actual code left
                        (unless (string= trimmed-line "")
                          (push raw-line *bogu-code*) ;; Preserve raw line for saving
                          
                          ;; 3. Smart Semicolon Insertion
                          (let* ((trimmed-prev (string-trim " " input-string))
                                 (last-char-prev (if (> (length trimmed-prev) 0) 
                                                     (char trimmed-prev (1- (length trimmed-prev))) 
                                                     #\Space))
                                 (first-char-next (if (> (length trimmed-line) 0) 
                                                      (char trimmed-line 0) 
                                                      #\Space))
                                 (separator (if (or (string= input-string "")
                                                    (char= last-char-prev #\[)
                                                    (char= first-char-next #\[)
                                                    (char= first-char-next #\]))
                                                " "      ; <-- Keep arguments attached!
                                                " & "))) ; <-- Safely separate commands!
                            (setf input-string (concatenate 'string input-string separator trimmed-line))
                            
                            ;; 4. Evaluate if brackets are balanced
                            (when (= (count #\[ input-string) (count #\] input-string))
                              (handler-case
                                  (let* ((tokens (lex-bogu-string input-string))
                                         (ast (parse-bogu-tokens tokens)))
                                    (when ast 
                                      (commit ast)))
                                (error (e) 
                                  (format t "~%[Compiler Error] Could not parse block in file.~%Details: ~A~%" e)))
                              (setf input-string "")))))))
            (format t "Error: File ~a.bogu not found.~%" target)))
      (format t "loaded \"compositions/~a/~a.bogu\"~%" target target))))

(defun load-stdlib ()
  "Silently flashes the Standard Library into *stdlib-vars* ROM."
  (let ((stdlib-path (comp-path "stdlib" (bogu-folder "stdlib") "bogu")))
    (with-open-file (in stdlib-path :direction :input :if-does-not-exist nil)
      (when in
        (let ((input-string ""))
          (loop for raw-line = (read-line in nil)
                while raw-line do
                  (let ((trimmed-line (strip-comments raw-line)))
                    (unless (string= trimmed-line "")
                      (let* ((trimmed-prev (string-trim " " input-string))
                             (last-char-prev (if (> (length trimmed-prev) 0) 
                                                 (char trimmed-prev (1- (length trimmed-prev))) 
                                                 #\Space))
                             (first-char-next (if (> (length trimmed-line) 0) 
                                                  (char trimmed-line 0) 
                                                  #\Space))
                             (separator (if (or (string= input-string "")
                                                (char= last-char-prev #\[)
                                                (char= first-char-next #\[)
                                                (char= first-char-next #\]))
                                            " " 
                                            " & ")))
                        (setf input-string (concatenate 'string input-string separator trimmed-line))
                        (when (= (count #\[ input-string) (count #\] input-string))
                          (handler-case
                              (let* ((tokens (lex-bogu-string input-string))
                                     (ast (parse-bogu-tokens tokens)))
                                (when ast 
                                  (with-open-stream (*standard-output* (make-broadcast-stream))
                                    (let ((*vars* *stdlib-vars*))
                                      (execute-ast ast)))))
                            (error (e) nil))
                          (setf input-string "")))))))))))

(defun reset-bogu ()
  "Resets all global variables to default, kills loops, and reloads the Standard Library."
  (maphash (lambda (k v)
             (when (and v (sb-thread:thread-alive-p v))
               (sb-thread:terminate-thread v)))
           *loop-threads*)
  (clrhash *loop-threads*)
  (clrhash *live-loops*)
  (setf *master-epoch* nil)
  (bpm 60)
  (setf *current-instrument* 1)
  (setf *score* '())
  (clrhash *tracks*)
  (setf *bogu-code* '())
  (clrhash *vars*)        
  (setf *current-project* nil)
  (setf *transpose-offset* 0)
  (setf *velocity* 0.8)
  
  (load-stdlib)
  (format t "~%[SYSTEM] Memory wiped. Standard Library online.~%"))

(defun reset ()
  "Allows the 'reset' command to be evaluated directly from a .bogu script."
  (reset-bogu))
