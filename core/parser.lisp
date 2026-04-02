;; core/parser.lisp
(in-package :bogu)

;; 1. The Global Syntax Macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\[
    (lambda (stream char)
      (declare (ignore char))
      (read-delimited-list #\] stream t)))

  (set-macro-character #\]
    (lambda (stream char)
      (declare (ignore stream char))
      (error "Unmatched close bracket ]"))))

(defun lex-bogu-string (input-string)
  "Phase 1: Pure Lexer. Reads text directly into BOGU symbols."
  (let ((tokens nil)
        ;; THE FIX: Explicitly bind the package context during reading
        (*package* (find-package :bogu)))
    (with-input-from-string (stream (string-upcase input-string))
      (loop for token = (read stream nil 'eof)
            until (eq token 'eof)
            do (push token tokens)))
    (reverse tokens)))

(defun parse-bogu-tokens (tokens &optional current-cmd ast)
  "Phase 2: State Machine Parser. Intelligently groups symbols into an AST."
  (if (null tokens)
      ;; End of the line: push the final command and return the tree
      (reverse (if current-cmd (cons (reverse current-cmd) ast) ast))
      
      (let ((token (car tokens)))
        (cond
          ;; 1. It's a nested bracket block! Recursively parse it.
          ((listp token)
           (let ((parsed-sub (parse-bogu-tokens token)))
             ;; Prevent double-parentheses unwrapping
             (let ((clean-sub (if (= (length parsed-sub) 1) (car parsed-sub) parsed-sub)))
               (parse-bogu-tokens (cdr tokens)
                                  (cons clean-sub current-cmd)
                                  ast))))
                                  
          ;; 2. We hit a known Command! Start a new AST node.
          ((and current-cmd 
		(symbolp token)
		(not (and (= (length current-cmd) 1) 
			  (member (car current-cmd) '(HELP SWEEP DEF LIVE-LOOP STOP-LOOP ENGRAVE RETRO TRANSPOSE INVERT STACCATO CHANCE CHOOSE))))
		(or (gethash token *command-dictionary*)
		    ;; REMOVED *vars* and *stdlib-vars* here!
		    (member token '(DEF I KEY BPM PLAY SAVE VARS WHERE HELP RESET LOAD DEL SEEK SYNC BANG SYNTH WAIT IF))))
           ;; Close the current command, push to AST, and start a new one with this token
           (parse-bogu-tokens (cdr tokens)
                              (list token)
                              (cons (reverse current-cmd) ast)))
          
          ;; 3. It's just an argument (note, rhythm, math). Accumulate it.
          (t
           (parse-bogu-tokens (cdr tokens)
                              (cons token current-cmd)
                              ast))))))
