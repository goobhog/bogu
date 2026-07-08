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
        (*package* (find-package :bogu)))
    (with-input-from-string (stream (string-upcase input-string))
      (loop for token = (read stream nil 'eof)
            until (eq token 'eof)
            do (push token tokens)))
    (reverse tokens)))

(defun is-rhythm-token-p (token)
  "Checks if a token represents a Bogu rhythm value (like q, e, s.)."
  (and (symbolp token) (rtm token)))

(defun cmd-is-full-p (cmd-list next-token)
  "Smart Arity Checker: Determines if the accumulated command has all its MAXIMUM arguments."
  (if (null cmd-list)
      nil
      (let* ((cmd-sym (car cmd-list))
             (arg-count (1- (length cmd-list)))
             (entry (and (symbolp cmd-sym) (gethash cmd-sym *command-dictionary*))))
        (cond
          ;; 1. Built-in Commands (Check max signature arity)
          (entry
           (let* ((sig (getf entry :sig))
                  (rest-pos (position '&rest sig))
                  (max-args (if rest-pos most-positive-fixnum (length sig))))
             (>= arg-count max-args)))
          ;; 2. Notes and Rests (They take 0 args, OR exactly 1 if the next token is a rhythm)
          ((or (and (symbolp cmd-sym) (note-p cmd-sym))
               (eq cmd-sym 'RST) (eq cmd-sym 'R))
           (if (= arg-count 0)
               (not (is-rhythm-token-p next-token)) ; Full if the next token is NOT a rhythm
               t)) ; If it already has 1 arg (the rhythm), it is completely full
          ;; 3. L-System Rewrite Rules
          ((and (symbolp cmd-sym) (gethash cmd-sym *rewrite-rules*))
           (let* ((rule (gethash cmd-sym *rewrite-rules*))
                  (vars (getf rule :vars)))
             (>= arg-count (length vars))))
          ;; 4. User Variables and all other standalone symbols (Take exactly 0 arguments)
          (t
           (>= arg-count 0))))))

(defun cmd-min-satisfied-p (cmd-list)
  "Checks if the accumulated command has satisfied its MINIMUM required arguments."
  (if (null cmd-list)
      t
      (let* ((cmd-sym (car cmd-list))
             (arg-count (1- (length cmd-list)))
             (entry (and (symbolp cmd-sym) (gethash cmd-sym *command-dictionary*))))
        (cond
          ;; Built-in Commands
          (entry
           (let* ((sig (getf entry :sig))
                  (rest-pos (position '&rest sig))
                  (required-sigs (if rest-pos (subseq sig 0 rest-pos) sig))
                  (min-args (count-if-not (lambda (s) (search "-OPTIONAL" (string-upcase (string s)))) required-sigs)))
             (>= arg-count min-args)))
          ;; Notes and Rests
          ((or (and (symbolp cmd-sym) (note-p cmd-sym))
               (eq cmd-sym 'RST) (eq cmd-sym 'R))
           t) ;; min 0 args
          ;; Rewrite Rules
          ((and (symbolp cmd-sym) (gethash cmd-sym *rewrite-rules*))
           (let* ((rule (gethash cmd-sym *rewrite-rules*))
                  (vars (getf rule :vars)))
             (>= arg-count (length vars))))
          ;; Variables and unknown symbols
          (t t)))))

(defun is-bogu-command-p (token)
  "Checks if a symbol is a structural command that should force a boundary.
   Notes, Rests, AND Rhythms are deliberately EXCLUDED."
  (and (symbolp token)
       (not (rtm token)) ; Rhythms are pure data, never commands!
       (or (gethash token *command-dictionary*)
           (gethash token *rewrite-rules*))))

(defun parse-bogu-tokens (tokens &optional current-cmd ast)
  "Arity-Aware Parser. Intelligently groups symbols based on their command signatures."
  (if (null tokens)
      (reverse (if current-cmd (cons (reverse current-cmd) ast) ast))
      (let ((token (car tokens)))
        (cond
          ;; 1. Smart Boundary Detection (MOVED ABOVE BRACKET LOGIC)
          ;; Split if the current command is FULL, OR if the token is a known command 
          ;; AND the current command has already satisfied its minimum required arity.
          ((and current-cmd 
                (let* ((rev-cmd (reverse current-cmd))
                       (cmd-sym (car rev-cmd)))
                  (or (cmd-is-full-p rev-cmd token)
                      (and (is-bogu-command-p token)
                           (cmd-min-satisfied-p rev-cmd)
                           (not (member cmd-sym '(HELP VARS SAVE LOAD)))))))
           ;; THE FIX: Split the AST and leave 'token' in the stream for the next recursion pass.
           ;; This prevents greedy commands from accidentally eating adjacent bracket blocks!
           (parse-bogu-tokens tokens
                              nil
                              (cons (reverse current-cmd) ast)))
                              
          ;; 2. Bracket blocks (Recursive execution boundary)
          ((listp token)
           (let* ((parsed-sub (parse-bogu-tokens token))
                  (clean-sub (if (= (length parsed-sub) 1) (car parsed-sub) parsed-sub)))
             (parse-bogu-tokens (cdr tokens)
                                (cons clean-sub current-cmd)
                                ast)))
                                
          ;; 3. Argument Accumulation
          (t
           (parse-bogu-tokens (cdr tokens)
                              (cons token current-cmd)
                              ast))))))
