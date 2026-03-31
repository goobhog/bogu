;; parser.lisp
(in-package :bogu)

;; 1. The Global Syntax Macros
;; CRITICAL: This must remain global (eval-when) so the Lisp compiler 
;; can natively read embedded Bogu code in your tests and REPL!
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
  "Phase 1: The Lexer. Tokenizes a raw string into Bogu symbols."
  (let* ((str input-string)
         ;; THE FIX: We revert to your original regex for newlines.
         ;; It safely preserves newlines so Lisp comments (;) terminate 
         ;; correctly, while still injecting the '&' command separator!
         (str (cl-ppcre:regex-replace-all "\\n" str (format nil "~% & ~%")))
         
         ;; Pad explicit ampersands so they don't attach to symbols
         (str (cl-ppcre:regex-replace-all "&" str " & "))
         
         ;; Force uppercase to align with Lisp's internal symbol registry
         (str (string-upcase str))
         (tokens nil))
    
    (with-input-from-string (stream str)
      (loop for token = (read stream nil 'eof)
            until (eq token 'eof)
            ;; Filter out consecutive ampersands to prevent empty commands
            do (unless (and (eq token '&) (eq (car tokens) '&))
                 (push token tokens))))
    
    (reverse tokens)))

(defun parse-bogu-tokens (tokens)
  "Phase 2: The Parser. Slices a flat list of tokens by '&' into an Abstract Syntax Tree."
  (let ((ast nil)
        (current-cmd nil))
    (dolist (token tokens)
      (cond
        ;; 1. If we hit an ampersand, finish the current command
        ((eq token '&)
         (when current-cmd
           (push (reverse current-cmd) ast)
           (setf current-cmd nil)))
        
        ;; 2. If we hit a nested list, recursively parse it!
        ((listp token)
         (let ((parsed-sub (parse-bogu-tokens token)))
           ;; The unwrapping fix: prevents double-parentheses in combinatoric blocks
           (if (= (length parsed-sub) 1)
               (push (car parsed-sub) current-cmd)
               (push parsed-sub current-cmd))))
        
        ;; 3. Otherwise, just add the token
        (t
         (push token current-cmd))))
         
    ;; 4. Push the final command
    (when current-cmd
      (push (reverse current-cmd) ast))
      
    ;; 5. Return the finalized Tree
    (reverse ast)))
