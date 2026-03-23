;; parser.lisp
(in-package :bogu)

;; 0. Teach Lisp our custom Bogu syntax characters
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
  "Phase 1: The Lexer. Safely tokenizes a raw string into Bogu symbols."
  (let* ((str input-string)
         ;; Pad syntax symbols so they stand alone as distinct tokens
         (str (cl-ppcre:regex-replace-all "\\[" str " [ "))
         (str (cl-ppcre:regex-replace-all "\\]" str " ] "))
         (str (cl-ppcre:regex-replace-all "&" str " & "))
         ;; Force uppercase to align with Lisp's internal symbol registry
         (str (string-upcase str)))
    
    (with-input-from-string (stream str)
      (loop for token = (read stream nil 'eof)
            until (eq token 'eof)
            collect token))))

(defun parse-bogu-tokens (tokens)
  "Phase 2: The Parser. Slices a flat list of tokens by '&' into an Abstract Syntax Tree."
  (let ((ast nil)
        (current-cmd nil))
    (dolist (token tokens)
      (cond
        ;; 1. If we hit an ampersand, finish the current command and push it to the AST
        ((eq token '&)
         (when current-cmd
           (push (reverse current-cmd) ast)
           (setf current-cmd nil)))
        
        ;; 2. If we hit a nested list (a bracketed block), recursively parse it!
        ((listp token)
         (push (parse-bogu-tokens token) current-cmd))
        
        ;; 3. Otherwise, just add the token to the current command
        (t
         (push token current-cmd))))
         
    ;; 4. Don't forget to push the final command after the loop finishes!
    (when current-cmd
      (push (reverse current-cmd) ast))
      
    ;; 5. Return the finalized Tree
    (reverse ast)))
