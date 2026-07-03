;; core/evaluator.lisp
(in-package :bogu)

;; =============================================================================
;; AST NODE PREDICATES
;; =============================================================================

(defun is-variable-p (node) 
  "Checks both user memory and the Standard Library ROM."
  (and (symbolp node) (or (gethash node *vars*) (gethash node *stdlib-vars*))))

(defun is-note-node-p (node)
  (or (and (symbolp node) (note-p node))
      (and (listp node) 
           (symbolp (car node)) 
           (note-p (car node))
           (cadr node)
           (rtm (cadr node))))) 

(defun is-rest-p (node) 
  (or (and (symbolp node) (member node '(RST R)))
      (and (listp node) 
           (member (car node) '(RST R))
           (cadr node)
           (rtm (cadr node))))) 

(defun is-command-p (node)
  (and (listp node) (symbolp (car node))
       (or (gethash (car node) *command-dictionary*)
           (gethash (car node) *rewrite-rules*))))

;; =============================================================================
;; AST DISPATCHERS
;; =============================================================================

(defun eval-variable (node)
  "Evaluates the stored AST block from either memory bank."
  (execute-ast (or (gethash node *vars*) (gethash node *stdlib-vars*))))

(defun eval-rest (node)
  (let ((dur (if (and (listp node) (cadr node)) (rtm (cadr node)) 1.0)))
    (list (make-rest-event 0.0 dur))))

(defun eval-note (node)
  (let* ((sym (if (listp node) (car node) node))
         (dur (if (and (listp node) (cadr node)) (rtm (cadr node)) 1.0)))
    (multiple-value-bind (pitch-sym octave) (parse-note-symbol sym)
      (list (make-note-event pitch-sym 0.0 dur :octave octave)))))

(defun eval-command (node)
  (let ((cmd-entry (gethash (car node) *command-dictionary*))
        (rule-entry (gethash (car node) *rewrite-rules*)))
    (cond
      (rule-entry (apply-rewrite-rule node))
      (cmd-entry  (funcall (getf cmd-entry :fn) (cdr node))))))

;; =============================================================================
;; THE SYMBOLIC REDUCER
;; =============================================================================

(defun execute-ast (ast)
  "Phase 3: The Symbolic Reducer. Resolves variables and commands into pure music data."
  (let ((master-events nil)
        (local-cursor 0.0)
        ;; Safely detect if the AST itself is a single command node that needs wrapping
        (nodes-to-process 
         (cond 
           ((null ast) nil)
           ((atom ast) (list ast)) ;; <--- THE FIX: Safely wrap naked symbols!
           ((and (listp ast) 
                 (or (is-command-p ast) (is-rest-p ast) (is-note-node-p ast)))
            (list ast))
           (t ast))))
    
    (dolist (node nodes-to-process)
      (let ((result 
             (cond
               ((is-variable-p node) (eval-variable node))
               ((is-rest-p node)     (eval-rest node))
               ((is-note-node-p node)(eval-note node))
               ((is-command-p node)  (eval-command node))
               ((listp node)         (execute-ast node))
               (t (format t "~%[Compiler Error] Unrecognized AST node: ~A~%" node) nil))))

        ;; --- THE STITCHER ---
        (when (musical-data-p result) 
          (let ((block-len 0.0))
            ;; Measure the true footprint of the block
            (dolist (e result)
              (setf block-len (max block-len (+ (getf e :time) (or (getf e :written-dur) 0.0)))))
            
            ;; Use our new utility to shift time, and push to master stream
            (dolist (e (shift-events result local-cursor))
              (push e master-events))
            
            (incf local-cursor block-len)))))
            
    (reverse master-events)))

(defun execute-node (node)
  "Phase 3: The Engine. Pure Dictionary Dispatcher."
  (when (null node) (return-from execute-node t))
  (let* ((cmd (car node))
         (args (cdr node))
         (var-body (or (and (symbolp cmd) (gethash cmd *vars*))
                       (and (symbolp cmd) (gethash cmd *stdlib-vars*))))
         (rule-body (and (symbolp cmd) (gethash cmd *rewrite-rules*)))
         (dict-entry (and (symbolp cmd) (gethash cmd *command-dictionary*))))
    (cond
      ((listp cmd) (execute-ast node) t)
      (rule-body (apply-rewrite-rule node) t)
      (dict-entry (funcall (getf dict-entry :fn) args) t)
      (var-body (execute-ast var-body) t)
      (t (format t "~%[Compiler Error] Unknown command or variable: ~A~%" cmd)))))

(defun commit (ast &optional (instr-override nil))
  "Phase 4: The Committer. Glues musical data to the global timeline."
  (let ((events (execute-ast ast))
        (track-mins (make-hash-table))
        (track-maxs (make-hash-table)))
    
    (when events 
      ;; Phase 1: Measure the true local timeline bounds per track
      (dolist (e events)
        (let* ((event-instr (or instr-override (getf e :instr) *current-instrument*))
               (start-time (getf e :time))
               (end-time (+ start-time (or (getf e :written-dur) (getf e :dur) 0.0))))
          (setf (gethash event-instr track-mins) 
                (min start-time (or (gethash event-instr track-mins) start-time)))
          (setf (gethash event-instr track-maxs) 
                (max end-time (or (gethash event-instr track-maxs) end-time)))))

      ;; Phase 2: Map events to their respective independent track playheads
      (dolist (e events)
        (let* ((event-instr (or instr-override (getf e :instr) *current-instrument*))
               (trk (ensure-track event-instr)) ; <--- THE FIX: Guarantee the track exists!
               (trk-start (track-playhead trk))
               (min-t (gethash event-instr track-mins))
               ;; Normalize the event relative to when this specific track started its block
               (abs-t (+ trk-start (- (getf e :time) min-t))))
          
          (cond
            ;; 2A. PROCESS NOTES
            ((and (eq (getf e :type) :note)
                  (not (eq (getf e :pitch-symbol) 'RST))
                  (not (eq (getf e :pitch-symbol) 'R)))
             (let ((pitch-sym (getf e :pitch-symbol))
                   (explicit-octave (getf e :octave))
                   (ast-trans (or (getf e :transpose) 0)))
               (multiple-value-bind (p calc-oct) (calculate-diatonic-pitch (cdr (assoc pitch-sym *notes*)) ast-trans trk)
                 (let* ((octave-shift (- calc-oct 4))
                        (final-octave (+ explicit-octave octave-shift))
                        (p-dur (or (getf e :dur) (getf e :written-dur))))
                   
                   (push (list :type :note :instr event-instr
                               :time abs-t :dur p-dur :pitch p :octave final-octave
                               :pch (+ final-octave 4 (/ p 100.0)) :vel (track-velocity trk))
                         *score*)))))
                         
            ;; 2B. PROCESS CONTROLS 
            ((eq (getf e :type) :control)
             (let ((new-c (copy-list e)))
               (setf (getf new-c :instr) event-instr)
               (setf (getf new-c :time) abs-t) 
               (push new-c *score*)))

            ;; 2C. PROCESS META
            ((eq (getf e :type) :meta)
             (let ((new-m (copy-list e)))
               (setf (getf new-m :instr) event-instr)
               (setf (getf new-m :time) abs-t)
               (push new-m *score*))))))
      
      ;; Phase 3: Advance each active track's playhead independently
      (maphash (lambda (instr-id max-t)
                 (let* ((min-t (gethash instr-id track-mins))
                        (delta (- max-t min-t))
                        (trk (ensure-track instr-id)))
                   (incf (track-playhead trk) delta)))
               track-maxs))))
