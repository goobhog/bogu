(defun all-positions (item list)
  "Returns a new list of all positions an item appears in a list."
  (let ((l nil))
    (dotimes (i (length list))
      (if (equal item (elt list i))
	  (push i l)))
    (nreverse l)))

(defun flatten (l)
  "Removes parentheses from a multi-dimensional list."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun fn-it (fn)
  "Calls function on an item."
  (list 'function fn))

(defun quote-it (p)
  "Quotes an item."
  (list 'quote p))

(defun stringem (&rest items)
  "Adjoins items as one string."
  (format nil "~{~a~^~}" items))

(defun bogu-folder (name)
  (ensure-directories-exist (stringem 'bogu/compositions/ name #\/)))

(defun rtm (rval)
  "Returns rhythm quantity for corresponding rhythm symbol. If given a number, it simply returns that number."
  (cond ((numberp rval) rval)
	((eq 'q rval) 1.0)
	((eq 'e rval) 0.5)
	((eq 's rval) 0.25)
	((eq 'h rval) 2.0)
	((eq 'w rval) 4.0)
	((eq 't rval) 0.125)
	((eq 'q. rval) 1.5)
	((eq 'q.. rval) 1.75)
	((eq 'e. rval) 0.75)
	((eq 'e.. rval) 0.875)
	((eq 's. rval) 0.375)
	((eq 's.. rval) 0.4375)
	((eq 'h. rval) 3.0)
	((eq 'h.. rval) 3.5)
	((eq 'qt rval) (/ 2.0 3.0))
	((eq 'et rval) (/ 1.0 3.0))
	((eq 'st rval) (/ 0.5 3.0))
	((eq 'tt rval) (/ 0.25 3.0))
	((eq 'ht rval) (/ 4.0 3.0))
	((eq 'qq rval) (/ 2.0 5.0))
	((eq 'eq rval) (/ 1.0 5.0))
	((eq 'sq rval) (/ 0.5 5.0))
	((eq 'tq rval) (/ 0.25 5.0))
	((eq 'hq rval) (/ 4.0 5.0))))

(defun bogu-reader (code)
  "Formats bogu code for lisp reader."
  (let ((cmd (read-from-string
	      (concatenate 'string "(" code ")"))))
    (cond ((or (and (eql (car cmd) 'chord)
		    (member (cdr cmd) *allowed-commands*))
	       (and (eq (car cmd) 'seq)
		    (member (cdr cmd) *allowed-commands*)))
	   (append (list (car cmd) (quote-it (cadr cmd)))
		   (mapcar #'fn-it (cddr cmd))))
	  ((and (eql (car cmd) 'sarp)
		(member (cddr cmd) *allowed-commands*))
	   (append (list (elt cmd 0)
			 (quote-it (elt cmd 1))
			 (quote-it (elt cmd 2)))
		   (mapcar #'fn-it (cdddr cmd))))
	  ((eq (car cmd) 'load) (append '(bogu-load) (cdr cmd)))
	  (t (cons (car cmd) (mapcar #'quote-it (cdr cmd)))))))
