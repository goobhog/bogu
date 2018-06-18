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
	((eq 'ht rval) (/ 4.0 3.0))))
