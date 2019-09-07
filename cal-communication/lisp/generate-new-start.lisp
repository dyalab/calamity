(in-package :cal-coms)

(defun generate-new-start (operators facts  &optional (stdev .25))
  (let* ((ground    (tmsmt::ground-domain operators facts))
	(ret
	 (loop for fluent in (cdr (tmsmt::ground-domain-start ground))
	    collect (let ((x (if (eq (car fluent) 'not)
			    (generate-false stdev)
			    (generate-true stdev)))
		     (y (if (eq (car fluent) 'not)
			    (cadr fluent)
			    fluent)))
		      (format nil "(probabilistic ~D ~A)" x y)))))
    (format t "~A~%" ret)
    (with-open-file (f "./prob-start.pddl" :direction :output
		       :if-exists :supersede :if-does-not-exist :create)
      (format f "~A~%" ret))))
