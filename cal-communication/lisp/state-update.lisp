(in-package :cal-coms)


(defun compile-action (string-action &optional (start 0))
  (let* ((pos (position #\SPACE string-action :start start))
	 (word (subseq string-action start pos)))
    (if pos
	(cons (intern (string-upcase word) 'tmsmt/pddl)
	      (compile-action string-action (+ pos 1)))
	(list (intern (string-upcase word) 'tmsmt/pddl)))))

(defun find-action (action-name action-list)
  (cond
    ((null action-list)
     nil)
    ((eq (tmsmt::pddl-action-name (car action-list))
	 action-name)
     (car action-list))
    (t
     (find-action action-name (cdr action-list)))))

(defun process-effect (exp &optional (prob 1))
  (cond
    ((eq (car exp) 'not)
     (process-effect (cadr exp) (if (= prob 1)
				0
				1)))
    ((or (eq (car exp) 'and)
	 (eq (car exp) 'or))
     (loop for x in (cdr exp)
	collect (process-effect x prob)))
    (t
     (cons exp prob))))



(defun read-action-channel (feedback-planner)
  (let* ((val-hash (tmsmt::probability-calculator-value-hash
		    (tmsmt::feedback-planner-probability-calculator feedback-planner)))
	 (r :ok)
	 (ret nil))
    (loop while (or (eq r :ok)
		    (eq r :missed-frame))
	 do (multiple-value-bind (new-r action-string)
		(sns::read-sns-msg-text *action* :wait nil :last nil)
	      (setf r new-r)
	      (when (or (eq r :ok)
			(eq r :missed-frame))
		(when (tmsmt::feedback-planner-trace feedback-planner)
		  (format t "~%Status: ~a. String: ~a~%" r action-string)
		  (format t "Time: ~s.~%" (string-time))
		  (format t "Compiled actions ~S.~%" (compile-action action-string)))
		(setf ret t)
		(let* ((action (compile-action action-string))
		       (generic-actions (tmsmt::pddl-operators-actions
					 (tmsmt::feedback-planner-operator feedback-planner)))
		       (pddl-action (find-action (car action) generic-actions))
		       (arg-alist (tmsmt::exp-args-alist (tmsmt::pddl-action-parameters pddl-action)
							 (cdr action)))
		       (effect (sublis arg-alist (tmsmt::pddl-action-effect pddl-action)))
		       (update-alist (process-effect effect)))
		  (loop for (fluent . prob) in update-alist
		     do (setf (gethash (cons 0 fluent) val-hash) prob))))))
    ret))

(defun read-state-file (feedback-planner)
  (let* ((val-hash (tmsmt::probability-calculator-value-hash
		    (tmsmt::feedback-planner-probability-calculator feedback-planner)))
	 (f (open *update-file* :if-does-not-exist nil))
	 state-add
	 state-rem
	 ret
	 (string-arg "("))
    (when f
      (loop for line = (read-line f nil)
	 while line
	 do (setf string-arg (concatenate 'string string-arg line)))
      (setf string-arg (concatenate 'string string-arg ")"))
      (loop for symb-list in (read-from-string string-arg)
	 do (cond
	      ((numberp (car symb-list)) ;Probability of a fluent being true
	       (setf (gethash (cons 0 (cadr symb-list)) val-hash) (car symb-list))
	       (setf ret t))
	      ((eq 'cl-user::add (car symb-list)) ;Adding a new object
	       (format t "adding new object ~S~%" (cadr symb-list))
	       (push (cdr symb-list) state-add)
	       (setf ret t))
	      ((eq 'remove (car symb-list)) ;Removing an object
	       (format t "removing ~S~%" (cadr symb-list))
	       (push (cadr symb-list) state-rem)
	       (setf ret t))))
      (when (or state-add state-rem)
	(tmsmt::make-state-changes feedback-planner state-add state-rem))
      (delete-file f)
      (close f))
    ret))

(defun read-multiple (feedback-planner)
  (let* ((x (read-action-channel feedback-planner))
	 (y (read-state-file feedback-planner)))
    (values (or x y) y)))
