(in-package :cal-coms)

(defun get-list-location (l idx)
  (cond
    ((null l)
     nil)
    ((= idx 0)
     (car l))
    (t
     (get-list-location (cdr l) (- idx 1)))))

(defun env-list (varname)
  (read-from-string (concatenate 'string "("
                                 (uiop/os:getenv varname)
                                 ")")))

(defun get-connection-pred (obj)
  (cond ((eq #\W (char obj 0))
	 'TMSMT/PDDL::CONNECTED-WIRE)
	((eq #\S (char obj 0))
	 'TMSMT/PDDL::CONNECTED-SWITCH)
	((eq #\L (char obj 0))
	 'TMSMT/PDDL::CONNECTED-LAMP)))

(defun choose-from-list (choose-list state &optional ign-element)
  (let* ((num-elements (length choose-list))
	 (ret (get-list-location choose-list (random num-elements state))))
    (loop while (equal ret ign-element)
       do (setf ret (get-list-location choose-list (random num-elements state))))
    ret))

(defun move-object (move-object move-location locations old-locs stdev)
  (with-open-file (f *update-file* :direction :output
		     :if-exists :append
		     :if-does-not-exist :create)
    (let ((tr (generate-true stdev))
	  (old-loc (gethash move-object old-locs))
	  (connection-pred (get-connection-pred move-object)))
      (format f "(~D (TMSMT/PDDL::CONNECTED TMSMT/PDDL::~a))~%" tr move-location)
      (format f "(~D (~S TMSMT/PDDL::~a))~%" tr connection-pred move-location)
      (format f "(~D (TMSMT/PDDL::HOLDING TMSMT/PDDL::~a))~%" (- 1 tr) move-object)
      (loop for loc in locations
	 do (let ((x (if (equal loc move-location)
			 tr
			 (generate-false stdev))))
	      (format f "(~D (TMSMT/PDDL::at TMSMT/PDDL::~a tmsmt/pddl::~a))~%"
		      x move-object loc)
	      (when (equal old-loc loc)
		(format f "(~D (TMSMT/PDDL::CONNECTED TMSMT/PDDL::~a))~%" x loc)
		(format f "(~D (~S TMSMT/PDDL::~a))~%" x connection-pred loc))))))
  (save-backup *update-file* "prob")
  (format t "~a ~a~%" move-object move-location)
  (setf (gethash move-object old-locs) move-location)
  (sns::write-sns-msg-text *change* (format nil "reparent ~a ~a" move-object move-location)))


(defun add-object (copy-obj new-loc locations num-objects stdev)
  (let* ((object-type (car (string-to-list copy-obj #/_)))
	 (new-object (format nil "~a_~D" object-type (+ 1 num-objects)))
	 (true-prob (generate-true stdev))
	 (connection-pred (get-connection-pred copy-obj))
	 new-fluents)

    (push (format nil "(~D (TMSMT/PDDL::CONNECTED TMSMT/PDDL::~a))"
		  true-prob new-loc)
	  new-fluents)
    (push (format nil "(~D (~S TMSMT/PDDL::~a))"
		  true-prob connection-pred new-loc)
	  new-fluents)

    (push (format nil "(~D (tmsmt/pddl::holding tmsmt/pddl::~a))" (generate-false stdev) new-object)
	  new-fluents)

    (loop for location in locations
       do (let ((x (if (equal new-loc location)
		       (generate-true stdev)
		       (generate-false stdev))))
	    (push (format nil "(~D (TMSMT/PDDL::AT TMSMT/PDDL::~a TMSMT/PDDL::~a))"
			  x new-object location)
		  new-fluents)))


    (with-open-file (f *update-file* :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
      (let* ((new-string (cons (format nil "tmsmt/pddl::~a" object-type)
			       (list new-fluents)))
	     (new-string (cons (format nil "tmsmt/pddl::~a" new-object) new-string))
	     (new-string (cons 'add new-string)))
	(format f "~a~%" new-string)))

    (save-backup *update-file* "prob")
    (format t "added ~a at ~a~%" new-object new-loc)
    (sns::write-sns-msg-text *change* (format nil "add ~a ~a ~a" new-object new-loc copy-obj))
    new-object))

(defun remove-object (obj)
  (with-open-file (f *update-file* :direction :output
		     :if-exists :append
		     :if-does-not-exist :create)
    (format f "(remove TMSMT/PDDL::~a)~%" obj))
  (save-backup *update-file* "prob")
  (format t "removed ~a.~%" obj)
  (sns::write-sns-msg-text *change* (format nil "remove ~a" obj)))



(defun sim-disturber ()
   (let* ((sg (robray::scene-graph (env-list "TMSMT_SCENE_FILES")))
	  (stdev (or (uiop/os:getenv "TMSMT_STDEV")
		     "0.25"))
	  (stdev (read-from-string stdev))
	  (step  (or (uiop/os:getenv "TMSMT_DISTURB_STEP")
		     "1"))
	  (step  (read-from-string step))
	  (move-likelihood (or (uiop/os:getenv "TMSMT_MOVE")
			       "0"))
	  (move-likelihood (read-from-string move-likelihood))
	  (add-likelihood (or (uiop/os:getenv "TMSMT_ADD")
			      "0"))
	  (add-likelihood (read-from-string add-likelihood))
	  (rem-likelihood (or (uiop/os:getenv "TMSMT_REM")
			      "0"))
	  (rem-likelihood (read-from-string rem-likelihood))
	  (total-likelihood (+ move-likelihood add-likelihood rem-likelihood))
	  (locations (remove "LOCATION_A" (find-frame-type sg "location") :test 'equal))
	  (objs (find-frame-type sg "moveable"))
	  (last-obj ())
	  (old-locs (make-hash-table :test 'equal))
	  (counter 0)
	  (state (make-random-state t)))
     (format t "scenegraph created~%")
     (format t "locs: ~S~%" locations)
     (format t "objs: ~S~%" objs)
     (initialize)
     (loop while t
	do (sleep step)
	do (loop-over-text-channel *action* (lambda (str r)
					      (declare (ignore r))
					      (let* ((string-list (string-to-list str #\space))
						     (obj (cadr   string-list))
						     (loc (caddr  string-list)))
						(setf last-obj obj)
						(setf (gethash obj old-locs) loc)
						(format t "location of ~A was ~A~%" obj loc)))
	      :wait nil :last nil)
	do (let ((chosen-obj (choose-from-list objs state last-obj))
		 (chosen-loc (choose-from-list locations state))
		 (choice     (random total-likelihood state)))
	     (format t "random choice: ~D~%" choice)
	     (cond
	       ((< choice add-likelihood)
		(push (add-object chosen-obj chosen-loc locations counter stdev) objs)
		(incf counter))
	       ((< choice (+ add-likelihood move-likelihood))
		(move-object chosen-obj chosen-loc locations old-locs stdev))
	       (t
		(remove-object chosen-obj)
		(setf objs (remove chosen-obj objs))))))))
