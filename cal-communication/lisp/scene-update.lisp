(in-package :cal-coms)

(defun string-to-list (str char &optional (start 0))
  (let ((pos (position char str :start start)))
    (if pos
	(let ((word (subseq str start pos)))
	  (cons word (string-to-list str char (+ 1 pos))))
	(list (subseq str start)))))




(defun read-new-scene (scene q)
  (let ((scene scene))

    ;; Update the start state
    (multiple-value-bind (ptr r frame-size)
	(ach::get-foreign-alloc *state* :wait nil :last t)
      (declare (ignore frame-size))
      (when (or (eq r :ok)
		(eq r :missed-frame))
	(let* ((n-q (tmsmt::tree-map-count q))
	       (motor-arr (foreign-alloc :double :count n-q))
	       (i 0))
	  (sns::fill-state n-q (null-pointer) (sns::sns-msg-motor-state-pos ptr)
			   (sns::sns-msg-motor-state-incpos ptr)
			   motor-arr)
	  (tmsmt::map-tree-map :inorder nil (lambda (k v)
				       (declare (ignore v))
				       (setf (tmsmt::tree-map-find q k)
					     (mem-aref motor-arr :double i))
				       (incf i))
			q))))

    ;; Reparent if necessary
    (let ((locations (nconc (list "robotiq_gripper" "end_effector_grasp")
			    (find-frame-type scene "location")))
	  (objects   (find-frame-type scene "stackable")))
      (loop-over-text-channel *change*
	 (lambda (str r)
	   (destructuring-bind (action frame &rest args)
	       (string-to-list str #\space)
	     (cond
	       ((equal action "reparent")
		(let ((parent (car args)))
		  (format t "Reparenting: ~a to ~a with status ~a~%"
			  frame parent r)
		  (setf scene (tmsmt::scene-graph-reparent
			       scene parent frame :tf amino::+tf-ident+))))
	       ((equal action "add")
		(let ((parent     (car args))
		      (c-frame (cadr args)))
		(format t "Adding new frame ~a at ~a with status ~a~%"
			frame parent r)
		(let* ((new-frame (copy-frame (scene-graph-find scene c-frame) frame parent)))
		  (setf scene (scene-graph scene new-frame))
		  (loop for loc in locations
		     do (setf scene (robray::scene-graph-allow-collision scene frame loc)))
		  (loop for obj in objects
		     do (setf scene (robray::scene-graph-allow-collision scene frame obj))))))
	       ((equal action "remove")
		(setf scene (robray::scene-graph-clean-frame scene frame))))))
	 :wait nil :last nil))
    (values scene q)))
