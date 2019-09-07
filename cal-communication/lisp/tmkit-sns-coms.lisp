(in-package :cal-coms)

(defparameter *state* nil)
(defparameter *change* nil)
(defparameter *action* nil)

(defparameter *plan-file* "../test/coms-plan.tmp")
(defparameter *update-file* "../test/state-file.tmp")

(defun initialize ()
  (sns::sns-init)
  (setf *state*    (sns::open-channel "state")
	*change* (sns::open-channel "change")
	*action*   (sns::open-channel "action"))

    (ach::flush-channel *state*)
    (ach::flush-channel *change*)
    (ach::flush-channel *action*))

(defun sns-communication (m-plan-list)
  (when m-plan-list
      (let* ((chan (sns::open-channel "points"))
	     (m-plan-list (tmsmt::ensure-list m-plan-list))
	     (len  (length (robray::motion-plan-path (car m-plan-list))))
	     (start-pt (loop for i from 0 to (- (/ len 2) 1)
			  collect (aref (robray::motion-plan-path (car m-plan-list)) i))))

	;; Send the first point with a flag to restart the sequence
	(sns::send-list chan (cons 1 start-pt))
	;; Send the rest of the list
	(loop for m-plan in m-plan-list
	   do (let* ((path (robray::motion-plan-path m-plan))
		     (dest (loop for i from (/ len 2) to (- len 1)
			      collect (aref path i))))
		(sns::send-list chan (cons 0 dest)))))))

(defun string-time ()
  (multiple-value-bind (sec min hr day month year)
      (get-decoded-time)
    (format nil "~4d-~2,'0d-~2,'0d:~2,'0d:~2,'0d:~2,'0d" year month day hr min sec)))

(defun write-file (m-plan)
  (let ((rope-plan (tmsmt::rope m-plan)))
    (tmsmt::output-rope rope-plan *plan-file* :if-exists :supersede)
    (save-backup *plan-file* "coms-plan")))

(defun save-backup (file name)
  (multiple-value-bind (sec min hr day month year)
	(get-decoded-time)
    (copy-file file (format nil (concatenate 'string
					     "../test/~4d-~2,'0d-~2,'0d:~2,'0d:~2,'0d:~2,'0d " name)
			      year month day hr min sec))))

(defun copy-file (in-file out-file)
  (with-open-file (in in-file :direction :input
		      :if-does-not-exist :create)
    (with-open-file (out out-file :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (loop for line = (read-line in nil)
	 while line
	 do (format out "~A~%" line)))))

(defun loop-over-text-channel (chan func &key wait last)
  (let ((r :ok))
    (loop while (or (eq r :ok)
		    (eq r :missed-frame))
       do (multiple-value-bind (new-r str)
	      (sns::read-sns-msg-text chan :wait wait :last last)
	    (setf r new-r)
	    (when (or (eq r :ok)
		      (eq r :missed-frame))
	      (funcall func str r))))))
