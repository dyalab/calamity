(in-package :cal-coms)

(defun communication (m-plan-list)
  (when m-plan-list
      (sns::sns-init)
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
		(sns::send-list chan (cons 0 dest)))))
      (sns::sns-end)))
