(in-package :point-gen)


(defun send-list (chan data)
  (let* ((len (length data))
	 (msg (sns-msg-vector-heap-alloc (+ 1 len))) ;;for some reason the first element is always 0.
	 (ptr (foreign-slot-pointer msg '(:struct sns-msg-vector) 'x))
	 (i 1))


    (loop for el in data
       do (setf (mem-aref ptr
			  :double i)  (coerce el 'double-float))
	 do (incf i 1))
    (setf (foreign-slot-value msg '(:struct sns-msg-vector) 'x) ptr)
    (ach::check-status (sns-msg-vector-put (ach::ach-handle-pointer chan) msg)
		       "Failed to put message on channel")))


(defun test ()
  (sns-init)
  (let* ((q0 '(0 0 0 0 0 0 0))
	 (q1 '(1 0 0 0 0 0 0))
	 (chan (open-channel "points")))
    (send-list chan q0)
    (sleep 1)
    (send-list chan q1)
    (sleep 1)
    (send-list chan q0))
  (sns-end))
