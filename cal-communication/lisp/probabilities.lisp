(in-package :cal-coms)

(defun generate-gaussian (mean stdev)
  (let ((x (* (sqrt (* -2
		       (log (random 1.0))))
	      (cos (* 2 pi (random 1.0))))))
    (+ mean
       (* stdev x))))

(defun generate-true (stdev)
  (let ((x (generate-gaussian 0 stdev)))
    (if (> 0 x)
  	(+ x 1)
  	(+ (* x -1) 1))))

(defun generate-false (stdev)
  (let ((x (generate-gaussian 0 stdev)))
    (if (> 0 x)
  	(* x -1)
  	x)))
