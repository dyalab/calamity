(in-package :cal-coms)


(defun find-frame-type (sg type)
  (let ((ret))
    (robray::map-scene-graph-frames nil (lambda (frame)
					  (if
					   (robray::scene-frame-geometry-isa
					    frame type)
					   (setf ret (cons (robray::scene-frame-name frame)
							   ret))))
				    sg)
    ret))

(defun copy-frame (frame &optional name parent)
  (let* ((geom (car (robray::scene-frame-fixed-geometry frame)))
	 (dim (robray::scene-box-dimension (robray::scene-geometry-shape geom)))
	 (opts (robray::scene-geometry-options geom))
	 (type (robray::scene-geometry-type geom))
	 (new-frame (copy-structure frame))
	 (new-geom (robray::scene-geometry-box opts dim))
	 (name   (or name (robray::scene-frame-name frame)))
	 (parent (or parent (robray::scene-frame-parent frame))))

    (setf (robray::scene-geometry-type new-geom)   type
	  (robray::scene-frame-parent new-frame)   parent
	  (robray::scene-frame-name new-frame)     name
	  (robray::scene-frame-geometry new-frame) (list new-geom))
    new-frame))

(defun copy-frames (sg types)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for type in types
       do (setf (gethash type hash)
		(copy-frame (scene-graph-find sg (car (find-frame-type sg type))))))
    hash))
