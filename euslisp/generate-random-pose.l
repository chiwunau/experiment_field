(require "package://pr2eus/pr2-interface.l")
(if (not (boundp '*pr2*))
    (pr2))
(if (not (boundp '*irtviewer*))
    (make-irtviewer))
(send *irtviewer* :objects (list *pr2*))

(setq *robots* nil)

(defun get-min-max-angle-table
  ()
  (let (min max)
    (setq min (mapcar #'(lambda (j) (send j :min-angle)) (send *pr2* :joint-list)))
    (setq max (mapcar #'(lambda (j) (send j :max-angle)) (send *pr2* :joint-list)))
    (list min max)))

(defun make-random-angle-vector ()
  (let* ((min-max-table (get-min-max-angle-table))
	 (random-av (coerce (make-list (length (car min-max-table)) :initial-element 0) float-vector)))

    (dotimes (n (length (car min-max-table)))
      (let ((min (elt (car min-max-table) n))
	    (max (elt (cadr min-max-table) n)))
	(if (or (eq (abs min) *inf*) (eq (abs max) *inf*))
	    (setf (elt random-av n) (+ -180 (random 180.0)))
	  (setf (elt random-av n) (+ min (* (random 1.0) (- max min)))))))
    random-av))

(defun play-random-av (&optional (interval 1.0))
  (do-until-key
   (dotimes (i (length *robots*))
   (send (elt *robots* i) :angle-vector (make-random-angle-vector)))
   (send *irtviewer* :draw-objects)
   (unix:usleep (round (* interval 1e6)))))

(defun init-robot-list (&optional (nofrobot 10))
  (setq *robots* (make-list nofrobot))
  (dotimes (i nofrobot)
    (let ((r (pr2))
	  (c (make-coords)))
      (send c :translate (float-vector (* 1500 (round (/ i 10))) (* 1500.0 (mod i 10)) 0))
      (send r :move-to c)
    (setf (elt *robots* i) r))))



  
	  
	  
      
	