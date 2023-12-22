(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))

(defun walk (grid)
  (loop for toggle = t then (not toggle)
        until (zerop (grid-stencil5 (lambda (self &rest neighbors)
                                      (if (or (numberp self) (eql self #\.))
                                          (let ((n (remove-if-not #'numberp neighbors)))
                                            (if (and n (or (not (numberp self))
                                                           (< (reduce #'min n) self)))
                                                (1+ (reduce #'min n)) self))
                                          self))
                                    grid))))

(defun count-simple (grid steps)
  (count-if (lambda (x)
              (and (numberp x) (<= x steps) (= (mod x 2) (mod steps 2))))
            (grid-list grid)))

(defun count-periodic-diagonal (pattern center steps direction)
  (let ((corner (make-array (array-dimensions pattern)))
        (row-max (1- (array-dimension pattern 0)))
        (col-max (1- (array-dimension pattern 1))))
    (grid-stencil1 #'identity pattern :target-grid corner)
    (setf (apply #'aref corner (ecase direction
                                 (:north-west (list row-max col-max))
                                 (:north-east (list row-max 0))
                                 (:south-west (list 0 col-max))
                                 (:south-east (list 0 0))))
          (+ 2 (apply #'aref center (ecase direction
                                      (:north-west (list 0 0))
                                      (:north-east (list 0 col-max))
                                      (:south-west (list row-max 0))
                                      (:south-east (list row-max col-max))))))
    (walk corner)
    (reduce #'+
            (mapcar (lambda (x)
                      (cond
                        ((and (numberp x) (= (mod x 2) (mod steps 2)))
                         (let ((n (max 0 (1+ (floor (- steps x)
                                                    (* (array-dimension pattern 0)
                                                       2))))))
                           (* n n)))       ; only works if rows = cols
                        ((and (numberp x) (/= (mod x 2) (mod steps 2)))
                         (let ((n (max 0 (1+ (floor (- steps x
                                                       (array-dimension pattern 0))
                                                    (* (array-dimension pattern 0)
                                                       2))))))
                           (* n (1+ n))))  ; only works if rows = cols
                        (t 0)))
                    (grid-list corner)))))


(defun count-periodic-orthogonal (pattern center steps direction)
  (loop with edge = (make-array (array-dimensions pattern))
        with row-max = (1- (array-dimension pattern 0))
        with col-max = (1- (array-dimension pattern 1))
        with dim = (ecase direction ((:west :east) 0) ((:north :south) 1))
        with buffer = (make-array (array-dimension pattern dim))
        for first = t then nil
        repeat 10 ; TODO convergence criterium
        do (loop for x from 0 below (array-dimension buffer 0)
                 do (setf (aref buffer x)
                          (apply #'aref
                                 (if first center edge)
                                 (ecase direction
                                   (:west (list x 0))
                                   (:north (list 0 x))
                                   (:east (list x col-max))
                                   (:south (list row-max x))))))
        do (grid-stencil1 #'identity pattern :target-grid edge)
        do (loop for x from 0 below (array-dimension buffer 0)
                 do (setf (apply #'aref edge (ecase direction
                                               (:west (list x col-max))
                                               (:north (list row-max x))
                                               (:east (list x 0))
                                               (:south (list 0 x))))
                          (1+ (aref buffer x))))
        do (walk edge)
        sum (count-simple edge steps) into sum
        finally (return (+ (- (count-simple edge steps))
                           sum
                           (reduce
                             #'+
                             (mapcar
                               (lambda (x)
                                 (if (and (numberp x) (= (mod x 2) (mod steps 2)))
                                     (max 0 (1+ (floor (- steps x)
                                                       (array-dimension pattern dim))))
                                     0))
                               (grid-list edge)))))))

(defun count-periodic (pattern center steps)
  (+ (count-simple center steps)
     (reduce #'+ (mapcar (curry #'count-periodic-orthogonal pattern center steps)
                         '(:west :north :east :south)))
     (reduce #'+ (mapcar (curry #'count-periodic-diagonal pattern center steps)
                         '(:north-west :north-east :south-west :south-east)))))

(let* ((pattern (make-grid (parse-input)))
       (center (make-array (array-dimensions pattern)))
       (start (position-grid #\S pattern)))
  (when (apply #'/= (array-dimensions pattern))
    (format t "Solution only valid for quadratic patterns!~%"))
  (when (evenp (array-dimension pattern 0))
    (format t "Solution only valid for odd dimensions!~%"))
  (setf (aref pattern (car start) (cdr start)) #\.)
  (grid-stencil1 #'identity pattern :target-grid center)
  (setf (aref center (car start) (cdr start)) 0)
  (walk center)

  (format t "~D~&" (count-simple center 64))
  ;; only works on example up to 100 steps
  (format t "~D~&" (count-periodic pattern center 26501365)))
