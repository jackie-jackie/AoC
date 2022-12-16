(load "../../../common/lisp/util.lisp")

(defun beacon-blocked (sensors row)
  (loop for (sx sy bx by) in sensors
        for sensor-range = (+ (abs (- sx bx)) (abs (- sy by)))
        for sensor-distance = (abs (- row sy))
        if (<= sensor-distance sensor-range)
          collect (cons (- sx (- sensor-range sensor-distance))
                        (+ sx (- sensor-range sensor-distance))))
  )

(defun combine-ranges (ranges)
  (loop with sorted =  (sort ranges #'< :key #'car)
        with acc = (car sorted)
        for queue in (cdr sorted)
        if (> (car queue) (1+ (cdr acc))) collect acc into result
        and do (setf acc queue)
        else do (setf (cdr acc) (max (cdr acc) (cdr queue)))
        finally (return (reverse (cons acc result)))
    )
  )

(defun count-ranges (ranges)
  (reduce #'+ (mapcar #'1+ (mapcar #'- (mapcar #'cdr ranges) (mapcar #'car ranges))))
  )

(let ((sensors (parse-input :pre (lambda (line)
                                   (loop for s in (cdr (split-sequence line #\= #\, #\:))
                                                 by #'cddr
                                         collect (parse-integer s)
                                         )
                                   ))))
  (format t "~D~&" (- (count-ranges (combine-ranges (beacon-blocked sensors 2000000)))
                      (length (remove-duplicates (sort (mapcar #'caddr
                                                               (remove 2000000
                                                                       sensors
                                                                       :test #'/=
                                                                       :key #'cadddr))
                                                       #'<)))
                      ))
  (format t "~D~&" (loop for y from 0 to 4000000
                         for ranges = (combine-ranges (beacon-blocked sensors y))
                         if (> (length ranges) 1)
                           ; doesn't work if there are gaps outside solution area
                           return (+ y (* 4000000 (1+ (cdar ranges))))
                         if (> (caar ranges) 0)
                           return y
                         if (< (cdar ranges) 4000000)
                           return (+ y (* 4000000 4000000))))
  )
