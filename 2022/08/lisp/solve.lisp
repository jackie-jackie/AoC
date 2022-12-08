(load "../../../common/lisp/util.lisp")

(defmacro map-matrix (matrix direction fun)
  (case direction
    (:left `(loop for row in ,matrix
                  collect (,fun row)))
    (:right `(mapcar #'reverse (map-matrix (mapcar #'reverse ,matrix) :left ,fun)))
    (:top `(apply #'mapcar #'list (map-matrix (apply #'mapcar #'list ,matrix) :left ,fun)))
    (:bottom `(reverse (map-matrix (reverse ,matrix) :top ,fun)))
    (t (error "Unknown direction ~a" direction))
    )
  )

(defmacro scene-from (trees direction)
  `(map-matrix ,trees
               ,direction
               (lambda (row)
                 (loop for h in row
                       for rest = (cdr row) then (cdr rest)
                       for view = (position-if (lambda (x) (>= x h)) rest)
                       collect (if view (1+ view) (length rest)))
                 ))
  )


(defmacro visible-from (trees direction)
  `(map-matrix ,trees
               ,direction
               (lambda (row)
                 (loop for h in row
                       for v = t then (> h m)
                       maximize h into m
                       collect v)
                 ))
  )



(let ((trees (parse-input :pre (lambda (line)
                                 (loop for c across line
                                       collect (digit-char-p c))))))
  (format t "~D~%" (loop for left in (visible-from trees :left)
                         for right in (visible-from trees :right)
                         for top in  (visible-from trees :top)
                         for bottom in  (visible-from trees :bottom)
                         sum (loop for vl in left
                                   for vr in right
                                   for vt in top
                                   for vb in bottom
                                   count (or vl vr vt vb))))
  (format t "~D~%" (loop for left in (scene-from trees :left)
                         for right in (scene-from trees :right)
                         for top in  (scene-from trees :top)
                         for bottom in  (scene-from trees :bottom)
                         maximize (loop for vl in left
                                        for vr in right
                                        for vt in top
                                        for vb in bottom
                                        maximize (* vl vr vt vb))))
  )
