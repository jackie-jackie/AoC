(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

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

(defmacro reduce-matrix4 (matrix fun red-row red-col red-elem)
  `(loop for left in (map-matrix ,matrix :left ,fun)
         for right in (map-matrix ,matrix :right ,fun)
         for top in (map-matrix ,matrix :top ,fun)
         for bottom in (map-matrix ,matrix :bottom ,fun)
         ,red-row (loop for vl in left
                   for vr in right
                   for vt in top
                   for vb in bottom
                   ,red-col (,red-elem vl vr vt vb)))
  )

(let ((trees (parse-input :pre (lambda (line) (map 'list #'digit-char-p line)))))
  (format t "~D~&" (reduce-matrix4 trees
                                   (lambda (row)
                                     (loop for h in row
                                           for v = t then (> h m)
                                           maximize h into m
                                           collect v)
                                     )
                                   sum
                                   count
                                   or))
  (format t "~D~&" (reduce-matrix4 trees
                                   (lambda (row)
                                     (loop for (h . rest) on row
                                           for view = (position-if (lambda (x)
                                                                     (>= x h))
                                                                   rest)
                                           collect (if view (1+ view) (length rest)))
                                     )
                                   maximize
                                   maximize
                                   *))
  )
