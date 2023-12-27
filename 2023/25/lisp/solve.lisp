(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun bfs (neighbors start end)
  "Returns a path from start to end using breadth-first-search."
  (loop with pred = (make-array (array-dimension neighbors 0) :initial-element nil)
        with queue = (list start)
        while queue
        for node = (pop queue)
        do (loop for n in (aref neighbors node)
                 unless (or (aref pred n) (= n start))
                 do (setf (aref pred n) node)
                 and do (push n queue))
        if (aref pred end) return (loop for n = end then (aref pred n)
                                        while n
                                        collect n)))

(defun same-partition-p (neighbors start end)
  "Tests if start and end are in the same partition of the graph after the cut
   by counting the number of disjoint paths from start to end. Start and end are
   in different partition iff there are 3 such paths."
  (loop with neighbors-copy = (copy-seq neighbors)
        for i from 0
        for path = (bfs neighbors-copy start end)
        while path
        do (loop for (a b) on path
                 while b
                 do (setf (aref neighbors-copy b) (remove a (aref neighbors-copy b))))
        finally (return (/= i 3))))

(let* ((nodes (loop with node-ids = (make-hash-table :test #'equal)
                    with id = -1
                    for node in (parse-input)
                    collect (loop for n in (split-sequence node #\  #\:)
                                  if (gethash n node-ids) collect (gethash n node-ids)
                                  else collect (setf (gethash n node-ids) (incf id)))))
       (neighbors (make-array (1+ (reduce #'max (flatten nodes))) :initial-element '())))
  ;; Build (symmetric) neighbor lists
  (loop for node in nodes
        do (loop for n in (rest node)
                 do (push n (aref neighbors (first node)))
                 do (push (first node) (aref neighbors n))))
  (format t "~D~&" (loop for node from 0 below (array-dimension neighbors 0)
                         count (same-partition-p neighbors 0 node) into size
                         finally (return (* size (- (array-dimension neighbors 0) size)))))
  (format t "~D~&" nil))
