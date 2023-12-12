(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun factorial (n &optional (acc 1))
  (if (= n 0) acc (factorial (1- n) (* n acc))))

(defun binomial (n k)
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

(defun cluster-lengths (gears)
  (mapcar #'length (split-sequence gears #\.)))

(defun count-exhaustive (gears index target free)
  (cond ((< free 0) 0)
        ((> (+ (length (cluster-lengths (subseq gears 0 (position #\? gears)))))
            (length target))
         0)
        ((let ((diffs (mapcar #'-
                              (cluster-lengths (subseq gears 0 (position #\? gears)))
                              target)))
           (and (find-if-not #'zerop diffs)
                (or (> (find-if-not #'zerop diffs) 0)
                    (< (position-if-not #'zerop diffs) (1- (length diffs))))))
         0)
        ((>= index (length gears)) (if (/= free 0) 0 1))
        ((char= (char gears index) #\?)
         (+ (count-exhaustive (let ((gears-copy (copy-seq gears)))
                        (setf (char gears-copy index) #\.)
                        gears-copy)
                      (1+ index)
                      target
                      free)
            (count-exhaustive (let ((gears-copy (copy-seq gears)))
                        (setf (char gears-copy index) #\#)
                        gears-copy)
                      (1+ index)
                      target
                      (1- free))))
        (t (count-exhaustive gears (1+ index) target free))))

(defun cluster-min-size (cluster)
  (count #\# cluster))

(defun cluster-max-size (cluster)
  (length cluster))

(defun cluster-max-clusters (cluster)
  (let ((clstr (loop for gear across cluster
                     and prev = #\? then gear
                     unless (char= prev gear #\#) collect gear)))
    (setf clstr (loop for gear in clstr
                      and prev = #\? then gear
                      if (char= prev #\#) collect #\.
                      else collect gear))
    (setf clstr (loop for gear in (reverse clstr)
                      and prev = #\? then gear
                      if (char= prev #\#) collect #\.
                      else collect gear))
    (+ (reduce #'+ (mapcar (lambda (c) (ceiling c 2))
                           (mapcar #'length (split-sequence clstr #\# #\.))))
       (count #\# clstr))))

(defun cluster-possibilities (cluster target)
  (if (and target
           (>= (length cluster) (+ (reduce #'+ target) (1- (length target))))
           (every (curry #'char= #\?) cluster))
      (binomial (- (length cluster) (reduce #'+ target) -1)
                (- (length cluster) (reduce #'+ target) (1- (length target))))
      (count-exhaustive cluster 0 target (- (reduce #'+ target) (count #\# cluster)))))

(defun process-clusters (clusters target)
  (if (= 1 (length clusters))
      (cluster-possibilities (first clusters) target)
      (loop with cluster = (first clusters)
            with rest = (rest clusters)
            for split from 0 to (min (length target) (cluster-max-clusters cluster))
            for left = (subseq target 0 split)
            for right = (subseq target split (length target))
            if (and (>= (cluster-max-size cluster) (reduce #'+ left))
                    (<= (cluster-min-size cluster) (reduce #'+ left))
                    (>= (reduce #'+ (mapcar #'cluster-max-size rest)) (reduce #'+ right))
                    (<= (reduce #'+ (mapcar #'cluster-min-size rest)) (reduce #'+ right)))
            sum (* (cluster-possibilities cluster left)
                   (process-clusters rest right)))))

(defun possibilities (record)
  (process-clusters (split-sequence (first record) #\.) (rest record)))

(let* ((input (parse-input :pre (lambda (line)
                                  (destructuring-bind (gears . target)
                                    (split-sequence line #\  #\,)
                                    (cons gears (mapcar #'parse-integer target)))))))
  (format t "~D~&" (reduce #'+ (mapcar #'possibilities input)))
  (quit)
  (format t "~D~&" (reduce #'+
                           (mapcar #'possibilities
                                   (mapcar (lambda (record)
                                             (cons (format nil "~{~A~^?~}"
                                                           (loop repeat 5
                                                                 collect (first record)))
                                                   (loop repeat 5 append (rest record))))
                                           input)))))
