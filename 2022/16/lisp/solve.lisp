(load "../../../common/lisp/util.lisp")

(defun valve-int (str)
  (+ (* 26 (char- (char str 0) #\A))
     (char- (char str 1) #\A))
  )

(defun parse-line (line)
  (let ((split (split-sequence line #\  #\, #\; #\=)))
    (list (valve-int (nth 1 split))
          (parse-integer (nth 5 split))
          (mapcar #'valve-int (subseq split 10)))
    )
  )

(defun tunnels (input)
  (loop for i in input
        append (loop for tun in (caddr i)
                     if (< (car i) tun) collect (cons (cons (car i) tun) 1)))
  )

(defun valves (input)
  (loop for i in input
        collect (cons (car i) (cadr i)))
  )

(defun trans-closure (tunnels)
  (loop for max-dist from 1
        for cl = tunnels
            then (loop for ((start . end) . dist) in closure
                       append (loop for ((s . e) . d) in closure
                                    if (and (= end s)
                                            (<= (+ dist d) max-dist)
                                            (not (assoc (cons start e)
                                                        closure
                                                        :test #'equal)))
                                    collect (cons (cons start e)
                                                  (+ dist d))
                                    if (and (= start s)
                                            (/= end e)
                                            (<= (+ dist d) max-dist)
                                            (not (assoc (cons (min end e)
                                                              (max end e))
                                                        closure
                                                        :test #'equal)))
                                    collect (cons (cons (min end e) (max end e))
                                                  (+ dist d))
                                    if (and (= end e)
                                            (/= start s)
                                            (<= (+ dist d) max-dist)
                                            (not (assoc (cons (min start s)
                                                              (max start s))
                                                        closure
                                                        :test #'equal)))
                                    collect (cons (cons (min start s)
                                                        (max start s))
                                                  (+ dist d))))
        while cl
        append (remove-duplicates cl :test #'equal) into closure
        finally (return closure))
  )

(defun dists-valves (from tunnels time-left)
  (when from (loop for ((s . e) . d) in tunnels
                   if (and (= from s) (< d time-left)) collect (cons e d)
                   if (and (= from e) (< d time-left)) collect (cons s d)))
  )

(defun release (time-left start valves tunnels)
  (if (or (<= time-left 0) (not valves))
      0
      (loop with dists = (dists-valves start tunnels time-left)
            for (next . rate) in valves
            for distance = (cdr (assoc next dists))
            if distance
              maximize (+ (release (- time-left distance 1) next
                                   (remove next valves :key #'car)
                                   tunnels)
                          (* rate (- time-left distance 1)))))
  )

(defun sane-combinations (time-left start valves tunnels)
  (loop with dists0 = (dists-valves (car start) tunnels (car time-left))
        with dists1 = (dists-valves (cdr start) tunnels (cdr time-left))
        for (v . vs) on (mapcar #'car valves) while vs
        append (loop for vv in vs
                     for dv0 = (cdr (assoc v dists0))
                     for dv1 = (cdr (assoc v dists1))
                     for dvv0 = (cdr (assoc vv dists0))
                     for dvv1 = (cdr (assoc vv dists1))
                     if (and dv0 dv1 dvv0 dvv1 (<= dv0 dv1) (<= dvv1 dvv0))
                       collect (cons (assoc v dists0) (assoc vv dists1))
                     else if (and dv0 dv1 dvv0 dvv1 (>= dv0 dv1) (>= dvv1 dvv0))
                       collect (cons (assoc vv dists0) (assoc v dists1))
                     else if (and dv0 dv1 dvv0 dvv1)
                       collect (cons (assoc v dists0) (assoc vv dists1))
                       and collect (cons (assoc vv dists0) (assoc v dists1))
                     else if (and dv0 dvv1)
                       collect (cons (assoc v dists0) (assoc vv dists1))
                     else if (and dvv0 dv1)
                       collect (cons (assoc vv dists0) (assoc v dists1))))
  )

(defun release2 (time-left start valves tunnels)
  (if (or (not valves))
      0
      (loop with combos = (sane-combinations time-left start valves tunnels)
            for ((next0 . distance0) . (next1 . distance1)) in combos
            maximize (+ (release2 (cons (- (car time-left) distance0 1)
                                        (- (cdr time-left) distance1 1))
                                  (cons next0 next1)
                                  (remove next0
                                          (remove next1
                                                  valves
                                                  :key #'car)
                                          :key #'car)
                                  tunnels)
                        (* (cdr (assoc next0 valves))
                           (- (car time-left) distance0 1))
                        (* (cdr (assoc next1 valves))
                           (- (cdr time-left) distance1 1))) into m
            finally (return (if combos
                                m
                                (max (release (car time-left)
                                              (car start)
                                              valves
                                              tunnels)
                                     (release (cdr time-left)
                                              (cdr start)
                                              valves
                                              tunnels))))))
  )

(let* ((input (parse-input :pre #'parse-line))
       (valves (valves input))
       (tunnels (trans-closure (tunnels input))))
  (format t "~D~&" (release 30 0 (remove 0 valves :key #'cdr) tunnels))
  (format t "~D~&" (release2 '(26 . 26) '(0 . 0) (remove 0 valves :key #'cdr) tunnels))
  )
