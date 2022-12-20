(load "../../../common/lisp/util.lisp")

(defun parse-line (line)
  (let ((split (split-sequence line #\  #\: #\.)))
    (loop with part = (nthcdr 3 split)
          while part
          collect (cons (car part)
                        (loop for (cost type . rest) on (nthcdr 3 part) by #'cdddr
                              collect (cons type (parse-integer cost))
                              while (string= (car rest) "and")
                              finally (setf part (cdr rest)))))
    )
  )

(defun value-or-zero (item alist)
  (let ((entry (assoc item alist :test #'string=)))
    (if entry (cdr entry) 0))
  )

(defun update-res (res bots &optional (time 1) cost)
  "Add resources produced by bots in given time and subtract a one-time cost."
  (loop for type in (union (mapcar #'car bots) (mapcar #'car res) :test #'string=)
        collect (cons type (+ (* (value-or-zero type bots) time)
                              (value-or-zero type res)
                              (- (value-or-zero type cost)))))
  )

(defun add-bot (new bots)
  (if (assoc new bots :test #'string=)
      (loop for (b . c) in bots
            collect (cons b (if (string= b new)
                                (1+ c)
                                c)))
      (cons (cons new 1) bots))
  )

(defun upper-bound (resources bots time-left)
  (+ (value-or-zero "geode" resources)
     (* time-left (value-or-zero "geode" bots))
     (/ (* time-left (1- time-left)) 2))
  )

(defun time-to-build (blueprint res bots bot cost)
  "Get time needed to produce enough resources to cover cost or nil if we can
   never cover the cost or we don't want to build the bot."
  (and (or (string= bot "geode")
           (< (value-or-zero bot bots) ; current number of bots
              (loop for b in blueprint ; highest cost of resource produced by this bot
                    maximize (value-or-zero bot (cdr b)))))
       (loop for (r . c) in cost
             unless (assoc r bots :test #'string=) ; can't produce needed resources
               return nil
             if (<= c (value-or-zero r res)) ; current resources are sufficient
               maximize 0
             else
               maximize (ceiling (- c (value-or-zero r res)) (value-or-zero r bots))))
  )

(defun max-geodes (blueprint resources bots time-left current-max)
  (if (or (<= time-left 0) (<= (upper-bound resources bots time-left) current-max))
      (value-or-zero "geode" resources)
      (loop for (type . cost) in (reverse blueprint)
            for time = (time-to-build blueprint resources bots type cost)
            if (and time (> time-left time))
              maximize (max-geodes blueprint
                                  (update-res resources bots (1+ time ) cost)
                                  (add-bot type bots)
                                  (- time-left time 1)
                                  m)
                into m
            finally (return (max m (max-geodes blueprint
                                (update-res resources bots)
                                bots
                                0
                                m)))))
  )

(let ((blueprints (parse-input :pre #'parse-line)))
  (format t "~D~&" (loop for b in blueprints
                         for id from 1
                         sum (* id (max-geodes b '() '(("ore" . 1)) 24 0))))
  (format t "~D~&" (reduce #'* (loop for b in blueprints
                                     repeat 3
                                     collect (max-geodes b '() '(("ore" . 1)) 32 0))))
  )
