(load "../../../common/lisp/util.lisp")

(defun resource (str)
  (cond
    ((string= str "ore") :ore)
    ((string= str "clay") :clay)
    ((string= str "obsidian") :obsidian)
    ((string= str "geode") :geode)
    )
  )

(defun parse-line (line)
  (let ((split (split-sequence line #\  #\: #\.)))
    (reverse ; reversing blueprints improves performance later on
      (loop with part = (nthcdr 3 split)
            while part
            collect (cons (resource (car part))
                          (loop for (cost res . rest) on (nthcdr 3 part) by #'cdddr
                                collect (cons (resource res) (parse-integer cost))
                                while (string= (car rest) "and")
                                finally (setf part (cdr rest))))))
    )
  )

(defun value-or-zero (item alist)
  (let ((entry (assoc item alist)))
    (if entry (cdr entry) 0))
  )

(defun update-resources (resources bots &optional (time 1) cost)
  "Add resources produced by bots in given time and subtract a one-time cost."
  (loop for res in (union (mapcar #'car bots) (mapcar #'car resources))
        collect (cons res (+ (* (value-or-zero res bots) time)
                              (value-or-zero res resources)
                              (- (value-or-zero res cost)))))
  )

(defun add-bot (bot bots)
  (if (assoc bot bots)
      (loop for (res . count) in bots
            collect (cons res (if (string= res bot)
                                  (1+ count)
                                  count)))
      (cons (cons bot 1) bots))
  )

(defun upper-bound (resources bots time-left)
  (+ (value-or-zero :geode resources)
     (* time-left (value-or-zero :geode bots))
     (/ (* time-left (1- time-left)) 2))
  )

(defun time-to-build (blueprint resources bots bot cost)
  "Get time needed to produce enough resources to cover cost or nil if we can
   never cover the cost or we don't want to build the bot."
  (and (or (string= bot :geode)
           (< (value-or-zero bot bots) ; current number of bots
              (loop for b in blueprint ; highest cost of resource produced by this bot
                    maximize (value-or-zero bot (cdr b)))))
       (loop for (res . count) in cost
             unless (assoc res bots)
               return nil ; can't produce needed resources
             if (<= count (value-or-zero res resources))
               maximize 0 ; current resources are enough, no time needed
             else
               maximize (ceiling (- count (value-or-zero res resources)) ; res needed
                                 (value-or-zero res bots)))) ; production rate
  )

(defun max-geodes (blueprint time-left &optional (resources '())
                             (bots '((:ore . 1))) (current-max 0))
  (if (or (<= time-left 0) (<= (upper-bound resources bots time-left) current-max))
      (value-or-zero :geode resources)
      (loop for (res . cost) in blueprint
            for time = (time-to-build blueprint resources bots res cost)
            if (and time (> time-left time))
              maximize (max-geodes blueprint
                                  (- time-left time 1)
                                  (update-resources resources bots (1+ time ) cost)
                                  (add-bot res bots)
                                  m)
                into m
            finally (return (max m (max-geodes blueprint
                                               0
                                               (update-resources resources bots)
                                               bots
                                               m)))))
  )

(let ((blueprints (parse-input :pre #'parse-line)))
  (format t "~D~&" (loop for b in blueprints
                         for id from 1
                         sum (* id (max-geodes b 24))))
  (format t "~D~&" (reduce #'* (loop for b in blueprints
                                     repeat 3
                                     collect (max-geodes b 32))))
  )
