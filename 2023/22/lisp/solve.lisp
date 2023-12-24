(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defstruct brick
  id
  head
  safe
  supported-by
  delta)

(defmacro loop-brick (early-exit grid x y z &rest body)
  `(loop with brick-id = (brick-id (aref ,grid ,x ,y ,z))
         with delta = (brick-delta (aref ,grid ,x ,y ,z))
         for position = (list ,x ,y ,z) then (mapcar #'+ position delta)
         while (ignore-errors (= (brick-id (apply #'aref ,grid position)) brick-id))
         ,@body
         ,@(if early-exit `(until (= (third delta) 1)) `())))

(defmacro loop-grid (op grid &rest body)
  `(loop for x from 0 below (array-dimension ,grid 0)
         ,op (loop for y from 0 below (array-dimension ,grid 1)
                   ,op (loop for z from 0 below (array-dimension grid 2)
                             for elem = (aref grid x y z)
                             if (ignore-errors (brick-head elem))
                             ,@body))))

(defun fall-distance (grid x y z)
  "Determine how far a brick can fall down"
  (loop-brick t grid x y z
    for d = (loop for zz from (1- z) downto 0
                  while (equal (aref grid (first position) (second position) zz) 0)
                  count t)
    if (zerop d) return 0
    minimize d))

(defun move-brick (grid x y z distance)
  "Move a whole brick downwards."
  (when (> distance 0)
    (loop-brick nil grid x y z
      do (setf (apply #'aref grid (mapcar #'- position (list 0 0 distance)))
               (apply #'aref grid position))
      do (setf (apply #'aref grid position) 0))
    t))

(defun supported-by (grid x y z)
  "Returns list of brick IDs which support the brick at the given position."
  (loop-brick t grid x y z
    if (ignore-errors (brick-p (apply #'aref grid (mapcar #'- position '(0 0 1)))))
    collect (brick-id (apply #'aref grid (mapcar #'- position '(0 0 1))))))

(defun chain-length (brick bricks)
  (if brick
      1
      0
      )
  )

(let* ((bricks (parse-input :pre (lambda (line)
                                  (mapcar #'parse-integer
                                          (split-sequence line #\, #\~)))))
       (grid (make-array (mapcar #'1+ (subseq (reduce (lambda (a b) (mapcar #'max a b))
                                                      bricks)
                                              3)))))
  ;; place bricks into grid
  (loop for brick in bricks
        for delta = (mapcar #'signum (mapcar #'- (subseq brick 3) brick))
        for i from 1
        do (loop for position = (subseq brick 0 3) then (mapcar #'+ position delta)
                 for head = t then nil
                 do (setf (apply #'aref grid position)
                          (make-brick :id i :head head :safe t
                                      :delta (if (equal delta '(0 0 0)) '(0 1 0) delta)))
                 until (equal position (subseq brick 3))))
  ;; let bricks fall down
  (loop for i from 1
        until (zerop (loop-grid sum grid
                       count (move-brick grid x y z (fall-distance grid x y z)))))
  ;; determine support structure
  (loop-grid do grid
    do (when (= 1 (length (setf (brick-supported-by (aref grid x y z))
                                (remove-duplicates (supported-by grid x y z)))))
         (loop-brick t grid x y z
           do (ignore-errors (setf (brick-safe (apply #'aref
                                                      grid
                                                      (mapcar #'- position '(0 0 1))))
                                   nil)))))
  ;; merge brick safety
  (loop-grid do grid
    do (setf (brick-safe (aref grid x y z))
             (loop-brick nil grid x y z
               always (brick-safe (apply #'aref grid position)))))

  (let ((bricks-processed (loop-grid append grid
                            collect (aref grid x y z))))
    (format t "~D~&" (count-if #'brick-safe bricks-processed))
    (format t "~D~&" nil)))
