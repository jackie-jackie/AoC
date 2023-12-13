(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defparameter *directions* '#1=(((-1 . 0) (-1 . 1) (-1 . -1)) ; north
                                ((1 . 0) (1 . 1) (1 . -1)) ; south
                                ((0 . -1) (1 . -1) (-1 . -1)) ; west
                                ((0 . 1) (1 . 1) (-1 . 1)) ; east
                                . #1#)
  )

(defun score (elves)
  (loop for i from 0 below (array-total-size elves)
        for tile = (row-major-aref elves i)
        if (consp tile) minimize (car tile) into min-x
        and maximize (car tile) into max-x
        and minimize (cdr tile) into min-y
        and maximize (cdr tile) into max-y
        and count t into n-elves
        finally (return (- (* (- max-x min-x -1) (- max-y min-y -1)) n-elves)))
  )

(defun proposed-move (elf elves directions)
  (loop with (elf-x . elf-y) = elf
        for dir in directions
        for (target-dx . target-dy) = (car dir)
        repeat 4
        if (loop for (dx . dy) in dir
                 never (aref elves (+ elf-x dx) (+ elf-y dy)))
          collect (cons (+ elf-x target-dx) (+ elf-y target-dy)) into free
        finally
          (return (if (zerop (mod (length free) 4)) elf (car free))))
  )

(defun simulate-round (elves directions)
  ; determine proposed moves
  (loop for i from 0 below (array-total-size elves)
        for elf = (row-major-aref elves i)
        if elf
          do (setf (row-major-aref elves i) (proposed-move elf elves directions)))
  ; move elves handling possible collisions
  (loop for i from 0 below (array-dimension elves 0)
        do (loop for j from 0 below (array-dimension elves 1)
                 for elf = (aref elves i j)
                 for (target-x . target-y) = elf
                 if (and elf
                         (consp (aref elves target-x target-y))
                         (not (equal elf (cons i j))))
                   do (let ((target (aref elves target-x target-y)))
                        (setf (aref elves (car target) (cdr target))
                              (cons (car target) (cdr target)))
                        (setf (aref elves i j) (cons i j))
                        (setf (aref elves target-x target-y) :occupied))
                 else if (and elf (eql (aref elves target-x target-y) :occupied))
                   do (setf (aref elves i j) (cons i j))
                 else if (and elf (not (aref elves target-x target-y)))
                   do (setf (aref elves target-x target-y) (cons i j))
                      (setf (aref elves i j) :origin)))
  ; cleanup
  (loop for i from 0 below (array-dimension elves 0)
        sum (loop for j from 0 below (array-dimension elves 1)
                  for elf = (aref elves i j)
                  if (and (consp elf) (not (equal elf (cons i j))))
                    do (setf (aref elves i j) (cons i j))
                  and count t
                  else if (not (consp elf))
                    do (setf (aref elves i j) nil)))
  )

(defun simulate (elves checkpoint)
  (loop with checkpoint-value = nil
        for directions on *directions*
        for rounds from 1
        while (> (simulate-round elves directions) 0)
        if (= rounds checkpoint)
          do (setf checkpoint-value (score elves))
        finally (return (values rounds checkpoint-value))
        )
  )

(let* ((input (parse-input))
       ; TODO adaptive array size
       (elves (make-array (list (* 4 (length input)) (* 4 (length (car input))))
                          :initial-element nil)))
  (setf *print-circle* t)
  (loop for line in input
        for i from (floor (array-dimension elves 0) 4)
        do (loop for c across line
                 for j from (floor (array-dimension elves 1) 4)
                 if (char= c #\#)
                   do (setf (aref elves i j) (cons i j))
                 )
        )
  (multiple-value-bind
    (rounds checkpoint-value )
    (simulate elves 10)
    (format t "~D~&" checkpoint-value)
    (format t "~D~&" rounds)
    )
  )
