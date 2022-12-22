(load "../../../common/lisp/util.lisp")

(defun parse-path (path)
  (loop with start = 0
        until (= start (length path))
        collect (multiple-value-bind
                  (num end)
                  (parse-integer path :start start :junk-allowed t)
                  (setf start end)
                  num)
        if (< start (length path))
          collect (char path start)
          and do (incf start)
        )
  )

(defun walk (location direction distance board)
  (loop with row = (car location)
        with col = (cdr location)
        for nrow = (mod (+ row (case direction (1 1) (3 -1) (t 0)))
                        (array-dimension board 0))
        for ncol = (mod (+ col (case direction (0 1) (2 -1) (t 0)))
                        (array-dimension board 1))
        for i from 1 to distance
        do (loop while (eq (aref board nrow ncol) #\ )
                 do (setf nrow (mod (+ nrow (case direction (1 1) (3 -1) (t 0)))
                                    (array-dimension board 0)))
                    (setf ncol (mod (+ ncol (case direction (0 1) (2 -1) (t 0)))
                                    (array-dimension board 1))))
        if (eq (aref board nrow ncol) #\#)
          return (cons row col)
        else
          do (setf row nrow)
             (setf col ncol)
        finally (return (cons row col)))
  )

(defun score (row column direction)
  (+ (* 1000 (1+ row)) (* 4 (1+ column)) direction)
  )

(let* ((input (parse-input :until ""))
       (board (make-array (list (length input) (reduce #'max (mapcar #'length input)))
                          :initial-element #\ ))
       (path (cons nil (car (parse-input :pre #'parse-path)))))
  (setf *print-circle* t)
  (loop for line in input
        for i from 0
        do (loop for c across line
                 for j from 0
                 do (setf (aref board i j) c)))
  (format t "~D~&" (loop for (turn dist) on path by #'cddr
                         for dir = 0 then (mod (+ dir (case turn (#\L -1) (#\R 1))) 4)
                         for loc = (walk (cons 0 (position #\. (car input)))
                                         dir
                                         dist
                                         board)
                             then (walk loc dir dist board)
                         finally (return (score (car loc) (cdr loc) dir))))
  (format t "~D~&" nil)
  )
