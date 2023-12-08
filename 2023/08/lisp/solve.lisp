(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun node-number (node)
  (parse-integer (format nil "~{~2,'0X~}" (map 'list (lambda (c) (char- c #\A)) node))
                 :radix 16))

(defun start-p (node)
  (= (mod node #x100) (node-number "A")))

(defun end-p (node)
  (= (mod node #x100) (node-number "Z")))

(defun steps (start end network directions)
  "Count steps from start to end following directions."
  (loop for direction in directions
        and node = start then (funcall direction (aref network node))
        for steps from 0
        until (cond ((integerp end) (= end node)) ((functionp end) (funcall end node)))
        finally (return steps)))

(defun direction-function (direction)
  (cond ((char= direction #\L) #'car) ((char= direction #\R) #'cdr)))

(defun parse-line (line)
  (mapcar #'node-number (split-sequence line #\  #\, #\= #\( #\))))

(let ((directions (map 'list #'direction-function (first (parse-input :until ""))))
      (network-list (parse-input :pre #'parse-line))
      (network (make-array (1+ (node-number "ZZZ")))))
  (rplacd (last directions) directions)
  (loop for (node left right) in network-list
        do (setf (aref network node) (cons left right)))
  (format t "~D~&" (steps (node-number "AAA") (node-number "ZZZ") network directions))
  (format t "~D~&" (loop for start in (remove-if-not #'start-p network-list :key #'car)
                         collect (steps (car start) #'end-p network directions) into steps
                         finally (return (apply #'lcm steps)))))
