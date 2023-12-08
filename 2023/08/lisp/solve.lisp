(load (merge-pathnames "../../../common/lisp/util.fasl" *load-truename*))

(defun node-number (node)
  (parse-integer (format nil "~{~2,'0X~}" (map 'list #'char-code node)) :radix 16))

(defun parse-line (line)
  (destructuring-bind (node left right) (mapcar #'node-number
                                                (split-sequence line #\  #\, #\= #\( #\)))
    (cons node (cons left right))))

(defun start-p (node)
  (= (mod node #x100) (node-number "A")))

(defun end-p (node)
  (= (mod node #x100) (node-number "Z")))

(defun steps (start end network directions)
  "Count steps from start to end following directions."
  (loop for direction in directions
        and node = start then (funcall direction (cdr (assoc node network)))
        for steps from 0
        until (cond ((integerp end) (= end node)) ((functionp end) (funcall end node)))
        finally (return steps)))

(defun direction-function (direction)
  (cond ((char= direction #\L) #'car) ((char= direction #\R) #'cdr)))

(let ((directions (map 'list #'direction-function (first (parse-input :until ""))))
      (network (parse-input :pre #'parse-line)))
  (rplacd (last directions) directions)
  (format t "~D~&" (steps (node-number "AAA") (node-number "ZZZ") network directions))
  (format t "~D~&" (loop for start in (remove-if-not #'start-p (mapcar #'car network))
                         collect (steps start #'end-p network directions) into steps
                         finally (return (apply #'lcm steps)))))
