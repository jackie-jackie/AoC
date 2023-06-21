(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun mix (input size &optional (multiplier 1))
  (loop for i from 0 below size
        for before = (loop for c on input
                           until (= i (caadr c))
                           finally (return c))
        for elem = (cadr before)
        for dist = (mod (* (cdr elem) multiplier) (1- size))
        unless (zerop dist)
        do (pop (cdr before))
           (push elem (cdr (nthcdr dist before)))
           (setf input before)
        finally (return input))
  )

(defun score (mixed size &optional (multiplier 1))
  (loop for elem = (nthcdr (mod 1000 size) (loop for c on mixed
                                                 until (zerop (cdar c))
                                                 finally (return c)))
            then (nthcdr (mod 1000 size) elem)
        repeat 3
        sum (* (cdar elem) multiplier))
  )

(defun make-cycle (list)
  (let ((copy (copy-seq list)))
    (setf (cdr (last copy)) copy)
    )
  )

(let* ((input (loop for n in (parse-input :pre #'parse-integer)
                    for i from 0
                    collect (cons i n)))
       (size (length input)))
  (setf *print-circle* t)
  (format t "~D~&" (score (mix (make-cycle input) size) size))
  (format t "~D~&" (loop for mixed = (make-cycle input) then (mix mixed size 811589153)
                         repeat 10
                         finally (return (score mixed size 811589153))))
  )
