(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defclass module ()
  ((outputs :initarg :outputs :initform nil :accessor outputs)))

(defclass flip-flop (module)
  ((state :initform :low :accessor state)))
(defun make-flip-flop (outputs)
  (make-instance 'flip-flop :outputs outputs))

(defclass conjunction (module)
  ((state :initarg :state :initform '() :accessor state)))
(defun make-conjunction (outputs)
  (make-instance 'conjunction :outputs outputs))

(defun add-input (conjunction new-input)
  (unless (assoc new-input (state conjunction))
    (push (cons new-input :low) (state conjunction))))

(defclass broadcaster (module) ())
(defun make-broadcaster (outputs)
  (make-instance 'broadcaster :outputs outputs))

(defgeneric process (module from pulse))

(defmethod process ((module broadcaster) from pulse)
  (declare (ignore from))
  pulse)

(defmethod process ((module flip-flop) from pulse)
  (declare (ignore from))
  (when (eql pulse :low)
    (setf (state module) (ecase (state module) (:high :low) (:low :high)))))

(defmethod process ((module conjunction) from pulse)
  (rplacd (assoc from (state module)) pulse)
  (if (every (curry #'eql :high) (mapcar #'cdr (state module)))
      :low
      :high))

(defun to-keyword (string)
  (read-from-string (concatenate 'string ":" string)))

(defun count-pulses (modules)
  (loop with pulses = (make-array 1
                                  :initial-element (list :button :roadcaster :low)
                                  :fill-pointer 1)
        for i from 0
        while (< i (length pulses))
        for (from to pulse) = (aref pulses i)
        for new-pulse = (ignore-errors (process (gethash to modules) from pulse))
        if new-pulse do (loop for out in (outputs (gethash to modules))
                              do (vector-push-extend (list to out new-pulse)
                                                     pulses))
        finally (return (complex (count :low pulses :key #'third)
                                 (count :high pulses :key #'third)))))

(defun target-rx (modules)
  (loop with pulses = (make-array 1
                                  :initial-element (list :button :roadcaster :low)
                                  :fill-pointer 1)
        for i from 0
        while (< i (length pulses))
        for (from to pulse) = (aref pulses i)
        for new-pulse = (ignore-errors (process (gethash to modules) from pulse))
        if (and (eql to :rx) (eql pulse :low)) return t
        if new-pulse do (loop for out in (outputs (gethash to modules))
                              do (vector-push-extend (list to out new-pulse)
                                                     pulses))))

(let* ((input (parse-input :pre (lambda (l) (split-sequence l #\  #\, #\- #\>))))
       (modules (make-hash-table :size (length input))))
  (loop for (name . destinations) in input
        do (setf (gethash (to-keyword (subseq name 1)) modules)
                 (ecase (char name 0)
                   (#\b (make-broadcaster (mapcar #'to-keyword destinations)))
                   (#\% (make-flip-flop (mapcar #'to-keyword destinations)))
                   (#\& (make-conjunction (mapcar #'to-keyword destinations))))))
  (loop for (name . destinations) in input
        do (loop for dest in destinations
                 for dest-key = (to-keyword dest)
                 if (eql (type-of (gethash dest-key modules)) 'conjunction)
                 do (add-input (gethash dest-key modules)
                               (to-keyword (subseq name 1)))))
  (format t "~D~&" (loop repeat 1000
                         sum (count-pulses modules) into sum
                         finally (return (* (realpart sum) (imagpart sum)))))
  (quit) ; takes way too long
  (format t "~D~&" (loop for i from 1001
                         until (target-rx modules)
                         finally (return i))))
