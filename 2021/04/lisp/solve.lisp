(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun score (board draw)
  (* (reduce #'+ (remove nil (coerce (make-array (array-total-size board)
                                                 :displaced-to board)
                                     'list)))
     draw))

(defun mark (board draw)
  (loop for x from 0 below (array-dimension board 0)
        thereis (loop for y from 0 below (array-dimension board 1)
                      thereis (if (eql (aref board x y) draw)
                                  (progn
                                    (setf (aref board x y) nil)
                                    (or (loop for x from 0 below (array-dimension board 0)
                                              never (aref board x y)
                                              finally (return (score board draw)))
                                        (loop for y from 0 below (array-dimension board 1)
                                              never (aref board x y)
                                              finally (return (score board draw))))
                                    )
                                  nil))))

(defmacro mark-all (boards draw)
  `(loop for board in ,boards
         for score = (mark board ,draw)
         if score collect score into scores
         else collect board into new-boards
         finally (setf ,boards new-boards) (return (car scores))))

(let ((draws (car (parse-input :until ""
                               :pre (lambda (l)
                                      (mapcar #'parse-integer (split-sequence l #\,))))))
      (boards (loop for b = (parse-input :until ""
                                         :pre (lambda (l)
                                                (mapcar #'parse-integer (split-space l))))
                    while b
                    collect (make-array (list (length b) (length b))
                                        :initial-contents b))))
  (format t "~D~&" (loop for (draw . rest) on draws
                         do (setf draws rest)
                         thereis (mark-all boards draw)))
  (format t "~D~&" (loop for draw in draws
                         for score = (mark-all boards draw)
                         unless boards return score)))
