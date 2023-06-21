(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defparameter *rocks* '#1=((#( t   t   t   t ))
                           (#(nil  t  nil)
                            #( t   t   t )
                            #(nil  t  nil))
                           (#(nil nil  t )
                            #(nil nil  t )
                            #( t   t   t ))
                           (#( t )
                            #( t )
                            #( t )
                            #( t ))
                           (#( t   t )
                            #( t   t ))
                           . #1#))

(defparameter *shaft*  #(t nil nil nil nil nil nil nil t))
(defparameter *ground* #(t  t   t   t   t   t   t   t  t))

(defun shaft ()
  (copy-seq *shaft*)
  )

(defun collides (rock tower offset)
  (loop for rslice in rock
        for tslice in tower
        thereis (loop for rpiece across rslice
                      for tpiece across (make-array (array-dimensions rslice)
                                                    :displaced-to tslice
                                                    :displaced-index-offset offset)
                      thereis (and rpiece tpiece)))
  )

(defun place (rock tower offset)
  (loop for rslice in rock
        for tslice in tower
        do (loop with tslice-adj = (make-array (array-dimensions rslice)
                                               :displaced-to tslice
                                               :displaced-index-offset offset)
                 for i from 0 below (array-dimension rslice 0)
                 if (aref rslice i)
                   do (setf (aref tslice-adj i) t)))
  )

(defun adjust-top (tower required)
  (cond
    ((and (<= required  0) (not (equalp *shaft* (car tower))))
     tower)
    ((<= required 0)
     (adjust-top (cdr tower) required))
    ((equalp *shaft* (car tower))
     (cons (car tower) (adjust-top (cdr tower) (1- required))))
    (t
     (cons (shaft) (adjust-top tower (1- required)))))
  )

(defun simulate (pattern total-rocks)
  (loop with offset = 3
        with nrocks = 0
        with rocks = *rocks*
        with tower = (list (shaft) (shaft) (shaft) (shaft) *ground*)
        with tower-position = tower
        for p in pattern
        for rock = (car rocks)
        until (>= nrocks total-rocks)
        ; move horizontally
        unless (collides rock tower-position (+ offset p))
          do (setf offset (+ offset p))
        ; move vertically
        if (collides rock (cdr tower-position) offset)
          do (place rock tower-position offset)
             (incf nrocks)
             (setf rocks (cdr rocks))
             (setf tower (adjust-top tower (+ 3 (length (car rocks)))))
             (setf tower-position tower)
             (setf offset 3)
        else
          do (setf tower-position (cdr tower-position))
        finally (return tower)
        )
  )

(defun tower-height (pattern total-rocks)
  (1- (length (adjust-top (simulate pattern total-rocks) 0)))
  )

(let ((pattern (map 'list
                    (lambda (c)
                      (case c (#\> 1) (#\< -1))
                      )
                    (car (parse-input)))))
  (setf (cdr (last pattern)) pattern)
  (setf *print-circle* t)
  (format t "~D~&" (tower-height pattern 2022))
  ;(format t "~D~&" (tower-height pattern 1000000000000))
  ; TODO implementation does not scale to 1000000000000. after a certain height
  ; the tower is periodic but we need to determine the offset and period length
  )
