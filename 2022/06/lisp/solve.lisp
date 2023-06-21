(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun find-marker (marker-length data)
  (loop for i = 0 then (+ i 1 dupe)
        for candidate = (subseq data i (+ i marker-length))
        for dupe = (loop for c across candidate
                         for j from 0
                         with dupe = nil
                         if (find c candidate :start (1+ j))
                           do (setf dupe j)
                         finally
                           (return dupe))
        if (not dupe)
          return (+ i marker-length)
        )
  )

(let ((data (car (parse-input))))
  (format t "~D~&" (find-marker 4 data))
  (format t "~D~&" (find-marker 14 data))
  )
