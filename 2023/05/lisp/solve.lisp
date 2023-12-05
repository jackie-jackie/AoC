(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun do-map (sources map)
  (loop for source in sources
        collect (loop for (m-dst m-src m-len) in map
                      if (< (1- m-src) source (+ m-src m-len))
                      do (return (+ m-dst (- source m-src)))
                      finally (return source))))

(defun do-map-ranges (sources map)
  (loop for (start length) on sources by #'cddr
        for end = (+ start length)
        append (loop for (m-dst m-src m-len) in map
                     for m-end = (+ m-src m-len)
                     if (< start m-src end)          ; split prefix
                     append (do-map-ranges (list start (- m-src start)) map) into res
                     and do (decf length (- m-src start))
                     and do (setf start m-src)
                     if (< start m-end end)          ; split suffix
                     append (do-map-ranges (list m-end (- end m-end)) map) into res
                     and do (decf length (- end m-end))
                     and do (setf end m-end)
                     if (<= m-src start end m-end)   ; range fully in map
                     do (return (cons (+ start (- m-dst m-src)) (cons length res)))
                     finally (return (cons start (cons length res))))))

(let ((seeds (mapcar #'parse-integer (cdar (parse-input :pre #'split-space :until ""))))
      (maps (loop for map = (parse-input :pre #'split-space :until "")
                  while map
                  collect (mapcar (lambda (m) (mapcar #'parse-integer m)) (rest map)))))
  (format t "~D~&" (reduce #'min (reduce #'do-map maps :initial-value seeds)))
  (format t "~D~&" (loop for location in (reduce #'do-map-ranges
                                                 maps
                                                 :initial-value seeds)
                         by #'cddr
                         minimize location)))
