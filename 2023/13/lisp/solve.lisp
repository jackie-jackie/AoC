(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun mirror-index (smudges lines)
  (loop for i from 1 below (length lines)
        if (loop for l from (1- i) downto 0
                 for r from i below (length lines)
                 sum (count nil (map 'list #'char= (aref lines l) (aref lines r))) into d
                 if (> d smudges) return nil
                 finally (return (= d smudges)))
        return i
        finally (return 0)))

(defun note-summary (smudges note)
  (+ (* 100 (mirror-index smudges (car note))) (mirror-index smudges (cdr note))))

(let ((notes (loop for input = (parse-input :until "")
                   while input
                   collect (cons (coerce input 'vector)
                                 (apply (curry #'map 'vector #'list) input)))))
  (format t "~D~&" (reduce #'+ (mapcar (curry #'note-summary 0) notes)))
  (format t "~D~&" (reduce #'+ (mapcar (curry #'note-summary 1) notes))))
