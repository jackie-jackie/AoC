(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun hand-cards (hand &optional joker-wildcard)
  (if joker-wildcard (second hand) (first hand)))

(defun hand-type (hand &optional joker-wildcard)
  (if joker-wildcard (fourth hand) (third hand)))

(defun hand-bid (hand)
  (fifth hand))

(defun cards-type (cards &optional joker-wildcard)
  "Get an integer representation of the type of the set of cards."
  (let ((counts (sort (map 'list
                           (lambda (c) (count c cards))
                           (remove-duplicates (sort (if joker-wildcard
                                                        (remove #\J cards)
                                                        (copy-seq cards))
                                                    #'char<)))
                      #'>)))
    (if (zerop (length counts)) (push 5 counts)
        (incf (first counts) (- 5 (reduce #'+ counts))))
    (reduce #'+ (mapcar #'* counts counts))))

(defun cards-number (cards &optional joker-wildcard)
  "Get an integer representation of the set of card maintaining sort order."
  (let ((card-order (if joker-wildcard "J23456789TQKA""23456789TJQKA")))
    (parse-integer (format nil "~{~X~}" (map 'list
                                             (lambda (c) (position c card-order))
                                             cards))
                   :radix 16)))

(defun hand< (hand1 hand2 &optional joker-wildcard)
  (if (/= (hand-type hand1 joker-wildcard) (hand-type hand2 joker-wildcard))
      (< (hand-type hand1 joker-wildcard) (hand-type hand2 joker-wildcard))
      (< (hand-cards hand1 joker-wildcard) (hand-cards hand2 joker-wildcard))))

(defun winnings (hands &optional joker-wildcard)
  (loop for hand in (sort hands (lambda (h1 h2) (hand< h1 h2 joker-wildcard)))
        for rank from 1
        sum (* rank (hand-bid hand))))

(let* ((hands (parse-input :pre (lambda (line)
                                  (let ((split (split-space line)))
                                    (list (cards-number (first split))
                                          (cards-number (first split) t)
                                          (cards-type (first split))
                                          (cards-type (first split) t)
                                          (parse-integer (second split))))))))
  (format t "~D~&" (winnings (copy-seq hands)))
  (format t "~D~&" (winnings hands t)))
