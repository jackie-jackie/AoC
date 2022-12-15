(load "../../../common/lisp/util.lisp")

(defun valid-field (field)
  (destructuring-bind (key value) field
    (cond
      ((string= key "byr")
       (multiple-value-bind
         (num len)
         (parse-integer value :junk-allowed t)
         (and num (= (length value) len) (>= num 1920) (<= num 2002))
         ))
      ((string= key "iyr")
       (multiple-value-bind
         (num len)
         (parse-integer value :junk-allowed t)
         (and num (= (length value) len) (>= num 2010) (<= num 2020))
         ))
      ((string= key "eyr")
       (multiple-value-bind
         (num len)
         (parse-integer value :junk-allowed t)
         (and num (= (length value) len) (>= num 2020) (<= num 2030))
         ))
      ((string= key "hgt")
       (multiple-value-bind
         (num len)
         (parse-integer value :junk-allowed t)
         (or (and num (string= (subseq value len) "cm") (>= num 150) (<= num 193))
             (and num (string= (subseq value len) "in") (>= num 59) (<= num 76))
             )
         ))
      ((string= key "hcl")
       (and (= (length value) 7)
            (char= (aref value 0) #\#)
            (every (lambda (c)
                     (or (and (char>= c #\0) (char<= c #\9))
                         (and (char>= c #\a) (char<= c #\f)))
                     )
                   (subseq value 1))))
      ((string= key "ecl")
       (find value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))
      ((string= key "pid")
       (= 9 (length value) (nth-value 1 (parse-integer value :junk-allowed t))))
      )
    )
  )

(let ((passports (loop for passport = (parse-input :pre #'split-space :until "")
                       while passport
                       collect (remove "cid"
                                       (mapcar (lambda (field)
                                                 (split-sequence field #\:))
                                               (flatten passport))
                                       :key #'car
                                       :test #'string=))))
  (format t "~D~&" (loop for p in passports
                         count (= (length p) 7)))
  (format t "~D~&" (loop for p in passports
                         count (and (= (length p) 7)
                                    (every #'valid-field p))))
  )
