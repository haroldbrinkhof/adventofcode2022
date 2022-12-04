
(defun get-contents () (with-open-file (stream "day1_input")
                         (loop for line = (read-line stream nil) while line collect line)))


(defun combine-and-reduce () (let ((current 0))
                               (loop for value in (get-contents)
                                     if (not (string= value ""))
                                       do (setf current (+ current (parse-integer value)))
                                     else
                                       collect current and do (setf current 0))))


(defun get-top (&optional (number 1)) (reduce #'+ (subseq (sort (combine-and-reduce) #'>) 0 number)))
