
(defun get-contents () (with-open-file (stream "day4_input")
                         (loop for line = (read-line stream nil)
                               while line collect line)))

(defun one-covered-by-otherp (range1 range2)
  "returns whether one of these ranges completely covers the other"
  (let ((range1-start (get-start range1)) (range1-end (get-end range1)) (range2-start (get-start range2)) (range2-end (get-end range2)))
    (or (and (<= range1-start range2-start) (>= range1-end range2-end))
        (and (<= range2-start range1-start) (>= range2-end range1-end)))))

(defun any-range-at-least-partially-overlaps-the-otherp (range1 range2)
  "returns whether there's at least a partial overlap between the ranges"
  (let* ((range1-start (get-start range1)) (range1-end (get-end range1))
         (range2-start (get-start range2)) (range2-end (get-end range2))
         (biggest-start (if (> (- range1-end range1-start) (- range2-end range2-start)) range1-start range2-start))
         (biggest-end (if (> (- range1-end range1-start) (- range2-end range2-start)) range1-end range2-end))
         (smaller-start (if (> (- range1-end range1-start) (- range2-end range2-start)) range2-start range1-start))
         (smaller-end (if (> (- range1-end range1-start) (- range2-end range2-start)) range2-end range1-end)))
    (loop for c from biggest-start to biggest-end
          when (or (equal c smaller-start) (equal c smaller-end))
            return t)))

(defun get-start (range)
  "get the first part of the range as an integer"
  (parse-integer (first (uiop:split-string range :separator "-" ))))

(defun get-end (range)
  "get the last part of the range as an integer"
  (parse-integer (second (uiop:split-string range :separator "-" ))))


(defun how-many-fully-contain-the-other? ()
  "solution to part 1"
  (length (remove-if #'null (loop for line in (get-contents)
        for ranges = (uiop:split-string line :separator ",")
        collect (one-covered-by-otherp (first ranges) (second ranges))))))

(defun how-many-partially-contain-the-other? ()
  "solution to part 2"
  (length (remove-if #'null (loop for line in (get-contents)
        for ranges = (uiop:split-string line :separator ",")
        collect (any-range-at-least-partially-overlaps-the-otherp (first ranges) (second ranges))))))
