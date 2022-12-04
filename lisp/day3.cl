
(defun get-contents () (with-open-file (stream "day3_input")
                         (loop for line = (read-line stream nil)
                               while line collect line)))
(defun get-shared-value-between-3-lines (line1 line2 line3)
  "finds the matching unique character between the 3 strings"
  (loop for c across line1
        when (and (holds-value-p c line2) (holds-value-p c line3))
          return (calculate-item-value c)))

(defun get-shared-value (line)
  "get the value of the shared value in 1 line"
  (loop for c across line
        when (holds-value-p c (subseq line (/ (length line) 2)))
          return (calculate-item-value c)))

(defun holds-value-p (value text)
  "return wether text holds value"
  (loop for c across text
        when (equal value c)
          return t))


(defun calculate-item-value (item)
  "calculate the value of an item"
  (let ((character-code (char-code item)))
  (cond
               ((>= character-code 97) (- character-code 96))
               ((>= character-code 65) (- character-code 38))
               ('otherwise 0))))

(defun first-solution ()
  "calculates the solution to part1"
  (reduce #'+ (mapcar #'get-shared-value (get-contents))))

(defun second-solution ()
  "solution to the second part"
  (reduce #'+ (group-content-per-three-and-find-shared-item-value)))

(defun group-content-per-three-and-find-shared-item-value ()
  "groups lines from input per 3, searches for the common value between them and returns the value"
  (loop for c from 0 to (- (/ (length (get-contents)) 3) 1)
                collect (let ((position (* 3 c)))
                     (get-shared-value-between-3-lines
                     (elt (get-contents) position)
                     (elt (get-contents) (1+ position))
                     (elt (get-contents) (+ 2 position))))))
