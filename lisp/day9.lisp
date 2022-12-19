
(ql:quickload '(:fiveam :str))

(defpackage :day9
  (:import-from :fiveam :test :is-true :is-false)
  (:export :count-all-visible-trees )
   (:use :cl))

(in-package :day9)

(setf fiveam:*run-test-when-defined* t)
(declaim (optimize (debug 3)))

(defun get-contents ()
"read in the input data in a list, 1 line per entry"
  (with-open-file (stream "day9_input")
        (loop for line = (read-line stream nil)
            while line collect line)))

(defconstant +rows+ 5)
(defconstant +row-start+ 0)
(defconstant +row-end+ (- +rows+ 1))
(defconstant +columns+ 6)
(defconstant +col-start+ 0)
(defconstant +col-end+ (- +columns+ 1))
(defconstant +starting-position+ (* +row-end+ +columns+))

(defstruct board
    (head +starting-position+ :type integer)
    (tail +starting-position+ :type integer))

(defstruct grid-coordinate
  (row 0 :type integer)
  (column 0 :type integer))

(defun create-board-sequence (board command repetition)
  (let ((sequence))
  (dotimes (_ repetition)
    (push (cond ((string= command "D") (move-head-down board))
           ((string= command "L") (move-head-left board))
           ((string= command "R") (move-head-right board))
           ((string= command "U") (move-head-up board))
           ('otherwise nil))
        sequence)
    (setf board (first sequence)))
  sequence))

(defun move-head (board action)
  (let* ((current-head (board-head board))
         (current-tail (board-tail board))
         (new-head-position (funcall action current-head))
         (new-tail-position (if (positions-adjacent-p new-head-position current-tail)
                                    current-tail
                                    current-head)))
         (make-board :head new-head-position :tail new-tail-position)))

(defun move-head-up (board)
  (move-head board #'move-up))

(defun move-head-down (board)
  (move-head board #'move-down))
(defun move-head-left (board)
  (move-head board #'move-left))
(defun move-head-right (board)
  (move-head board #'move-right))

(defun position-to-grid-coordinates (position)
  "transform a position to row, column coordinates"
  (multiple-value-bind (row column) (floor position +columns+)
    (make-grid-coordinate :row row :column column)))
(defun grid-coordinates-to-position (grid-coordinate)
  "transform row, column coordinates to a position"
  (+ (* +columns+ (grid-coordinate-row grid-coordinate)) (grid-coordinate-column grid-coordinate)))

(defun move-up (starting-position)
  "calculates and returns the new position after moving up 1"
  (if (> +columns+ starting-position)
      starting-position
      (- starting-position +columns+)))

(test move-up-1-row
  (is-true (= 2 (move-up 8))))

(test move-up-1-row-when-on-top-row-same-position-returned
  (is-true (= 2 (move-up 2))))

(defun move-down (starting-position)
  "calculates and returns the new position after moving down 1"
  (if (<= (* +row-end+ +columns+ ) starting-position)
      starting-position
      (+ starting-position +columns+)))

(test move-down-1-row
  (is-true (= 14 (move-down 8))))

(test move-down-1-row-when-on-bottom-row-same-position-returned
  (is-true (= 28 (move-down 28))))

(defun move-left (starting-position)
  (if (= starting-position 0)
      starting-position
      (- starting-position 1)))

(test move-left-1-row
  (is-true (= 7 (move-left 8))))

(test move-left-1-row-when-on-top-left-same-position-returned
  (is-true (= 0 (move-left 0))))

(defun move-right (starting-position)
  (if (= (1+ starting-position) (* +rows+ +columns+))
      starting-position
      (1+ starting-position)))

(test move-right-1-row
  (is-true (= 9 (move-right 8))))

(test move-right-1-row-when-on-bottom-right-same-position-returned
  (is-true (= 29 (move-right 29))))

(defun positions-adjacent-p (position1 position2)
  (or (= position1 position2)
      (= position1 (move-left position2))
      (= position1 (move-right position2))
      (= position1 (move-up position2))
      (= position1 (move-down position2))
      (= position1 (move-up (move-left position2)))
      (= position1 (move-up (move-right position2)))
      (= position1 (move-down (move-left position2)))
      (= position1 (move-down (move-right position2)))))

(test positions-too-far-away-not-adjacent
  (is-false (positions-adjacent-p 1 3)))

(test identical-positions-count-as-adjacent
  (is-true (positions-adjacent-p 1 1)))

(test position-left-of-other-is-adjacent
  (is-true (positions-adjacent-p 0 1)))

(test position-right-of-other-is-adjacent
  (is-true (positions-adjacent-p 2 1)))

(test position-above-other-is-adjacent
  (is-true (positions-adjacent-p 2 8)))

(test position-below-other-is-adjacent
  (is-true (positions-adjacent-p 7 1)))

(test position-below-and-left-of-other-is-adjacent
  (is-true (positions-adjacent-p 6 1)))

(test position-below-and-right-of-other-is-adjacent
  (is-true (positions-adjacent-p 8 1)))

(test position-above-and-left-of-other-is-adjacent
  (is-true (positions-adjacent-p 1 8)))

(test position-above-and-right-of-other-is-adjacent
  (is-true (positions-adjacent-p 3 8)))

(defun solution1 (contents)
    (let* ((current-board (make-board))
           (tail-positions-found)
           (boards (list current-board)))
    (dolist (line contents)
        (let* ((values (str:split " " line))
            (board-sequence (create-board-sequence current-board (first values) (parse-integer (second values)))))
        (dolist (board board-sequence)
            (unless (member board boards :test #'equal)
            (push board boards)))
        (setf current-board (first board-sequence))))
    (dolist (board boards)
      (unless (member (board-tail board) tail-positions-found)
        (push (board-tail board) tail-positions-found)))
    (length tail-positions-found)))

(test solution1-verification-of-example-should-be-13
  (let ((test-contents (list "R 4" "U 4" "L 3" "D 1" "R 4" "D 1" "L 5" "R 2")))
    (is-true (= 13 (solution1 test-contents)))))


;;; test works perfectly for provided example, values don't match for what's expected with input data
;;; constraints unclear, no data concerning 'field', giving up, too frustrated with lack of correct info on constraints.
;;; maybe pick up again later
(format t "solution to part 1: ~a~%" (solution1 (get-contents)))
