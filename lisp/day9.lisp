
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

(defstruct grid-coordinate
  (row 0 :type integer)
  (column 0 :type integer))

(defconstant +starting-position+ (make-grid-coordinate :row 0 :column 0))
(defstruct knot
  (position +starting-position+ :type grid-coordinate)
  (knot-number 0 :type integer))


(defstruct board (knots))

(defun create-board-sequence (board command repetition)
  "determine which action to perform and perform it REPETITION times,
   returning list of sequential positions taken on the board"
  (let ((sequence))
    (dotimes (_ repetition)
        (push (cond ((string= command "D") (perform-action #'move-down board))
                    ((string= command "L") (perform-action #'move-left board))
                    ((string= command "R") (perform-action #'move-right board))
                    ((string= command "U") (perform-action #'move-up board)))
            sequence)
        (setf board (first sequence)))
    sequence))

(defun perform-action (action board)
  "move the head and move the head subsequently afterwards,
   return a new board with these positions"
  (let ((new-knots)
        (current-knots (board-knots board))) ;; for head just perform move action
    (push (make-knot :position (funcall action (knot-position (first current-knots)))) new-knots)
    ;; adjust the following tail dependent on preceding head or segment position
    (do ((x 1 (1+ x))) ((>= x (length current-knots)) (make-board :knots new-knots))
      (setf new-knots (append (copy-seq new-knots)
                              (list (if (positions-adjacent-p (knot-position (nth x current-knots)) (knot-position (nth (- x 1) new-knots)))
                                          (copy-knot (nth x current-knots)) ;; positions are identical, stay put
                                        (move-closer-again (nth (1- x) new-knots) (nth x current-knots))))))))) ;; positions differ take predecessors old position


(defun move-closer-again (head tail)
  (let ((head-row (grid-coordinate-row (knot-position head)))
        (head-column (grid-coordinate-column (knot-position head)))
        (tail-row (grid-coordinate-row (knot-position tail)))
        (tail-column (grid-coordinate-column (knot-position tail))))
    (make-knot :knot-number (knot-knot-number tail)
               :position (make-grid-coordinate :row (+ tail-row (signum (- head-row tail-row)))
                                               :column (+ tail-column (signum (- head-column tail-column)))))))

(defun move-up (starting-position)
  "calculates and returns the new position after moving up 1"
  (make-grid-coordinate :row (- (grid-coordinate-row starting-position) 1) :column (grid-coordinate-column starting-position)))

(test move-up-1-row
  (is-true (grid-coordinate= (make-grid-coordinate :row 0 :column 0) (move-up (make-grid-coordinate :row 1 :column 0)))))

(defun move-down (starting-position)
  "calculates and returns the new position after moving down 1"
  (make-grid-coordinate :row (1+ (grid-coordinate-row starting-position)) :column (grid-coordinate-column starting-position)))

(test move-down-1-row
  (is-true (grid-coordinate= (make-grid-coordinate :row 1 :column 0) (move-down (make-grid-coordinate :row 0 :column 0)))))

(defun move-left (starting-position)
  "calculates and returns the new position after moving left 1"
  (make-grid-coordinate :row (grid-coordinate-row starting-position) :column (- (grid-coordinate-column starting-position) 1)))

(test move-left-1-row
  (is-true (grid-coordinate= (make-grid-coordinate :row 0 :column 6) (move-left (make-grid-coordinate :row 0 :column 7)))))

(defun move-right (starting-position)
  "calculates and returns the new position after moving right 1"
  (make-grid-coordinate :row (grid-coordinate-row starting-position) :column (1+ (grid-coordinate-column starting-position))))

(test move-right-1-row
  (is-true (grid-coordinate= (make-grid-coordinate :row 0 :column 1)  (move-right (make-grid-coordinate :row 0 :column 0)))))

(defun grid-coordinate= (one two)
  "predicate to determine equality between two grid-coordinate"
  (unless (and (null one) (null two))
    (and (= (grid-coordinate-row one) (grid-coordinate-row two))
       (= (grid-coordinate-column one) (grid-coordinate-column two)))))

(test equality-of-grid-coordinates
  (is-true (grid-coordinate= +starting-position+ +starting-position+))
  (is-false (grid-coordinate= +starting-position+ (make-grid-coordinate :row 99 :column 0))))

(defun positions-adjacent-p (position1 position2)
  "predicate to determine if 2 positions are adjacent to each other"
  (or (grid-coordinate= position1 position2)
      (grid-coordinate= position1 (move-left position2))
      (grid-coordinate= position1 (move-right position2))
      (grid-coordinate= position1 (move-up position2))
      (grid-coordinate= position1 (move-down position2))
      (grid-coordinate= position1 (move-up (move-left position2)))
      (grid-coordinate= position1 (move-up (move-right position2)))
      (grid-coordinate= position1 (move-down (move-left position2)))
      (grid-coordinate= position1 (move-down (move-right position2)))))

(test positions-too-far-away-not-adjacent
  (is-false (positions-adjacent-p (make-grid-coordinate :row 0 :column 0) (make-grid-coordinate :row 1 :column 3))))

(test identical-positions-count-as-adjacent
  (is-true (positions-adjacent-p +starting-position+ +starting-position+)))

(test position-left-of-other-is-adjacent
  (is-true (positions-adjacent-p (make-grid-coordinate :row 0 :column 1) (make-grid-coordinate :row 0 :column 2))))

(test position-right-of-other-is-adjacent
  (is-true (positions-adjacent-p (make-grid-coordinate :row 0 :column 2) (make-grid-coordinate :row 0 :column 1))))

(test position-above-other-is-adjacent
  (is-true (positions-adjacent-p (make-grid-coordinate :row 1 :column 0) (make-grid-coordinate :row 0 :column 0))))

(test position-below-other-is-adjacent
  (is-true (positions-adjacent-p (make-grid-coordinate :row 0 :column 0) (make-grid-coordinate :row 1 :column 0))))

(test position-below-and-left-of-other-is-adjacent
  (is-true (positions-adjacent-p (make-grid-coordinate :row 1 :column 0) (make-grid-coordinate :row 0 :column 1))))

(test position-below-and-right-of-other-is-adjacent
  (is-true (positions-adjacent-p (make-grid-coordinate :row 1 :column 2) (make-grid-coordinate :row 0 :column 1))))

(test position-above-and-left-of-other-is-adjacent
  (is-true (positions-adjacent-p (make-grid-coordinate :row 0 :column 0) (make-grid-coordinate :row 1 :column 1))))

(test position-above-and-right-of-other-is-adjacent
  (is-true (positions-adjacent-p (make-grid-coordinate :row 0 :column 2) (make-grid-coordinate :row 1 :column 1))))


(defun count-positions-tail-visits (contents tail-number starting-board)
  "returns the number of positions our tail takes"
    (let* ((current-board starting-board)
           (tail-positions-found)
           (boards (list current-board)))
    (dolist (line contents)
        (let* ((values (str:split " " line))
            (board-sequence (create-board-sequence current-board (first values) (parse-integer (second values)))))
        ;;(format t "~a~%~a~%~%" line board-sequence)
        (dolist (board board-sequence)
            (unless (member board boards :test #'equal)
            (push board boards)))
        (setf current-board (first board-sequence))))
    (dolist (board boards)
      (unless (or (null (nth tail-number (board-knots board))) (member (knot-position (nth tail-number (board-knots board))) tail-positions-found :test #'grid-coordinate=))
        (push (knot-position (nth tail-number(board-knots board))) tail-positions-found)))
    (length tail-positions-found)))

(test solution-to-part-1-verification-of-example-should-be-13
  (let ((test-contents (list "R 4" "U 4" "L 3" "D 1" "R 4" "D 1" "L 5" "R 2")))
    (is-true (= 13 (count-positions-tail-visits test-contents 1
                                       (make-board :knots (list (make-knot :position +starting-position+ :knot-number 0)
                                                                (make-knot :position +starting-position+ :knot-number 1))))))))

(test solution-to-part-2-verification-of-example-should-be-36
  (let ((test-contents (list "R 5" "U 8" "L 8" "D 3" "R 17" "D 10" "L 25" "U 20")))
    (is-true (= 36 (count-positions-tail-visits test-contents 9
                                       (make-board :knots (list (make-knot :position +starting-position+ :knot-number 0)
                                                                (make-knot :position +starting-position+ :knot-number 1)
                                                                (make-knot :position +starting-position+ :knot-number 2)
                                                                (make-knot :position +starting-position+ :knot-number 3)
                                                                (make-knot :position +starting-position+ :knot-number 4)
                                                                (make-knot :position +starting-position+ :knot-number 5)
                                                                (make-knot :position +starting-position+ :knot-number 6)
                                                                (make-knot :position +starting-position+ :knot-number 7)
                                                                (make-knot :position +starting-position+ :knot-number 8)
                                                                (make-knot :position +starting-position+ :knot-number 9))))))))

(format t "solution to part 1: ~a~%" (count-positions-tail-visits (get-contents) 1
                                       (make-board :knots (list (make-knot :position +starting-position+ :knot-number 0)
                                                                (make-knot :position +starting-position+ :knot-number 1)))))

(format t "solution to part 2: ~a~%" (count-positions-tail-visits (get-contents) 9
                                       (make-board :knots (list (make-knot :position +starting-position+ :knot-number 0)
                                                                (make-knot :position +starting-position+ :knot-number 1)
                                                                (make-knot :position +starting-position+ :knot-number 2)
                                                                (make-knot :position +starting-position+ :knot-number 3)
                                                                (make-knot :position +starting-position+ :knot-number 4)
                                                                (make-knot :position +starting-position+ :knot-number 5)
                                                                (make-knot :position +starting-position+ :knot-number 6)
                                                                (make-knot :position +starting-position+ :knot-number 7)
                                                                (make-knot :position +starting-position+ :knot-number 8)
                                                                (make-knot :position +starting-position+ :knot-number 9)))))
