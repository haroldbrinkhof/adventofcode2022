(ql:quickload '(:fiveam :str))

(defpackage :day8
  (:import-from :fiveam :test :is-true :is-false)
  (:export :count-all-visible-trees )
   (:use :cl))

(in-package :day8)

(setf fiveam:*run-test-when-defined* t)
(declaim (optimize (debug 3)))

(defun get-contents ()
"read in the input data in a list, 1 line per entry"
  (with-open-file (stream "day8_input")
        (loop for line = (read-line stream nil)
            while line collect line)))


(defconstant +test-contents+
  (list "30373"
        "25512"
        "65332"
        "33549"
        "35390"))

#|
    The Elves have already launched a quadcopter to generate a map
    with the height of each tree (your puzzle input). For example:

    30373
    25512
    65332
    33549
    35390

    Each tree is represented as a single digit whose value is its height,
    where 0 is the shortest and 9 is the tallest.

    A tree is visible if all of the other trees between it and an edge
    of the grid are shorter than it. Only consider trees in the same row
    or column; that is, only look up, down, left, or right from any given
    tree.

    With 16 trees visible on the edge and another 5 visible in the
    interior, a total of 21 trees are visible in this test arrangement.
|#

(defun check-visibility-p (directional-lookup-function tree-grid row column)
  "loop over every value in a given direction and check that the tree at the
   current position is bigger than them all"
  (let ((current-row (nth row tree-grid)))
    (every (lambda (value) (> (digit-char-p (elt current-row column)) value))
             (funcall directional-lookup-function tree-grid row column))))

(defun look-down (tree-grid row column)
  "gather all values from the rows below the current column,
   not including the column on the current row"
  (loop for current-row from (1+ row) below (length tree-grid)
        collect (digit-char-p (elt (nth current-row tree-grid) column))))

(defun look-up (tree-grid row column)
  "gather all values from the rows above the current column,
   not including the column on the current row"
  (loop for current-row from (- row 1) downto 0
        collect (digit-char-p (elt (nth current-row tree-grid) column))))

(defun look-left (tree-grid row column)
  "gather all values to the left of the current column, not including the column"
  (loop for current-column from (- column 1) downto 0
        collect (digit-char-p (elt (nth row tree-grid) current-column))))

(defun look-right (tree-grid row column)
  "gather all values to the right of the current column, not including the column"
  (loop for current-column from (1+ column) below (length (nth row tree-grid))
        collect (digit-char-p (elt (nth row tree-grid) current-column))))

#|
    The top-left 5 is visible from the left and top.
    (It isn't visible from the right or bottom since other
    trees of height 5 are in the way.)
|#
(test visible-from-left
  (is-true (check-visibility-p #'look-left +test-contents+ 1 1)))

(test not-visible-from-left
  (is-false (check-visibility-p #'look-left +test-contents+ 1 2)))

(test outer-tree-visible-from-left
  (is-true (check-visibility-p #'look-left +test-contents+ 0 0)))

#|
    The left-middle 5 is visible, but only from the right.
|#
(test left-middle-5-visible-from-right
  (is-true (check-visibility-p #'look-right +test-contents+ 2 3)))

(test one-before-left-middle-5-not-visible-from-right
  (is-false (check-visibility-p #'look-right +test-contents+ 2 2)))

(test outer-tree-visible-from-right
  (is-true (check-visibility-p #'look-right +test-contents+ 0 4)))

#|
    The top-left 5 is visible from the left and top. (It isn't visible
    from the right or bottom since other trees of height 5 are in the way)

    The top-middle 5 is visible from the top and right.
|#
(test top-left-5-visible-from-left-and-top
  (is-true (check-visibility-p #'look-left +test-contents+ 1 1))
  (is-true (check-visibility-p #'look-up +test-contents+ 1 1)))

(test top-middle-5-visible-from-right-and-top
  (is-true (check-visibility-p #'look-right +test-contents+ 1 2))
  (is-true (check-visibility-p #'look-up +test-contents+ 1 2)))

#|
    In the bottom row, the middle 5 is visible, but the 3 and 4 are not.
|#
(test bottom-middle-5-visible
  (is-true (check-visibility-p #'look-down +test-contents+ 3 2))
  (is-false (check-visibility-p #'look-down +test-contents+ 3 1))
  (is-false (check-visibility-p #'look-down +test-contents+ 3 3)))

(test bottom-outer-layer-visible
  (is-true (check-visibility-p #'look-down +test-contents+ 4 2)))

#|
    top-right 1 is not visible from any direction
    center 3 is not visible from any direction
|#
(test top-right-1-invisible
  (is-false (tree-visible-p +test-contents+ 1 3))
  (is-false (tree-visible-p +test-contents+ 2 2)))

(defun tree-visible-p (tree-grid row column)
  "returns t if tree is visible from any direction"
  (or (check-visibility-p #'look-left tree-grid row column)
      (check-visibility-p #'look-right tree-grid row column)
      (check-visibility-p #'look-up tree-grid row column)
      (check-visibility-p #'look-down tree-grid row column)))

(defun count-all-visible-trees (tree-grid)
  "returns the number of trees visible in this tree-grid"
  (reduce #'+ (mapcar #'length (loop for row from 0 below (length tree-grid)
        collect (loop for column from 0 below (length (nth row tree-grid))
            when (tree-visible-p tree-grid row column)
                collect t)))))

#|
    a total of 21 trees are visible in this arrangement.
|#
(test found-21-trees-visible
  (is-true (= (count-all-visible-trees +test-contents+) 21)))

;;; solution to part 1
(format t "number of trees visible: ~a~%" (count-all-visible-trees (get-contents)))

#|
    To measure the viewing distance from a given tree, look up, down, left,
    and right from that tree; stop if you reach an edge or at the first tree
    that is the same height or taller than the tree under consideration.
    (If a tree is right on the edge, at least one of its viewing distances will be zero.)

    The Elves don't care about distant trees taller than those found by the rules above;
    the proposed tree house has large eaves to keep it dry, so they wouldn't be able to see
    higher than the tree house anyway.

    In the itest-contents, consider the middle 5 in the second row:

    30373
    25512
    65332
    33549
    35390

    Looking up, its view is not blocked; it can see 1 tree (of height 3).
    Looking left, its view is blocked immediately; it can see only 1 tree (of height 5, right next to it).
    Looking right, its view is not blocked; it can see 2 trees.
    Looking down, its view is blocked eventually; it can see 2 trees (one of height 3, then the tree of height 5 that blocks its view).
|#
(test looking-up-see-1
  (is-true (= 1 (calculate-scenic-score #'look-up +test-contents+ 1 2))))
(test looking-left-see-1
  (is-true (= 1 (calculate-scenic-score #'look-left +test-contents+ 1 2))))
(test looking-right-see-2
  (is-true (= 2 (calculate-scenic-score #'look-right +test-contents+ 1 2))))
(test looking-down-see-2
  (is-true (= 2 (calculate-scenic-score #'look-down +test-contents+ 1 2))))

(defun calculate-scenic-score (directional-lookup-function tree-grid row column)
  (let ((current-tree-height (digit-char-p (elt (nth row tree-grid) column))))
    (loop for height in (funcall directional-lookup-function tree-grid row column)
        sum 1 into total
        until (>= height current-tree-height)
        finally (return total))))


(defun calculate-total-scenic-score (tree-grid row column)
  "calculates the total scenic score by multiplying all scenic scores for 1 point"
  (* (calculate-scenic-score #'look-down tree-grid row column)
     (calculate-scenic-score #'look-up tree-grid row column)
     (calculate-scenic-score #'look-left tree-grid row column)
     (calculate-scenic-score #'look-right tree-grid row column)))
#|
    A tree's scenic score is found by multiplying together its viewing distance
    in each of the four directions.
    For this tree, this is 4 (found by multiplying 1 * 1 * 2 * 2).
|#
(test scenic-score-comes-out-to-4
  (is-true (= 4 (calculate-total-scenic-score +test-contents+ 1 2))))

(defun highest-scenic-score-available (tree-grid)
  "return the highest scenic score available to this grid:
    collects all scenic scores per row in lists, flattens the lists through append,
    sorts in descending order and returns the first."
  (first (sort (apply #'append (loop for row from 0 below (length tree-grid)
        collect (loop for column from 0 below (length (first tree-grid))
                    collect (calculate-total-scenic-score tree-grid row column))))
        #'>)))

(test highest-scenic-score-comes-out-to-8
  (is-true (= 8 (highest-scenic-score-available +test-contents+))))

;;; solution to part 2
(format t "highest scenic score: ~a~%" (highest-scenic-score-available (get-contents)))
