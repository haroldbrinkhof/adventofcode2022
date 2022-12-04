(defun get-contents () (with-open-file (stream "day2_input")
                         (loop for line = (read-line stream nil)
                               while line collect line)))
(defconstant rock 1)
(defconstant paper 2)
(defconstant scissors 3)
(defconstant loss 0)
(defconstant draw 3)
(defconstant win 6)

(defparameter *values* (make-hash-table :test 'equal)
  "the outcome of a round")
(setf (gethash "A X" *values*) (+ rock draw))
(setf (gethash "A Y" *values*) (+ paper win))
(setf (gethash "A Z" *values*) (+ scissors loss))
(setf (gethash "B X" *values*) (+ rock loss))
(setf (gethash "B Y" *values*) (+ paper draw))
(setf (gethash "B Z" *values*) (+ scissors win))
(setf (gethash "C X" *values*) (+ rock win))
(setf (gethash "C Y" *values*) (+ paper loss))
(setf (gethash "C Z" *values*) (+ scissors draw))

(defparameter *redirect* (make-hash-table :test 'equal)
  "the actual play that the encrypted play should redirect to to know its value")
(setf (gethash "A X" *redirect*) "A Z")
(setf (gethash "A Y" *redirect*) "A X")
(setf (gethash "A Z" *redirect*) "A Y")
(setf (gethash "B X" *redirect*) "B X")
(setf (gethash "B Y" *redirect*) "B Y")
(setf (gethash "B Z" *redirect*) "B Z")
(setf (gethash "C X" *redirect*) "C Y")
(setf (gethash "C Y" *redirect*) "C Z")
(setf (gethash "C Z" *redirect*) "C X")

(defun calculate-round-value (round)
  "gets the outcome of a round"
  (gethash round *values*))


(defun redirected-calculate-round-value (round)
  "gets the outcome of an encrypted round"
  (calculate-round-value (gethash round *redirect*)))



(defun result () (reduce #'+ (mapcar #'calculate-round-value (get-contents))))
(defun revised-result () (reduce #'+ (mapcar #'redirected-calculate-round-value (get-contents))))
