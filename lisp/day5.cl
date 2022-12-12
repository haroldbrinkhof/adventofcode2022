(defparameter *stacks* (make-array '(10) :initial-element (list)))
(defconstant +spacing+ 4)

(defun get-contents () (with-open-file (stream "day5_input")
                         (loop for line = (read-line stream nil)
                               while line collect line)))

(defun parse-line (line)
"parse the input and determine what to do with it"
  (unless (string= "" (string-trim '(#\Space #\Tab #\Newline)  line)) (let ((first-character (char line 0)))
    (cond
    ((char= first-character #\m) (eval (read-from-string (to-move line))))
    ((char= first-character #\[) (fill-in-stacks line))
    ))))


(defun fill-in-stacks (line)
  "fills in the stacks that hold the initial values"
   (loop for i from 1 to (length line) by +spacing+
         for current-value = (char line i)
         for pos = (1+ (floor i +spacing+))
         unless (char= current-value #\Space)
         do  (setf (aref *stacks* pos) (append (aref *stacks* pos) (list current-value) ))
         collect pos))

(defun to-move (line)
"some simple substitutions to arrive to an s-expression"
  (format nil "(~a)" (uiop:frob-substrings (uiop:frob-substrings line '("from") ":from") '("to") ":to")))



(defun move (amount &key from to)
  (move-part-2 amount :from from :to to))

(defun move-part-1 (amount &key from to)
  (dotimes (current amount) (move-box :from from :to to)))

(defun move-part-2 (amount &key from to)
  (move-box :from from :to to :amount amount))

(defun move-box (&key from to (amount 1) )
  "moves 1 box at a time"
  (setf (aref *stacks* to) (copy-seq (append (subseq (aref *stacks* from) 0 amount) (aref *stacks* to))))
  (setf (aref *stacks* from) (subseq (aref *stacks* from) amount)))

(defun init-stacks ()
"reset the stacks and load them. Execute move lines"
  (setf *stacks* (make-array '(10) :initial-element (list)))
  (dolist (line (get-contents)) (parse-line line))
  )


(defstruct loading-dock)

(defun get-first-layer ()
  (format nil "~a~a~a~a~a~a~a~a~a"
          (first (aref *stacks* 1))
          (first (aref *stacks* 2))
          (first (aref *stacks* 3))
          (first (aref *stacks* 4))
          (first (aref *stacks* 5))
          (first (aref *stacks* 6))
          (first (aref *stacks* 7))
          (first (aref *stacks* 8))
          (first (aref *stacks* 9))))
