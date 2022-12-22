
(ql:quickload '(:fiveam :str))

(defpackage :day10
  (:import-from :fiveam :test :is-true :is-false)
  (:export :solution-to-part-1)
   (:use :cl))

(in-package :day10)

(setf fiveam:*run-test-when-defined* t)
(declaim (optimize (debug 3)))

(defun get-contents ()
"read in the input data in a list, 1 line per entry"
  (with-open-file (stream "day10_input")
        (loop for line = (read-line stream nil)
            while line collect line)))

(defconstant +instructions+ (get-contents))

(defstruct cpu-state (cycle 1 :type integer) (register-x 1 :type integer))

(defclass cpu ()
  ((state :accessor cpu-state :initform (make-cpu-state))
   (trace-cycles :initarg :trace-cycles :accessor cpu-trace-cycles :initform nil)
   (trace-result :accessor cpu-trace-result :initform nil)))

(defgeneric load-instructions (cpu instructions))
(defmethod load-instructions ((cpu cpu) instructions)
  (setf (cpu-trace-result cpu) nil)
  (dolist (instruction instructions)
    (execute cpu instruction)))


(defgeneric execute (cpu instruction))
(defmethod execute ((cpu cpu) instruction)
  (let* ((parts (str:split " " instruction))
         (current-cycle (cpu-state-cycle (cpu-state cpu)))
         (register-x (cpu-state-register-x (cpu-state cpu)))
         (cycles (list (make-cpu-state :cycle (incf current-cycle)
                                       :register-x register-x)))
         (operation (first parts)))
         (unless (string= operation "noop")
           (let ((value (parse-integer (second parts))))
                   (push (make-cpu-state :cycle (incf current-cycle)
                                         :register-x (incf register-x value))
                         cycles)))
         (setf (cpu-state cpu) (first cycles))
         (reverse cycles)))

(defmethod execute :around ((cpu cpu) instruction)
  (let ((states (call-next-method)))
    (dolist (state states states)
      (unless (null state)
        (progn
          (multiple-value-bind (_ r) (floor (cpu-state-cycle state) 40) (declare (ignore _))
            (cond
                ((= 1 r)
                 (format t "~%~a" (if (pixel-visible-p state) #\# #\.)))
                ('otherwise
                 (format t "~a" (if (pixel-visible-p state) #\# #\.))))
            (if (member (cpu-state-cycle state) (cpu-trace-cycles cpu))
                (push state (cpu-trace-result cpu)))))))))

(defgeneric pixel-visible (state))
(defmethod pixel-visible-p ((state cpu-state))
  (let ((cycle (rem (1- (cpu-state-cycle state)) 40))
        (x (cpu-state-register-x state)))
    (or (= cycle (1- x)) (= cycle x) (= cycle (1+ x)))))

(defgeneric signal-strength (cpu-state))
(defmethod signal-strength ((state cpu-state))
  (* (cpu-state-cycle state) (cpu-state-register-x state)))

(defconstant +part1-test-instructions+ (list "addx 15" "addx -11" "addx 6" "addx -3" "addx 5" "addx -1" "addx -8" "addx 13"
                                              "addx 4" "noop" "addx -1" "addx 5" "addx -1" "addx 5" "addx -1" "addx 5" "addx -1"
                                              "addx 5" "addx -1" "addx -35" "addx 1" "addx 24" "addx -19" "addx 1" "addx 16" "addx -11"
                                              "noop" "noop" "addx 21" "addx -15" "noop" "noop" "addx -3" "addx 9" "addx 1" "addx -3"
                                              "addx 8" "addx 1" "addx 5" "noop" "noop" "noop" "noop" "noop" "addx -36" "noop" "addx 1"
                                              "addx 7" "noop" "noop" "noop" "addx 2" "addx 6" "noop" "noop" "noop" "noop" "noop" "addx 1"
                                              "noop" "noop" "addx 7" "addx 1" "noop" "addx -13" "addx 13" "addx 7" "noop" "addx 1" "addx -33"
                                              "noop" "noop" "noop" "addx 2" "noop" "noop" "noop" "addx 8" "noop" "addx -1" "addx 2" "addx 1"
                                              "noop" "addx 17" "addx -9" "addx 1" "addx 1" "addx -3" "addx 11" "noop" "noop" "addx 1" "noop"
                                              "addx 1" "noop" "noop" "addx -13" "addx -19" "addx 1" "addx 3" "addx 26" "addx -30" "addx 12"
                                              "addx -1" "addx 3" "addx 1" "noop" "noop" "noop" "addx -9" "addx 18" "addx 1" "addx 2" "noop"
                                              "noop" "addx 9" "noop" "noop" "noop" "addx -1" "addx 2" "addx -37" "addx 1" "addx 3" "noop"
                                              "addx 15" "addx -21" "addx 22" "addx -6" "addx 1" "noop" "addx 2" "addx 1" "noop" "addx -10"
                                              "noop" "noop" "addx 20" "addx 1" "addx 2" "addx 2" "addx -6" "addx -11" "noop" "noop" "noop"))




(test test-instructions-to-part-1-should-return-13140
  (is-true (= 13140 (let ((cpu (make-instance 'cpu :trace-cycles (list 20 60 100 140 180 220))))
                            (load-instructions cpu +part1-test-instructions+)
                            (reduce #'+ (mapcar #'signal-strength (cpu-trace-result cpu)))))))

(defun solution-to-part-1 ()
  (let ((cpu (make-instance 'cpu :trace-cycles (list 20 60 100 140 180 220))))
    (load-instructions cpu +instructions+)
    (reduce #'+ (mapcar #'signal-strength (cpu-trace-result cpu)))))

(format t "~%~a~%" (solution-to-part-1))
;; solution to part 2 appears as ZRARLFZU
;; ####.###...##..###..#....####.####.#..#.
;; ...#.#..#.#..#.#..#.#....#.......#.#..#.
;; ..#..#..#.#..#.#..#.#....###....#..#..#.
;; .#...###..####.###..#....#.....#...#..#.
;; #....#.#..#..#.#.#..#....#....#....#..#.
;; ####.#..#.#..#.#..#.####.#....####..##..
