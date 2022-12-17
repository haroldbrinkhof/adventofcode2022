(ql:quickload :str)

(defparameter *current-dir* nil) ; global holding a linked directory structure

(defstruct file name size)
(defclass dir ()
  ((name :initarg :name :documentation "the name of the directory")
   (parent :initarg :parent :initform nil :accessor parent
           :documentation "link to the parental directory node. Nil in case of root directory node.")
   (nodes :initarg :nodes :initform nil :accessor child-nodes
          :documentation "list of dir and file nodes that belong to the current dir node")))

(defgeneric is-root? (node)
  (:documentation "returns wether this node is the root directory"))
(defmethod is-root? ((node dir))
  (string= "/" (slot-value node 'name)))
(defmethod is-root? (node))

(defgeneric is-directory? (node)
  (:documentation "returns wether this node is a directory"))
(defmethod is-directory? ((node dir)) t)
(defmethod is-directory? (node))
(defun is-file? (node)
  "returns wether this node is a file"
  (not (is-directory? node)))

(defgeneric size (node)
  (:documentation
   "returns the size of a file or the size of a directory
    (meaning the sum of its files plus its subdirectories
     and their files recursively)"))
(defmethod size ((node file))
  (slot-value node 'size))
(defmethod size ((dir dir))
  (loop for node in (child-nodes dir)
        when (is-file? node)
          sum (slot-value node 'size) into total-size
        when (is-directory? node)
          sum (total-file-size node) into total-size
        finally (return total-size)))

(defgeneric sub-directories (node)
  (:documentation
   "returns the list of directories in the node list of this node"))
(defmethod sub-directories ((node dir))
  (loop for entry in (child-nodes node)
        when (is-directory? entry)
          collect entry))

(defun get-contents ()
"read in the input data in a list, 1 line per entry"
  (with-open-file (stream "day7_input")
        (loop for line = (read-line stream nil)
            while line collect line)))

(defun file-status? (line)
  "indicates wether this line represents file data"
  (str:digit? (first (str:split " " line))))

(defun add-file-status (line)
  "construct a file with its data and add it to its containing directory"
  (let ((status (str:split " " line)))
    (add-file-to-current-dir
     (make-file :name (last status)
                :size (parse-integer (first status))))))

(defun add-file-to-current-dir (file)
  (push file (child-nodes *current-dir*)))

(defun command? (line)
"indicates wether a line is a 'command' to be executed:
i.e. it starts with $"
  (string= "$" (str:s-first line)))

(defun process-command (line)
  "calls the functions the lines resolve to"
  (eval (read-from-string (extract-command line))))

(defun extract-command (line)
  "remove the starting $ and prep the rest for use with (eval ..)"
  (str:replace-first " '.." ".." (str:concat "(" (str:join " '" (str:split " " (subseq line 2))) ")")))


;;; 'commands' as seen in the input file
(defun cd (name)
  "make a dir, and store it with the rest of them. Depends on *current-dir*"
  (let ((new-dir (make-instance 'dir :name name :parent *current-dir*)))
    (unless (null *current-dir*)
      (push new-dir (child-nodes *current-dir*)))
    (setf *current-dir* new-dir)))

(defun ls () nil)

(defun cd.. ()
  "point *current-dir* to the directory node that's
   the parent of the current one, in other words: go up by 1"
  (unless (null *current-dir*)
    (setf *current-dir* (parent *current-dir*))))

(defun reset-current-dir-to-root ()
  "reset *current-dir* to point to the root directory node"
  (do () ((is-root? *current-dir*)) (cd..)))


(defun parse-and-prep ()
  "walk through the setup data and create dir and file nodes,
   point *current-dir* to them"
  (setf *current-dir* nil) ; discard any previous accumulated data
  (loop for line in (get-contents)
        when (command? line)
          do (process-command line)
        when (file-status? line)
          do (add-file-status line)))

(defun process-directories (&key selector operation)
  "walk all directories, select those that match the selector
   and perform the operation on them"
  (let ((selected))
  (do* ((dirs nil (setf dirs (append dirs (sub-directories current-dir))))
        (current-dir *current-dir* (setf current-dir (pop dirs))))
       ((null current-dir))
    (when (funcall selector current-dir)
      (push current-dir selected)))
  (funcall operation selected)))



(defun solution-part1 ()
  (parse-and-prep)
  (reset-current-dir-to-root)
  (process-directories
   :selector (lambda (dir) (<= (size dir) 100000))
   :operation (lambda (selected) (reduce #'+ (mapcar #'size selected)))))

(defun solution-part2 ()
  (parse-and-prep)
  (reset-current-dir-to-root)
  (let* ((max-space 70000000)
         (upgrade-space 30000000)
         (installed-space (size *current-dir*))
         (space-needed (- installed-space (- max-space upgrade-space))))
  (process-directories
   :selector (lambda (dir) (>= (size dir) space-needed))
   :operation (lambda (selected) (first (sort (mapcar #'size selected) #'<) )))))

(format t "solution to part 1: ~a~%solution to part2: ~a~%"
        (solution-part1)
        (solution-part2))
