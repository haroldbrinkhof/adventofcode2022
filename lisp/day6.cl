
(defun get-contents () (with-open-file (stream "day6_input")
                         (car (loop for line = (read-line stream nil)
                               while line collect line))))

(defun first-fully-unique-packet-at (stream packet-size)
  "traverses a stream one character at a time
    until it finds the first subpart of a
    certain given size that contains fully unique characters"
  (loop for current-position from 0 to (length stream)
        for end-position = (+ current-position packet-size)
        for packet-contents = (subseq stream current-position end-position)
        unless (identical-characters-inp packet-contents)
          return end-position))

(defun identical-characters-inp (value)
  "predicate indicating whether value contains non-unique characters"
    (loop for current-character across (sort (copy-seq value) #'char-lessp)
          ;; nifty trick that allows you to have the previous character assigned
          and previous-character = nil then current-character
          if (equal current-character previous-character)
            return t))


(defun first-marker-starts-at ()
  "solution to first part"
  (first-fully-unique-packet-at (get-contents) 4))

(defun first-message-starts-at ()
  "solution to second part"
  (first-fully-unique-packet-at (get-contents) 14))
