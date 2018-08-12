(defpackage :choices
  (:use :common-lisp))

(in-package :choices)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-read-number (prompt &key (error-msg "Invalid response.~%"))
  (let ((response (parse-integer (prompt-read prompt) :junk-allowed t)))
    (loop while (not response)
       do
	 (format t error-msg)
	 (setf response (parse-integer (prompt-read prompt) :junk-allowed t)))
    response))

(defun prompt-read-number-in-range (prompt max &key (min 0) (error-msg "Invalid response.~%"))
  (let ((response (prompt-read-number prompt :error-msg error-msg)))
    (loop while (not (and (<= response max)
			  (> response min)))
       do
	 (format t error-msg)
	 (setf response (prompt-read-number prompt :error-msg error-msg)))
    response))

(setf *d/choices* `((:text "Write code." :callback ,#'(lambda () (format t "OK, then go write code.~%" )))
		    (:text "Sleep." :callback ,#'(lambda () (format t "OK, then go sleep.~%")))))

(defun enumerate-plist (plist)
  (loop
     for item in plist
     for i from 1 to (length plist)
     collect (concatenate 'list (list :index i) item)))

(defun format-choice (choice)
  (format nil "~@[~a. ~]~a"
	  (getf choice :index)
	  (getf choice :text)))

(defun format-choices (choices &key (enumerate t))
  (let ((choices (if enumerate (enumerate-plist choices) choices)))
  (with-output-to-string (str)
    (dolist (choice (mapcar #'format-choice choices))
      (write-line choice str)))))

(defun choose (choices &key (enumerate t) (prompt "Choice"))
  (let ((choice nil))
    (loop while (not choice)
       do
	 (format t (format-choices choices :enumerate enumerate))
	 (setf choice (prompt-read-number-in-range prompt (length choices))))
    (setf choice (nth (- choice 1) choices))
    (funcall (getf choice :callback))))
