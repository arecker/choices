(defpackage :choices
  (:use :common-lisp))

(in-package :choices)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defvar *d/choices* '((:text "Write code.")
		      (:text "Sleep.")))

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
