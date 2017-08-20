;;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Base:10 -*-
;;;; *-* Last-edit: Friday, July 9, 1993  12:15:27; Edited-By: carlson *-* 
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; Copyright (c) 1990 - 1994 University of Massachusetts
;;; Department of Computer and Information Science
;;; Experimental Knowledge Systems Laboratory
;;; Professor Paul Cohen, Director.
;;; All rights reserved.
;;; 
;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee for non-commercial uses only 
;;; (not for resale), provided that the above copyright notice of EKSL, this 
;;; paragraph and the one following appear in all copies and in supporting 
;;; documentation.
;;; EKSL makes no representation about the suitability of this software for any
;;; purposes.  It is provided "AS IS", without express or implied warranties
;;; including (but not limited to) all implied warranties of merchantability
;;; and fitness for a particular purpose, and notwithstanding any other
;;; provision contained herein.  In no event shall EKSL be liable for any
;;; special, indirect or consequential damages whatsoever resulting from loss
;;; of use, data or profits, whether in an action of contract, negligence or
;;; other tortuous action, arising out of or in connection with the use or
;;; performance of this software, even if EKSL is
;;; advised of the possiblity of such damages.
;;; 
;;; For more information write to clasp-support@cs.umass.edu
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


(in-package :clasp-interface)

;;; ---------------------------------------------------------------------------
;;; Significance Result

(defclass significance (result)
  ((statistic    :initarg :statistic)
   (significance :initarg :significance))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open Significance"
    :target-form #'execute-on-target))

(defmethod show-in-results-display ((self significance))
  nil)

(defun make-significance (&rest options)
  (apply #'make-instance 'significance options))

(defun make-significance-from-list (list)
  (destructuring-bind 
    (statistic significance)
    list
    (make-significance :statistic statistic
		       :significance significance)))

(defmethod display ((significance significance) stream)
  (flet ((format-item (name)
           (let ((value (slot-value significance name)))
             (formatting-cell (stream :align-x :right)
			      (present value 
				       'nice-real
				       :stream stream))))
	 (format-label (string)
	   (formatting-cell
	    (stream :align-x :right)
	    (with-text-style
		(*label-text-style* stream)
	      (write-string string stream)))))
    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream)
      (format-label "Statistic") (format-label "Significance"))
     (formatting-row
      (stream)
      (format-item 'statistic) (format-item 'significance)))))

;;; ---------------------------------------------------------------------------
;;; T-test Result

(defclass t-test-result (modifiable-display-option-mixin significance)
  ((display-option :allocation :class :initform :display-in-interactor)
   (sample-error :initarg :sample-error)
   (dof          :initarg :dof))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open t-test"
    :target-form #'execute-on-target))

(defmethod show-in-results-display ((self t-test-result))
  t)

(defun make-t-test-result (&rest options)
  (apply #'make-instance 't-test-result options))

(defun make-t-test-result-from-list (list)
  (destructuring-bind 
    (t-statistic significance sample-error dof)
    list
    (make-t-test-result :statistic t-statistic :significance significance
			:sample-error sample-error :dof dof)))

(defmethod display ((t-test-result t-test-result) stream)
  (fresh-line stream)
  (flet ((format-item (name)
           (let ((value (slot-value t-test-result name)))
             (formatting-cell (stream :align-x :right)
			      (present value 
				       'nice-real
				       :stream stream))))
	 (format-label (string)
	   (formatting-cell
	    (stream :align-x :right)
	    (with-text-style
		(*label-text-style* stream)
	      (write-string string stream)))))
    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream)
      (format-label "T Statistic") (format-label "Significance")
      (format-label "Standard error") (format-label "Degrees of Freedom"))
     (formatting-row
      (stream)
      (format-item 'statistic) (format-item 'significance)
      (format-item 'sample-error) (format-item 'dof)))))

;;; ---------------------------------------------------------------------------
;;; d-test Result

(defclass d-test-result (modifiable-display-option-mixin significance)
  ((display-option :allocation :class :initform :display-in-interactor))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open D-test"
    :target-form #'execute-on-target))

(defmethod show-in-results-display ((self d-test-result))
  t)

(defun make-d-test-result (&rest options)
  (apply #'make-instance 'd-test-result options))

(defun make-d-test-result-from-list (list)
  (destructuring-bind 
    (d-statistic count times)
    list
    (make-d-test-result :statistic d-statistic :significance (/ count times))))

(defmethod display ((d-test-result d-test-result) stream)
  (fresh-line stream)
  (flet ((format-item (name)
           (let ((value (slot-value d-test-result name)))
             (formatting-cell (stream :align-x :right)
			      (present value 
				       'nice-real
				       :stream stream))))
	 (format-label (string)
	   (formatting-cell
	    (stream :align-x :right)
	    (with-text-style
		(*label-text-style* stream)
	      (write-string string stream)))))
    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream)
      (format-label "D Statistic") (format-label "Significance"))
     (formatting-row
      (stream)
      (format-item 'statistic) (format-item 'significance)))))

;;; ---------------------------------------------------------------------------
;;; Chi-square Result

(defclass chi-square-result (modifiable-display-option-mixin significance)
	  ((display-option :allocation :class :initform :display-in-interactor)
	   (g-test-result :initarg :g-test-result :accessor g-test-result)
	   (contingency-table :initarg :contingency-table
			      :accessor contingency-table)
	   (row-totals :initarg :row-totals :accessor row-totals)
	   (column-totals :initarg :column-totals :accessor column-totals)
	   (grand-total :initarg :grand-total :accessor grand-total)
	   (x-list :initarg :x-list :accessor x-list)
	   (y-list :initarg :y-list :accessor y-list))
  (:default-initargs
      :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open Chi-square"
    :target-form #'execute-on-target))

(defmethod show-in-results-display ((self chi-square-result))
  t)

(defun make-chi-square-result (&rest options)
  (apply #'make-instance 'chi-square-result options))

(defun make-chi-square-result-from-list (list)
  (destructuring-bind 
      (chi-square-statistic significance
       g-test-statistic g-test-significance g-test-dof
       table x-list y-list)
      list
    (let ((row-totals (make-list (array-dimension table 1) :initial-element 0))
	  (column-totals (make-list (array-dimension table 0)
				    :initial-element 0)))
      (dotimes (x (array-dimension table 0))
	(dotimes (y (array-dimension table 1))
	  (incf (nth y row-totals) (aref table x y))
	  (incf (nth x column-totals) (aref table x y))))
      (make-chi-square-result :statistic chi-square-statistic
			      :significance significance
			      :g-test-result
			      (make-g-test-result 
			       :statistic g-test-statistic
			       :significance g-test-significance
			       :dof g-test-dof)
			      :contingency-table table
			      :row-totals row-totals
			      :column-totals column-totals
			      :grand-total (reduce #'+ row-totals)
			      :x-list x-list :y-list y-list))))

(defmethod display ((self chi-square-result) stream)
  (fresh-line stream)
  (flet ((format-item (name)
           (let ((value (slot-value self name)))
             (formatting-cell
	      (stream :align-x :right)
	      (present value 'nice-real :stream stream))))
	 (format-value (value)
	   (formatting-cell
	    (stream :align-x :right)
	    (present value 'nice-real :stream stream)))
	 (format-label (value)
	   (formatting-cell
	    (stream :align-x :right)
	    (with-text-style
		(*label-text-style* stream)
	       (if (numberp value)
		   (present value 'nice-real :stream stream)
		 (write-string value stream)))))
	 (format-percent (numerator denominator)
	   (formatting-cell
	    (stream :align-x :right)
	    (present (/ numerator denominator .01) 'short-nice-real
		     :stream stream)
	    (format stream "%")))
	 (format-percent-label (numerator denominator)
	   (formatting-cell
	    (stream :align-x :right)
	    (with-text-style
		(*label-text-style* stream)
	      (present (/ numerator denominator .01) 'short-nice-real
		       :stream stream)
	      (format stream "%")))))

    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream)
      (format-label "Chi-square statistic") (format-label "Significance")
      (format-label "G-test"))
     (formatting-row
      (stream)
      (format-item 'statistic) (format-item 'significance)
      (formatting-cell
       (stream :align-x :right)
       (present (g-test-result self) 'g-test-result :stream stream))))
    
    (fresh-line stream)
    
    (with-text-style
	(*label-text-style* stream)
      (format stream "~%Observed Frequency Table~%"))
    (with-slots (x-list y-list contingency-table
		 row-totals column-totals grand-total) self
      (formatting-table
       (stream :inter-column-spacing '(2 :character))
       (formatting-row
	(stream)
	(formatting-cell (stream) (declare (ignore stream)))
	(dolist (x x-list)
	  (format-label x))
	(format-label "Totals"))
       (loop for y in y-list
	   for y-index from 0 to (1- (array-dimension contingency-table 1))
	   do
	     (formatting-row
	      (stream)
	      (format-label y)
	      (loop for x-index from 0 to
		    (1- (array-dimension contingency-table 0))
		  do
		    (format-value
		     (aref contingency-table x-index y-index)))
	      (format-label (nth y-index row-totals))))
       (formatting-row
	(stream)
	(format-label "Totals")
	(dolist (column-total column-totals)
	  (format-label column-total))
	(format-label grand-total))))
    
    (fresh-line stream)
    
    (with-text-style
	(*label-text-style* stream)
      (format stream "~%Percents of Row Totals~%"))
    (with-slots (x-list y-list contingency-table
		 row-totals column-totals grand-total) self
      (formatting-table
       (stream :inter-column-spacing '(2 :character))
       (formatting-row
	(stream)
	(formatting-cell (stream) (declare (ignore stream)))
	(dolist (x x-list)
	  (format-label x))
	(format-label "Totals"))
       (loop for y in y-list
	   for y-index from 0 to (1- (array-dimension contingency-table 1))
	   do
	     (formatting-row
	      (stream)
	      (format-label y)
	      (loop for x-index from 0 to
		    (1- (array-dimension contingency-table 0))
		  do
		    (format-percent
		     (aref contingency-table x-index y-index)
		     (nth y-index row-totals)))
	      (format-percent-label (nth y-index row-totals)
				    (nth y-index row-totals))))
       (formatting-row
	(stream)
	(format-label "Totals")
	(dolist (column-total column-totals)
	  (format-percent-label column-total grand-total))
	(format-percent-label grand-total grand-total))))
    
    (fresh-line stream)
    
    (with-text-style
	(*label-text-style* stream)
      (format stream "~%Percents of Column Totals~%"))
    (with-slots (x-list y-list contingency-table
		 row-totals column-totals grand-total) self
      (formatting-table
       (stream :inter-column-spacing '(2 :character))
       (formatting-row
	(stream)
	(formatting-cell (stream) (declare (ignore stream)))
	(dolist (x x-list)
	  (format-label x))
	(format-label "Totals"))
       (loop for y in y-list
	   for y-index from 0 to (1- (array-dimension contingency-table 1))
	   do
	     (formatting-row
	      (stream)
	      (format-label y)
	      (loop for x-index from 0 to
		    (1- (array-dimension contingency-table 0))
		  do
		    (format-percent
		     (aref contingency-table x-index y-index)
		     (nth x-index column-totals)))
	      (format-percent-label (nth y-index row-totals) grand-total)))
       (formatting-row
	(stream)
	(format-label "Totals")
	(dolist (column-total column-totals)
	  (format-percent-label column-total column-total))
	(format-percent-label grand-total grand-total))))
    
    (fresh-line stream)
    
    (with-text-style
	(*label-text-style* stream)
      (format stream "~%Expected Values~%"))
    (with-slots (x-list y-list contingency-table
		 row-totals column-totals grand-total) self
      (formatting-table
       (stream :inter-column-spacing '(2 :character))
       (formatting-row
	(stream)
	(formatting-cell (stream) (declare (ignore stream)))
	(dolist (x x-list)
	  (format-label x))
	(format-label "Totals"))
       (loop for y in y-list
	   for y-index from 0 to (1- (array-dimension contingency-table 1))
	   do
	     (formatting-row
	      (stream)
	      (format-label y)
	      (loop for x-index from 0 to
		    (1- (array-dimension contingency-table 0))
		  do
		    (format-value
		     (/ (* (nth y-index row-totals)
			   (nth x-index column-totals))
			   grand-total)))
	      (format-label (nth y-index row-totals))))
       (formatting-row
	(stream)
	(format-label "Totals")
	(dolist (column-total column-totals)
	  (format-label column-total))
	(format-label grand-total))))))

;;; ---------------------------------------------------------------------------
;;; G-test significance

(defclass g-test-result (modifiable-display-option-mixin significance)
  ((display-option :allocation :class :initform :display-in-interactor)
   (dof :initarg :dof))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open t-test"
    :target-form #'execute-on-target))

(defun make-g-test-result (&rest options)
  (apply #'make-instance 'g-test-result options))

(defun make-g-test-result-from-list (list)
  (destructuring-bind 
    (g-statistic significance dof)
    list
    (make-g-test-result :significance (make-significance
				       :statistic g-statistic
				       :significance significance)
			:dof dof)))

(defmethod display ((self g-test-result) stream)
  (fresh-line stream)
  (flet ((format-item (name)
           (let ((value (slot-value self name)))
             (formatting-cell (stream :align-x :right)
			      (present value 
				       'nice-real
				       :stream stream))))
	 (format-label (string)
	   (formatting-cell
	    (stream :align-x :right)
	    (with-text-style
		(*label-text-style* stream)
	      (write-string string stream)))))
    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream)
      (format-label "G statistic") (format-label "Significance")
      (format-label "Degrees of Freedom"))
     (formatting-row
      (stream)
      (format-item 'statistic) (format-item 'significance)
      (format-item 'dof)))))

;;; ---------------------------------------------------------------------------
;;; Scheffe table entry

(defclass scheffe-table-entry (significance) ()
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open Scheffe Table"
    :target-form #'execute-on-target))

(defun make-scheffe-table-entry (&rest options)
  (apply #'make-instance 'scheffe-table-entry options))

(defun make-scheffe-table-entry-from-list (list)
  (destructuring-bind
   (statistic significance)
   list
   (make-scheffe-table-entry :statistic statistic
			     :significance significance)))

(defmethod initialize-instance :after ((self scheffe-table-entry) &key)
  (with-slots (significance) self
    (setf (name self)
      (cond
       ((<= significance .01) "***")
       ((<= significance .05) "**")
       ((<= significance .1) "*")
       (t "")))))

(defmethod display ((scheffe-table-entry scheffe-table-entry) stream)
  (fresh-line stream)
  (present (slot-value scheffe-table-entry 'statistic)
	   '((labeled-real) :label "Scheffe Statistic")
	   :stream stream :view +dialog-view+)
  (fresh-line stream)
  (terpri)
  (present (slot-value scheffe-table-entry 'significance)
	   '((labeled-real) :label "Significance")
	   :stream stream :view +dialog-view+))

