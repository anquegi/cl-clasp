;;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Base:10 -*-
;;;; *-* Last-edit: Friday, July 9, 1993  12:15:43; Edited-By: carlson *-* 
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
;;; Anova One Way Table

(defclass anova-one-way-table (result)
  ((df-group :initarg :df-group)
   (ss-group :initarg :ss-group)
   (ms-group :initarg :ms-group)
   (f :initarg :f)
   (p :initarg :p)
   
   (df-error :initarg :df-error)
   (ss-error :initarg :ss-error) 
   (ms-error :initarg :ms-error)
   
   (df-total :initarg :df-total) 
   (ss-total :initarg :ss-total)
   (iv       :initarg :iv)
   (dv       :initarg  :dv))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open Anova Table"
    :target-form #'execute-on-target))

(defun make-anova-one-way-table (&rest options)
  (apply #'make-instance 'anova-one-way-table options))

(defun make-anova-one-way-table-from-list (list &rest initargs)
  (destructuring-bind 
    ((df-group ss-group ms-group f p)
     (df-error ss-error ms-error)
     (df-total ss-total))
    list
    (apply #'make-anova-one-way-table
     :df-group df-group :ss-group ss-group :ms-group ms-group 
     :f f :p p
     :df-error df-error :ss-error ss-error :ms-error ms-error
     :df-total df-total :ss-total ss-total
     initargs)))

(defmethod display ((anova-one-way-table anova-one-way-table) stream)
  (fresh-line stream)
  (flet ((format-item (name)
           (let ((value (slot-value anova-one-way-table name)))
             (formatting-cell (stream :align-x :right)
			      (present value 
				       'nice-real
				       :stream stream))))
	 (format-name (name)
	   (let ((value (slot-value anova-one-way-table name)))
	     (formatting-cell
	      (stream :align-x :right)
	      (with-text-style
		  (*label-text-style* stream)
	      (write-string (clasp::name-string value) stream)))))
	 (format-label (string)
	   (formatting-cell
	    (stream :align-x :right)
	    (with-text-style
		(*label-text-style* stream)
	      (write-string string stream)))))
    (formatting-table (stream :inter-column-spacing '(2 :character))
      (formatting-row 
       (stream)
       (format-label "Source") (format-label "DF")
       (format-label "Sum of Squares") (format-label "Mean Square")
       (format-label "F Ratio") (format-label "P"))
      (formatting-row (stream)
       (format-name 'iv) (format-item 'df-group) (format-item 'ss-group)
       (format-item 'ms-group) (format-item 'f) (format-item 'p))
      (formatting-row (stream)
       (format-label "Error") (format-item 'df-error) (format-item 'ss-error)
       (format-item 'ms-error))
      (formatting-row (stream)
       (format-label "Total") (format-item 'df-total) (format-item 'ss-total)
       ))))

;;; ---------------------------------------------------------------------------
;;; Anova One Way Result

(defclass anova-one-way-result (modifiable-display-option-mixin result)
  ((display-option :allocation :class :initform :display-in-interactor)
   (anova-table      :initarg :anova-table)
   (group-means      :initarg :group-means)
   (group-means-plot :initarg :group-means-plot)
   (scheffe-table    :initarg :scheffe-table)
   (alt-sst          :initarg :alt-sst))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open Anova"
    :target-form #'execute-on-target))

(defun make-anova-one-way-result
    (&rest initargs
     &key iv dv group-means anova-table scheffe-table
     &allow-other-keys)
  (let* ((data (make-graph-data
		:symbologies '(:scatter :line)
		:data (loop for i in group-means
			  for count = 1 then (+ count 1)
			  collect (list count i))
		:name "group means"))
	 (graph (make-graph
		 :x-label ""
		 :y-label "Group means"
		 :title "Group means from anova"))
	 (group-means-plot (make-graph-data-icon
			    :description
			    (format nil
				    "Group means plot from anova of ~a on ~a"
				    (description dv) (description iv))
			    :y-label "Group means"
			    :graph graph)))
    (add-dataset graph data)
    (apply #'make-instance 'anova-one-way-result
	   :anova-table (make-anova-one-way-table-from-list
			 anova-table :iv iv :dv dv)
	   :group-means (make-instance 'number-sequence
			  :value group-means
			  :description
			  (format nil "group means from anova of ~a on ~a"
				  (description dv) (description iv)))
	   :scheffe-table (make-scheffe-table-from-list
			   scheffe-table :group-means group-means)
	   :group-means-plot group-means-plot
	   :allow-other-keys t
	   initargs)))

(defun make-anova-one-way-result-from-list (list &rest initargs)
  (destructuring-bind 
    (anova-table group-means scheffe-table alt-sst)
    list
    (apply #'make-anova-one-way-result
	   :anova-table anova-table
	   :group-means group-means
	   :scheffe-table scheffe-table
	   :alt-sst alt-sst
	   initargs)))

(defmethod display ((anova-one-way-result anova-one-way-result) stream)
  (fresh-line stream)
  (flet ((format-item (name)
           (let ((value (slot-value anova-one-way-result name)))
             (formatting-cell (stream :align-x :right)
			      (present value 
				       'nice-real
				       :stream stream))))
	 (format-sequence (name)
	   (let ((value (slot-value anova-one-way-result name)))
	     (formatting-cell
	      (stream :align-x :right)
	      (display value stream))))
	 (format-label (string)
	   (with-text-style
	       (*label-text-style* stream)
	     (formatting-cell (stream :align-x :right)
			      (write-string string stream)))))
    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream)
      (format-label "Anova Table")
      (formatting-cell (stream)
		       (present-with-update (slot-value anova-one-way-result
							'anova-table)
					    #+ignore 'anova-one-way-table
		       :stream stream
		       :view +dialog-view+)))
     (formatting-row
      (stream)
      (format-label "Group means") (format-sequence 'group-means))
     (formatting-row 
      (stream)
      (format-label "Group means plot")
      (formatting-cell
       (stream)
       (present (slot-value anova-one-way-result 'group-means-plot)
		'graph-data-icon :stream stream :view +dialog-view+)))
     (formatting-row
      (stream)
      (format-label "Sheffe Table")
      (formatting-cell (stream)
		       (present (slot-value anova-one-way-result
					    'scheffe-table)
		       'scheffe-table
		       :stream stream
		       :view +dialog-view+)))
     (formatting-row
      (stream)
      (format-label "Alt. SS-Tot.")
      (format-item 'alt-sst)))))

;;; ---------------------------------------------------------------------------
;;; Anova Two Way Result

(defclass anova-two-way-table (modifiable-display-option-mixin result)
  ((display-option :allocation :class :initform :display-in-interactor)
   (iv1 :initarg :iv1 :accessor iv1)
   (iv2 :initarg :iv2 :accessor iv2)
   (dv  :initarg :dv :accessor dv)
   
   (df-interaction :initarg :df-interaction)
   (ss-interaction :initarg :ss-interaction)
   (ms-interaction :initarg :ms-interaction)
   (f-interaction  :initarg :f-interaction)
   (p-interaction  :initarg :p-interaction)

   (df-iv1         :initarg :df-iv1)
   (ss-iv1         :initarg :ss-iv1)
   (ms-iv1         :initarg :ms-iv1)
   (f-iv1          :initarg :f-iv1)
   (p-iv1          :initarg :p-iv1)

   (df-iv2         :initarg :df-iv2)
   (ss-iv2         :initarg :ss-iv2)
   (ms-iv2         :initarg :ms-iv2)
   (f-iv2          :initarg :f-iv2)
   (p-iv2          :initarg :p-iv2)

   (df-error       :initarg :df-error)
   (ss-error       :initarg :ss-error)
   (ms-error       :initarg :ms-error)

   (df-total       :initarg :df-total) 
   (ss-total       :initarg :ss-total)
   
   (cell-means     :initarg :cell-means)
   (row-totals     :initarg :row-totals)
   (column-totals  :initarg :column-totals)
   (grand-total    :initarg :grand-total)
   (a-labels       :initarg :a-labels)
   (b-labels       :initarg :b-labels))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open Anova"
    :target-form #'execute-on-target))

(defun make-anova-two-way-table (&rest options)
  (apply #'make-instance 'anova-two-way-table options))

(defun make-anova-two-way-table-from-list (list &rest initargs &key iv1 iv2 dv
					   &allow-other-keys)
  (destructuring-bind 
      ((df-interaction SS-interaction MS-interaction F-interaction
		       p-interaction)
       (df-iv1         SS-iv1         MS-iv1         F-iv1         p-iv1)
       (df-iv2         SS-iv2         MS-iv2         F-iv2         p-iv2)
       (df-error       SS-error       MS-error)
       (df-total       SS-total))
      list
    (apply #'make-anova-two-way-table
     :df-interaction df-interaction :SS-interaction SS-interaction 
     :MS-interaction MS-interaction :F-interaction F-interaction
     :p-interaction p-interaction

     :df-iv1 df-iv1 :SS-iv1 SS-iv1 :MS-iv1 MS-iv1 :F-iv1 F-iv1 :p-iv1 p-iv1

     :df-iv2 df-iv2 :SS-iv2 SS-iv2 :MS-iv2 MS-iv2 :F-iv2 F-iv2 :p-iv2 p-iv2

     :df-error df-error :SS-error SS-error :MS-error MS-error

     :df-total df-total :SS-total SS-total
     :iv1 iv1 :iv2 iv2 :dv dv
     initargs)))

(defmethod display ((self anova-two-way-table) stream)
  (fresh-line stream)
  (flet ((format-item (name)
           (let ((value (slot-value self name)))
             (formatting-cell (stream :align-x :right)
			      (present value 
				       'nice-real
				       :stream stream))))
	 (format-value (value)
	   (formatting-cell
	    (stream :align-x :right)
	    (present value 'nice-real :stream stream)))
	 (format-label (label)
	   (formatting-cell
	    (stream :align-x :right)
	    (with-text-style
		(*label-text-style* stream)
	      (cond
	       ((stringp label)
		(write-string label stream))
	       ((numberp label)
		(present label 'nice-real :stream stream))
	       (t
		(format stream "~a" label)))))))
    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream)
      (format-label "Source") (format-label "Degrees of Freedom")
      (format-label "Sum of Squares") (format-label "Mean Square")
      (format-label "F Ratio") (format-label "P"))
     (formatting-row
      (stream)
      (format-label "Interaction") (format-item 'df-interaction)
      (format-item 'ss-interaction) (format-item 'ms-interaction)
      (format-item 'f-interaction) (format-item 'p-interaction))
     (formatting-row
      (stream)
      (format-label (iv1 self)) (format-item 'df-iv1)
      (format-item 'ss-iv1) (format-item 'ms-iv1)
      (format-item 'f-iv1) (format-item 'p-iv1))
     (formatting-row
      (stream)
      (format-label (iv2 self)) (format-item 'df-iv2)
      (format-item 'ss-iv2) (format-item 'ms-iv2)
      (format-item 'f-iv2) (format-item 'p-iv2))
     (formatting-row
      (stream)
      (format-label "Error") (format-item 'df-error) (format-item 'ss-error)
      (format-item 'ms-error))
     (formatting-row
      (stream)
      (format-label "Total") (format-item 'df-total)
      (format-item 'ss-total)))
    (fresh-line stream)
    (format stream "~%")
    (with-slots (cell-means row-totals column-totals grand-total
		 a-labels b-labels) self
      (formatting-table
       (stream :inter-column-spacing '(2 :character))
       (formatting-row
	(stream)
	(format-label "Cell means")
	(dotimes (a (length a-labels))
	  (format-label (nth a a-labels))))
       (dotimes (row (array-dimension cell-means 0))
	 (formatting-row
	  (stream)
	  (format-label (nth row b-labels))
	  (dotimes (column (array-dimension cell-means 1))
	    (format-value (aref cell-means row column)))
	  (format-label (nth row row-totals))))
       (formatting-row
	(stream)
	(formatting-cell (stream) (declare (ignore stream)))
	(dotimes (column (array-dimension cell-means 1))
	  (format-label (nth column column-totals)))
	(format-label grand-total))))))

;;; ---------------------------------------------------------------------------
;;; Scheffe test Result

(defclass scheffe-table (result)
  ((scheffe-table :initarg :scheffe-table)
   (group-means   :initarg :group-means))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open Scheffe Table"
    :target-form #'execute-on-target))

#+ignore
(defun make-scheffe-table (&rest options)
  (apply #'make-instance 'scheffe-table options))

(defun make-scheffe-table-from-list
    (list &rest initargs)
  (apply #'make-instance 'scheffe-table
	 :scheffe-table
	 (mapcar #'(lambda (x)
		     (mapcar #'(lambda (y)
				 (make-scheffe-table-entry-from-list y))
			     x))
		 list)
	 initargs))

(defmethod display ((scheffe-table scheffe-table) stream)
  (fresh-line stream)
  (formatting-table
   (stream :inter-column-spacing '(2 :character))
   (let ((table (slot-value scheffe-table 'scheffe-table))
	 (group-means (slot-value scheffe-table 'group-means))
	 row)
     (formatting-row
      (stream)
      (formatting-cell
       (stream) (declare (ignore stream)))
      (if group-means
	  (dolist (group-mean (cdr group-means))
	    (formatting-cell
	     (stream :align-x :right)
	     (with-text-style
		 (*label-text-style* stream)
	       (present group-mean 'nice-real
			:stream stream :view +dialog-view+))))
	(dotimes (i (- (length table) 1))
	  (formatting-cell
	   (stream :align-x :right)
	   (with-text-style
	       (*label-text-style* stream)
	     (format stream "Group ~a" (1+ i)))))))
     (dotimes (i (length table))
       (setf row (nth i table))
       (formatting-row
	(stream)
	(formatting-cell
	 (stream :align-x :right)
	 (with-text-style
	     (*label-text-style* stream)
	   (if group-means
	       (present (nth i group-means) 'nice-real
			:stream stream :view +dialog-view+)
	     (format stream "Group ~a" i))))
	(dotimes (counter i)
	  (formatting-cell (stream) (declare (ignore stream))))
	(dotimes (j (length row))
	  (formatting-cell
	   (stream :align-x :right)
	   (present (nth j row)
		    'scheffe-table-entry
		    :stream stream
		    :view +dialog-view+))))))))

