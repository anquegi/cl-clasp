;;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Base:10 -*-
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

(in-package :CLASP-INTERFACE)

;;; ---------------------------------------------------------------------------
;;; Linear Regression brief Result

(defclass linear-regression-brief-result (modifiable-display-option-mixin
					  result)
  ((display-option :allocation :class :initform :display-in-interactor)
   (slope         :initarg :slope)
   (intercept     :initarg :intercept)
   (determination :initarg :determination)
   (std-err-slope :initarg :std-err-slope)
   (p-value       :initarg :p-value))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open Linear Regression Brief"
    :target-form #'execute-on-target))

(defun make-linear-regression-brief-result (&rest options)
  (apply #'make-instance 'linear-regression-brief-result options))

(defun make-linear-regression-brief-result-from-list (list)
  (destructuring-bind 
    (slope intercept determination std-err-slope p-value)
    list
    (make-linear-regression-brief-result :slope slope :intercept intercept
					 :determination determination
					 :std-err-slope std-err-slope
					 :p-value p-value)))

(defmethod display ((linear-regression-brief-result
		     linear-regression-brief-result) stream)
  (fresh-line stream)
  (flet ((format-item (name)
           (let ((value (slot-value linear-regression-brief-result name)))
             (formatting-cell (stream :align-x :right)
			      (present value 
				       'nice-real
				       :stream stream))))
	 (format-label (string)
	   (formatting-cell (stream :align-x :right)
			    (with-text-style
				(*label-text-style* stream)
			      (write-string string stream)))))
    (formatting-table (stream :inter-column-spacing '(2 :character))
       (formatting-row (stream)
		       (format-label "Slope") (format-item 'slope))
       (formatting-row (stream)
		       (format-label "Intercept") (format-item 'intercept))
       (formatting-row (stream)
		       (format-label "Coefficient of Determination (r^2)")
		       (format-item 'determination))
       (formatting-row (stream)
		       (format-label "Standard Error of the Slope")
		       (format-item 'std-err-slope))
       (formatting-row (stream)
		       (format-label "P-Value")
		       (format-item 'p-value)))))

;;; ---------------------------------------------------------------------------
;;; Linear Regression verbose Result

(defclass linear-regression-verbose-result (modifiable-display-option-mixin
					    result)
  ((display-option :allocation :class :initform :display-in-interactor)
   (slope             :initarg :slope)
   (intercept         :initarg :intercept)
   (determination     :initarg :determination)
   (correlation       :initarg :correlation)
   (std-err-slope     :initarg :std-err-slope)
   (std-err-intercept :initarg :std-err-intercept)
   (anova-table       :initarg :anova-table)
   (regression-plot   :initarg :regression-plot))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open Linear Regression Verbose"
    :target-form #'execute-on-target))

(defun make-linear-regression-verbose-result
    (&rest initargs &key iv dv anova-table slope intercept
     &allow-other-keys)
  (apply #'make-instance 'linear-regression-verbose-result
	 :anova-table (make-anova-one-way-table-from-list
		       anova-table
		       :iv iv :dv dv
		       :description
		       (format nil "anova of ~a on ~a"
			       (description iv) (description dv)))
	 :regression-plot (make-regression-plot-icon iv dv slope intercept)
	 :allow-other-keys t
	 initargs))

(defun make-linear-regression-verbose-result-from-list (list &rest initargs)
  (destructuring-bind 
      (slope intercept determination correlation
	     std-err-slope std-err-intercept anova-table)
      list
    (apply #'make-linear-regression-verbose-result
	   :slope slope :intercept intercept :determination determination
	   :correlation correlation :std-err-slope std-err-slope
	   :std-err-intercept std-err-intercept
	   :anova-table anova-table
	   initargs)))

(defmethod display ((linear-regression-verbose-result
		     linear-regression-verbose-result) stream)
  (fresh-line stream)
  (flet ((format-item (name)
           (let ((value (slot-value linear-regression-verbose-result name)))
             (formatting-cell (stream :align-x :right)
			      (present value 
				       'nice-real
				       :stream stream))))
	 (format-label (string)
	   (formatting-cell (stream :align-x :right)
			    (with-text-style
				(*label-text-style* stream)
			      (write-string string stream)))))
    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream)
      (format-label "Coefficient of Determination (r^2)")
      (format-item 'determination)
      (format-label "Correlation") (format-item 'correlation)))
    (fresh-line stream)
    (format stream "~%")
    (formatting-table
     (stream)
     (formatting-row
      (stream)
      (formatting-cell (stream) (declare (ignore stream)))
      (format-label "Coefficient")
      (format-label "Std. Err."))
     (formatting-row
      (stream)
      (format-label "Intercept") (format-item 'intercept)
      (format-item 'std-err-intercept))
     (formatting-row
      (stream)
      (format-label "Slope") (format-item 'slope)
      (format-item 'std-err-slope)))
    (fresh-line stream)
    (format stream "~%")
    (formatting-table
     (stream)
     (formatting-row
      (stream)
      (format-label "Anova Table")
      (formatting-cell
       (stream)
       (present (slot-value linear-regression-verbose-result 'anova-table)
		'anova-one-way-table
		:stream stream
		:view +dialog-view+)))
     (formatting-row
      (stream)
      (format-label "Regression plot")
      (formatting-cell
       (stream)
       (present (slot-value linear-regression-verbose-result 'regression-plot)
		'graph-data-icon
		:stream stream
		:view +dialog-view+))))))

;;; ---------------------------------------------------------------------------
;;; Multiple Linear Regression verbose Result

(defclass multiple-linear-regression-verbose-result
    (modifiable-display-option-mixin result)
    ((display-option :allocation :class :initform :display-in-interactor)
     (dv-name            :initarg :dv-name        :accessor dv-name)
     (iv-names           :initarg :iv-names       :accessor iv-names)
     (dv-length          :initarg :dv-length      :accessor dv-length)
     (num-ivs            :initarg :num-ivs        :accessor num-ivs)
     (df-regression      :initarg :df-regression  :accessor df-regression)
     (df-residual        :initarg :df-residual    :accessor df-residual)
     (df-total           :initarg :df-total       :accessor df-total)
     (ss-regression      :initarg :ss-regression  :accessor ss-regression)
     (ss-residual        :initarg :ss-residual    :accessor ss-residual)
     (ss-total           :initarg :ss-total       :accessor ss-total)
     (mse-regression     :initarg :mse-regression :accessor mse-regression)
     (mse-residual       :initarg :mse-residual   :accessor mse-residual)
     (f-statistic        :initarg :f-statistic    :accessor f-statistic)
     (p-value            :initarg :p-value        :accessor p-value)
     (determination      :initarg :determination  :accessor determination)
     (intercept          :initarg :intercept      :accessor intercept)
     (coefficients       :initarg :coefficients   :accessor coefficients)
     (betas              :initarg :betas          :accessor betas)
     (t-betas            :initarg :t-betas        :accessor t-betas)
     (p-betas            :initarg :p-betas        :accessor p-betas)
     (correlation-matrix :initarg :correlation-matrix
			 :accessor correlation-matrix))
  (:default-initargs
      :bitmap null-pattern
      :command-form #'print-args
      :command-documentation "Open Multiple Linear Regression Verbose"
      :target-form #'execute-on-target))

(defun make-multiple-linear-regression-verbose-result
    (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'multiple-linear-regression-verbose-result
	 :allow-other-keys t
	 initargs))

(defun make-multiple-linear-regression-verbose-result-from-list
    (list &rest initargs &key ivs dv &allow-other-keys)
  (destructuring-bind 
      (intercept coefficients r-list t-betas betas 
		 r-square f-statistic ss-reg-list  ss-percent-list
		 ss-regression ss-residual mse-regression mse-residual)
      list
    (declare (ignore ss-reg-list ss-percent-list))
    (let* ((dv-name (string (name dv)))
	   (iv-names (mapcar #'(lambda (x) (string (name x))) ivs))
	   (num-ivs (length ivs))
	   (dv-length (length (variable-value dv)))
	   (df-regression num-ivs)
	   (df-residual (- dv-length num-ivs 1))
	   (df-total (+ df-regression df-residual))
	   (ss-total (+ ss-regression ss-residual))
	   (p-value (f-significance f-statistic df-regression df-residual))
	   (p-betas (mapcar #'(lambda (t-beta)
				(students-t-significance
				 t-beta (+ df-regression df-residual -1)
				 :both))
			    t-betas))
	   (correlation-matrix
	    (make-array (list (+ num-ivs 1) num-ivs)
			:displaced-to (apply #'vector r-list)
			:displaced-index-offset 0)))
      (apply #'make-multiple-linear-regression-verbose-result
	     :dv-name dv-name
	     :iv-names iv-names
	     :dv-length dv-length
	     :num-ivs num-ivs
	     :df-regression df-regression
	     :df-residual df-residual
	     :df-total df-total
	     :ss-regression ss-regression
	     :ss-residual ss-residual
	     :ss-total ss-total
	     :mse-regression mse-regression
	     :mse-residual mse-residual
	     :f-statistic f-statistic
	     :p-value p-value
	     :determination r-square
	     :intercept intercept
	     :coefficients coefficients
	     :betas betas
	     :t-betas t-betas
	     :p-betas p-betas
	     :correlation-matrix correlation-matrix
	     initargs))))

(defmethod display ((self
		     multiple-linear-regression-verbose-result) stream)
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
	 (format-label (string)
	   (formatting-cell
	    (stream :align-x :right)
	    (with-text-style
		(*label-text-style* stream)
	      (write-string string stream)))))
    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream) (format-label "Count") (format-label "R-Squared"))
     (formatting-row
      (stream) (format-item 'dv-length) (format-item 'determination)))
    
    (fresh-line stream)
    (format stream "~%")
    
    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream)
      (format-label "Source") (format-label "DF")
      (format-label "Sum of Squares") (format-label "Mean Square Error"))
     (formatting-row 
      (stream)
      (format-label "REGRESSION") (format-item 'df-regression)
      (format-item 'ss-regression) (format-item 'mse-regression))
     (formatting-row 
      (stream)
      (format-label "RESIDUAL") (format-item 'df-residual)
      (format-item 'ss-residual) (format-item 'mse-residual))
     (formatting-row 
      (stream)
      (format-label "TOTAL") (format-item 'df-total)
      (format-item 'ss-total)))
    
    (fresh-line stream)
    (format stream "~%")

    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream)
      (format-label (format nil "F(~d,~d) statistic:"
			    (df-regression self) (df-residual self)))
      (format-item 'f-statistic))
     (formatting-row
      (stream)
      (format-label "p:")
      (format-item 'p-value)))
    
    (fresh-line stream)
    (format stream "~%")
    
    (formatting-table
     (stream :inter-column-spacing '(2 :character))
     (formatting-row
      (stream)
      (format-label "Variable") (format-label "Coefficent")
      (format-label "Beta") (format-label "t-Value") (format-label "p"))
     (formatting-row
      (stream)
      (format-label "INTERCEPT") (format-item 'intercept))
     (loop for name in (iv-names self)
	 for coefficient in (coefficients self)
	 for beta in (betas self)
	 for t-value in (t-betas self)
	 for p in (p-betas self)
	 do
	   (formatting-row
	    (stream)
	    (format-label name)
	    (format-value coefficient)
	    (format-value beta)
	    (format-value t-value)
	    (format-value p))))
  
    (fresh-line stream)
    (format stream "~%")
    
    (with-text-style ((make-text-style :sans-serif :bold :small) stream)
      (format stream "Correlations~%"))
    (let ((matrix (correlation-matrix self)))
      (formatting-table
       (stream :inter-column-spacing '(2 :character))
       (formatting-row
	(stream)
	(formatting-cell (stream) (declare (ignore stream)))
	(dolist (name (iv-names self))
	 (format-label name)))
       (formatting-row
	(stream)
	(format-label (dv-name self))
	(dotimes (x (num-ivs self))
	   (format-value (aref matrix 0 x))))
       (loop for row from 1 to (num-ivs self)
	   do
	     (formatting-row
	      (stream)
	      (format-label (nth (- row 1) (iv-names self)))
	      (loop for column from 0 to (- (num-ivs self) 1)
		  do
		    (format-value (aref matrix row column)))))))))

