;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Syntax:COMMON-LISP; Base:10 -*-
;;;; *-* Last-edit: Friday, February 19, 1993  13:14:00; Edited-By: carlson *-* 
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; Copyright (c) 1990 - 1993 University of Massachusetts
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

(define-command-table clasp-graphing)

(add-menu-item-to-command-table 'clasp "Graph" 
                                :menu 'clasp-graphing 
				:documentation "Graph creation"
                                :errorp nil :after "file")

(defun make-graph-data (&rest options)
  (apply #'make-instance 'graph::graph-data options))

(define-clasp-command (com-histogram
		       :command-table clasp-graphing :name t
		       :menu ("Histogram"
			      :documentation
			      "Create a histogram of a variable"))
    ((variable 'numbers :prompt "x" :mapping-option :map)
     (color-by '(null-or-type column) :prompt "Color by" :default nil))
  (let* ((graph (make-point-colored-histogram-graph variable color-by))
	 (icon (make-graph-data-icon
		:description (format nil "Histogram of ~a"
				     (description variable))
		:x-label ""
		:y-label (clasp::name-string variable)
		:graph graph)))
    (present-with-update icon :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-scatter-plot
		       :command-table clasp-graphing :name t
		       :menu ("Scatter Plot"
			      :documentation
			      "Create a scatter plot of two variables"))
  ((y-variable 'numbers :prompt "y" :mapping-option :map)
   (x-variable 'numbers :prompt "x" :mapping-option :map)
   (color-by '(null-or-type column) :prompt "Color by" :default nil))
  (let (icon graph)
    (setf graph (make-point-colored-graph y-variable x-variable color-by
					  :graph-name "Scatter plot"
					  :graph-data-options
					  '(:symbologies (:scatter)))
	  icon (make-graph-data-icon
		:description (format nil "scatter plot of ~a/~a"
				     (description x-variable)
				     (description y-variable))
		:x-label (clasp::name-string x-variable)
		:y-label (clasp::name-string y-variable)
		:graph graph))
    (present-with-update icon :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-line-plot
		       :command-table clasp-graphing :name "Line Plot"
		       :menu ("Line Plot"
			      :documentation
			      "Create a line plot of two variables"))
  ((y-variable 'numbers :prompt "y" :mapping-option :map)
   (x-variable 'numbers :prompt "x" :mapping-option :map)
   (color-by '(null-or-type column) :prompt "Color by" :default nil))
  (let (icon graph)
    (setf graph (make-point-colored-graph y-variable x-variable color-by
					  :graph-name "Line plot"
					  :graph-data-options
					  '(:symbologies (:line)))
	  icon (make-graph-data-icon
		:description (format nil "line plot of ~a/~a"
				     (description x-variable)
				     (description y-variable))
		:x-label (clasp::name-string x-variable)
		:y-label (clasp::name-string y-variable)
		:graph graph))
    (present-with-update icon :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-plot-sequence
		       :command-table clasp-graphing
		       :name "Row Line Plot"
		       :menu
		       ("Row Line Plot"
			:documentation
			"Create a line plot of a variable against row number"))
    ((data 'numbers :prompt "y" :mapping-option :map)
     (color-by '(null-or-type column) :prompt "Color by" :default nil))
  (let* ((x-variable (if (typep data 'column)
			 (get-variable 'row-number
				       (clasp::variable-dataset data))
		       (loop for row from 1 to (length data) collect row)))
	 (graph (make-point-colored-graph
		 data x-variable color-by
		 :graph-data-options '(:symbologies (:line))
		 :graph-options
		 `(:x-label "Row Number"
			    :title ,(format nil "Line plot of ~a"
					    (name data)))))
	 (icon (make-graph-data-icon
		:description (format nil "line plot of ~a" (description data))
		:x-label "Row Number"
		:y-label (clasp::name-string data)
		:graph graph)))
    (present-with-update icon :stream *standard-output* :view +dialog-view+)))

#+ignore
(define-clasp-command (com-line-plot-variable :command-table clasp-graphing
					      :name t
					      :menu "Line Plot - Variable")
    ((data 'column :prompt "Variable" :mapping-option :map))
    (let ((icon (make-graph-data-icon
		 #+ignore :name #+ignore (name data)
		 :description (format nil "line plot of ~a"
				      (description data))
		 :graph-data
		 (make-graph-data
		  :symbologies '(:line)
		  :data (loop for item in (simplify data)
			    for cnt from 1
			    collect `(,cnt ,item))
		  :name (name data)))))
    (place icon)
    (unless mapping-p (auto-open icon))))

    
(define-clasp-command
    (com-plot-regression
     :command-table clasp-graphing
     :name "Regression Plot"
     :menu
     ("Regression Plot"
      :documentation
      "Create a scatter plot of y on x and overlay a regression line"))
  ((dv 'numbers :prompt "y" :mapping-option :map)
   (iv 'numbers :prompt "x" :mapping-option :map))
  (let ((regression-plot-icon (make-regression-plot-icon iv dv)))
    (present-with-update regression-plot-icon
			 :stream *standard-output* :view +dialog-view+)))


(defun make-regression-plot-icon (iv dv &optional slope intercept)
  (let ((iv-value (simplify iv))
	(dv-value (simplify dv))
	(M slope)
	(b intercept))
    (unless (and slope intercept)
	    (multiple-value-setq (M b)
			      (linear-regression-minimal dv-value iv-value)))
    (let* ((graph-data-points (make-graph-data
			       :symbologies '(:scatter)
			       :data (mapcar #'list iv-value dv-value)
			       :name (name dv)))
	   (graph-data-line (make-instance 'graph::equation-data
					   ;; was graph-user:: -AC
					   :equation '(+ (* x slope) intercept)
					   :variable 'x
					   :min (minimum iv-value)
					   :max (maximum iv-value)
					   :increment (range iv-value)
					   :parameters `((slope ,M)
							 (intercept ,b))))
	   (graph (make-graph
		   :x-label (string (name iv))
		   :y-label (string (name dv))
		   :title
		   (format nil "Scatter Plot of ~a/~a w/ Regression Line" 
			   (name iv)
			   (name dv))))
	   (icon (make-graph-data-icon
		  #+ignore :name
		  #+ignore (format nil
				   "Scatter Plot of ~a/~a w/ Regression Line" 
				   (name iv)
				   (name dv))
		  :description (format nil "regression plot of ~a/~a" 
				       (description iv) (description dv))
		  :x-label (clasp::name-string iv)
		  :y-label (clasp::name-string dv)
		  :graph graph
		  :graph-data graph-data-points)))
      (add-dataset graph graph-data-points)
      (add-dataset graph graph-data-line)
      icon)))

(define-command (com-overlay-graphs :command-table clasp-graphing
				    :name t
				    :menu "Overlay Graphs")
    ((graph-icons '(sequence graph-data-icon) :prompt "Graphs"))
  (let ((new-icon (make-overlayed-graph-icon graph-icons)))
    (present-with-update new-icon :stream *standard-output*
			 :view +dialog-view+)))

(defun make-overlayed-graph-icon (graph-icons)
  (let* ((description
	  (format nil "~{~a~^ and ~}"
		  (mapcar #'description graph-icons)))
	 (new-graph (make-graph :x-label (x-label (car graph-icons))
				:y-label (y-label (car graph-icons))
				:title description))
	 (new-icon (make-graph-data-icon :description description)))
    ;; graph-data-icon objects have a graph-data slot (if there is only
    ;; one scigraph::dataset attached to the icon) and a graph slot (which
    ;; is used for things like scatterplot w/ regression line.)  This checks
    ;; if the graph slot is being used and uses its datasets, otherwise, it
    ;; just uses the graph-data slot.
    (dolist (graph-icon graph-icons)
      (when (and (slot-boundp graph-icon 'graph)
		 (not (null (graph graph-icon))))
	(dolist (dataset (graph::datasets (graph graph-icon)))
	  (add-dataset new-graph dataset)))
      (when (and (slot-boundp graph-icon 'graph-data)
		 (not (null (graph-data graph-icon))))
	(add-dataset new-graph (graph-data graph-icon))))
    (setf (slot-value new-icon 'graph) new-graph)
    new-icon))

(defun make-point-colored-graph (y-variable x-variable color-by
				 &key graph-data-options graph-options
				      graph-name)
  (let (graph)
    (if color-by
	(let (datasets graph-data)
	  (unless (and (typep y-variable 'column)
		       (typep x-variable 'column)
		       (eq (clasp::variable-dataset y-variable)
			   (clasp::variable-dataset x-variable))
		       (eq (clasp::variable-dataset color-by)
			   (clasp::variable-dataset x-variable)))
	    (error 'invalid-coloring-variable))
	  (setf datasets (partition-dataset
			  (clasp::variable-dataset x-variable)
			  `(.on. ,(name color-by))
		      (list y-variable x-variable color-by)
		      :unique-variable-names-p nil)
		graph-data (mapcar
			    #'(lambda (dataset)
				(let ((x (get-variable (name x-variable)
						       dataset))
				      (y (get-variable (name y-variable)
						       dataset)))
				  (apply #'make-graph-data
					 :data (mapcar #'list
						       (simplify x)
						       (simplify y))
					 (append graph-data-options
						 `(:name
						   ,(clasp::name-string
						     dataset))))))
			    datasets)
		graph (apply #'make-graph
			     (append
			      graph-options
			      `(:x-label ,(string (name x-variable))
				:y-label ,(string (name y-variable))
				:title ,(format nil "~a of ~a/~a"
						(or graph-name "Plot")
						(name x-variable)
						(name y-variable))))))
	  (dolist (graph-dataset graph-data)
	    (add-dataset graph graph-dataset))
	  (dolist (dataset datasets)
	    (delete-dataset dataset)))
      (let ((graph-data (apply #'make-graph-data
			       :data (mapcar #'list (simplify x-variable)
					     (simplify y-variable))
			       (append graph-data-options
				       `(:name ,(name y-variable))))))
	(setf graph (apply #'make-graph
			   (append graph-options
				   `(:x-label ,(string (name x-variable))
				     :y-label ,(string (name y-variable))
				     :title ,(format nil "~a of ~a/~a"
						     (or graph-name "Plot")
						     (name x-variable)
						     (name y-variable))))))
	(add-dataset graph graph-data)))
    graph))

(defun make-point-colored-histogram-graph
    (variable color-by &key graph-data-options graph-options graph-name)
  (let (graph)
    (if color-by
	(let (datasets graph-data)
	  (unless (and (typep variable 'column)
		       (eq (clasp::variable-dataset variable)
			   (clasp::variable-dataset color-by)))
	    (error 'invalid-coloring-variable))
	  (setf datasets (partition-dataset
			  (clasp::variable-dataset variable)
			  `(.on. ,(name color-by))
			  (list variable color-by)
			  :unique-variable-names-p nil)
		graph-data
		(mapcar
		 #'(lambda (dataset)
		     (let ((var (get-variable (name variable) dataset)))
		       (apply #'make-instance 'graph::histogram-data
			      :sample-data (simplify var)
			      :name (clasp::name-string dataset)
			      graph-data-options)))
		 datasets)
		graph (apply #'make-graph
			     :x-label (string (name variable))
			     :title (format nil "~a of ~a"
					    (or graph-name "Histogram")
					    (name variable))
			     graph-options))
	  (dolist (graph-dataset graph-data)
	    (add-dataset graph graph-dataset))
	  (dolist (dataset datasets)
	    (delete-dataset dataset)))
      (let ((graph-data (apply #'make-instance 'graph::histogram-data
			       :sample-data (simplify variable)
			       :name (name variable)
			       graph-data-options)))
	(setf graph (apply #'make-graph
			   :x-label (string (name variable))
			   :title (format nil "~a of ~a"
					  (or graph-name "Histogram")
					  (name variable))
			   graph-options))
	(add-dataset graph graph-data)))
    graph))

#+ignore
(define-command (com-plot-confidence-interval :command-table clasp-graphing :name t :menu "Plot Confidence Interval")
    ((middle 'numbers :mapping-option map)
     (lower 'numbers :mapping-option map)
     (upper 'numbers :mapping-option map))
  (let ((graph-data-1 (make-graph-data
			:symbologies '(:line)
			:data (loop for item in (simplify middle)
				    for cnt from 1
				  collect `(,cnt ,item))
			:name (name middle)))
	(graph-data-2 (make-graph-data
			:symbologies '(:scatter)
			:data-symbol :diamond
                        :symbol-height 2
			:data (loop for item in (simplify lower)
				    for cnt from 1
				  collect `(,cnt ,item))
			:name (name lower)))
	(graph-data-3 (make-graph-data
			:symbologies '(:scatter)
			:data-symbol :diamond
                        :symbol-height 2
			:data (loop for item in (simplify upper)
				    for cnt from 1
				  collect `(,cnt ,item))
			:name (name upper)))
	(graph (make-graph :x-label "X"
			   :y-label (string (clasp::name middle))
			   :title "Confidence Interval")))

    (add-dataset graph graph-data-2)
    (add-dataset graph graph-data-3)
    (add-dataset graph graph-data-1)
    (auto-open graph :refresh t :label "Confidence Interval")))

#| Aborted attempt to be tricky


(defclass variable-data (graph::graph-data)
  ((x-variable :initarg :x-variable :reader x-variable)
   (y-variable :initarg :y-variable :reader y-variable)))

(defmethod graph::map-data ((self variable-data) (function t) (data t))
   "Apply the function to each datum"
   (mapc #'function 
         (clasp::variable-value (x-variable self)) 
         (clasp::variable-value (y-variable self)) 
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (funcall function value))
             data))

(defmethod graph::datum-position ((self variable-data) (datum array))
    "Get the actual X and Y values to plot."
    (values (aref datum 0) (aref datum 1)))


|#
