;;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/v3dev/interface/data-manipulation-commands.lisp *-*
;;;; *-* Last-edit: Thursday, February 18, 1993  00:54:05; Edited-By: carlson *-* 
;;;; *-* Machine: Miles (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                       Data Manipulation Commands                       *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Adam Carlson
;;;             Experimental Knowledge Systems Laboratory
;;;             Paul R. Cohen, Principal Investigator
;;;             David L. Westbrook, Systems Manager
;;;             David M. Hart, Laboratory Manager
;;;             Department of Computer Science
;;;             University of Massachusetts
;;;             Amherst, Massachusetts 01003.
;;;
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
;;;
;;;  02-18-93 File Created.  (carlson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :CLASP-INTERFACE)

;;; --*--
;;; ***************************************************************************

;;; ----------------------------------------------------------------------------

(define-command-table clasp-transform)

(add-menu-item-to-command-table 'clasp "Transform" 
                                :menu 'clasp-transform 
				:documentation
				"Create new variables from old ones"
                                :errorp nil
				:after "Manipulate")

(define-clasp-command
    (com-formula
     :command-table clasp-transform 
     :name "User Defined"
     :menu
     ("User Defined"
      :documentation
      "Apply an arbitrary lisp expression to the variables of a dataset

 You will be prompted for a dataset and an expression.
 The dataset is the dataset in which the transformation
 should take place, the expression is a lisp expression
 which calculates the values for the new variable.  For
 example, to compute the difference between TREATMENT-1
 and TREATMENT-2 in a dataset, the expression would be
 (- TREATEMENT-1 TREATMENT-2)"))
    ((dataset-1 'dataset :prompt "Dataset")
     (expression-2 'expression :prompt "Expression"))
  (let ((values
	 (multiple-value-list (funcall
			       #'clasp::formula
			       dataset-1
			       expression-2))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'formula (length values) 1)
    (present-with-update (nth 0 values)
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-sort :command-table clasp-transform
				:name t
				:menu ("Sort"
				       :documentation
				       "Sort a variable"))
    ((variable-1 'variable :prompt "Variable to sort" :mapping-option :map))
  (present-with-update (clasp::sort-variable variable-1 variable-1)
		       :stream *standard-output* :view +dialog-view+))
				
(define-clasp-command (com-index :command-table clasp-transform
				:name t
				:menu ("Index"
				       :documentation
				       "Sort one variable on the values of another"))
    ((variable-1 'variable :prompt "Variable to sort")
     (variable-2 'variable :prompt "Variable to sort by"))
  (present-with-update (clasp::sort-variable variable-1 variable-2)
		       :stream *standard-output* :view +dialog-view+))
				
(define-clasp-command (com-add-a-constant :command-table clasp-transform
				    :name t
				    :menu
				    ("Add A Constant"
				     :documentation
				     "Add a constant value to a variable"))
    ((variable-1 'variable :prompt "Variable" :mapping-option :map)
     (number-2 'number :prompt "Constant" :mapping-option :map))
  (let ((values
	 (multiple-value-list (funcall
			       #'clasp::add-a-constant
			       variable-1
			       number-2))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'add-a-constant (length values) 1)
    (present-with-update (nth 0 values)
	     :stream *standard-output* :view +dialog-view+)))

#+ignore
(defclaspcom add-a-constant transform
  (variable number) (t t)
  (t) (column)
  :input-options ((:prompt "Variable") (:prompt "Constant")))

(define-clasp-command (com-natural-log :command-table clasp-transform :name t
				 :menu ("Natural Log"
					:documentation
					"Take the ln of a variable"))
    ((variable-1 'variable :prompt "Variable" :mapping-option :map))
  (let ((values
	 (multiple-value-list (funcall
			       #'clasp::natural-log
			       variable-1))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'natural-log (length values) 1)
    (present-with-update (nth 0 values)
	     :stream *standard-output* :view +dialog-view+)))

#+ignore
(defclaspcom natural-log transform
  (variable) (t)
  (t) (column)
  :input-options ((:prompt "Variable")))

(define-clasp-command (com-log10 :command-table clasp-transform :name t
			   :menu ("Log10"
				  :documentation
				  "Take the log base 10 of a variable"))
    ((variable-1 'variable :prompt "Variable" :mapping-option :map))
  (let ((values
	 (multiple-value-list (funcall
			       #'clasp::log10
			       variable-1))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'log10 (length values) 1)
    (present-with-update (nth 0 values)
	     :stream *standard-output* :view +dialog-view+)))

#+ignore
(defclaspcom log10 transform
  (variable) (t)
  (t) (column)
  :input-options ((:prompt "Variable")))

(define-clasp-command (com-recode-categorical
		 :command-table clasp-transform :name t
		 :menu ("Recode Categorical"
			:documentation
			"Reassign all the values of a categorical variable"))
    ((variable-1 'variable :prompt "Variable")
     (expression-2 'expression :prompt "Old Values")
     (expression-3 'expression :prompt "New Values"))
  (let ((values
	 (multiple-value-list (funcall
			       #'clasp::recode-categorical
			       variable-1
			       expression-2
			       expression-3))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'recode-categorical (length values) 1)
    (present-with-update (nth 0 values)
     :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-reassign-bins
		 :command-table clasp-transform :name t
		 :menu ("Reassign Bins"
			:documentation
			"Reassign all the values of a floating-point variable"))
    ((variable-1 'variable :prompt "Variable")
     (expression-2 'expression :prompt "Old Values")
     (expression-3 'expression :prompt "New Values"))
  (let ((values
	 (multiple-value-list (funcall
			       #'clasp::rebin
			       variable-1
			       expression-2
			       expression-3))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'rebin (length values) 1)
    (present-with-update (nth 0 values)
     :stream *standard-output* :view +dialog-view+)))

#+ignore
(defclaspcom recode-categorical transform
  (variable expression expression) (t t t)
  (t) (column)
  :input-options ((:prompt "Variable") (:prompt "Old Values")
				       (:prompt "New Values")))

#+ignore
(defclaspcom formula transform
  (dataset expression) (t t)
  (t) (column)
  :input-options ((:prompt "Dataset") (:prompt "Expression")))

(define-clasp-command (com-smooth-variable-4253h
		 :command-table clasp-transform
		 :name t
		 :menu ("Smooth 4253h"
			:documentation
			"Apply a 4253h smoothing function to a variable"))
    ((column-1 'variable :prompt "x" :mapping-option :map))
  (let ((values
	 (multiple-value-list (funcall
			       #'clasp::smooth-variable-4253h
			       column-1))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'smooth-variable-4253h (length values) 1)
    (present-with-update (nth 0 values)
     :stream *standard-output* :view +dialog-view+)))

#+ignore
(defclaspcom (smooth-variable-4253h :Menu "Smooth 4253h") transform
  (column) (t) (t) (column)
  :input-options ((:prompt "Variable")))

(define-clasp-command
    (com-discrete-derivative
     :command-table clasp-transform :name t
     :menu
     ("Discrete Derivative"
      :documentation
      "Take the difference of each pair of successive values in a variable"))
    ((column-1 'variable :prompt "x" :mapping-option :map))
  (let ((values
	 (multiple-value-list (funcall
			       #'clasp::difference-a-column
			       column-1))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'difference-a-column (length values) 1)
    (present-with-update (nth 0 values)
	     :stream *standard-output* :view +dialog-view+)))

#+ignore
(defclaspcom (difference-a-column :menu "Discrete Derivative")
    transform (column) (t) (t) (column)
    :input-options ((:prompt "Variable")))

;;; ***************************************************************************
;;; EOF
