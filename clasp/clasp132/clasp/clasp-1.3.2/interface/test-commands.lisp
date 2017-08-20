;;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/clasp/development/interface/test-commands.lisp *-*
;;;; *-* Last-edit: Friday, July 9, 1993  12:15:36; Edited-By: carlson *-* 
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
;;; Written by: David L. Westbrrok and Adam Carlson
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
;;;  02-18-93 File Created.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :CLASP-INTERFACE)

;;; --*--
;;; ***************************************************************************

(define-command-table clasp-tests)

(add-menu-item-to-command-table 'clasp "Test" 
                                :menu 'clasp-tests 
				:documentation "Statistical hypothesis testing"
                                :errorp nil :after "Transform")

;;; ---------------------------------------------------------------------------
;;; Command definition

(define-clasp-command (com-confidence-interval-z
		       :command-table clasp-tests :name t
		       :menu "Confidence Interval Using z Statistic")
    ((column-1 'numbers :prompt "x" :mapping-option :map)
     (real-2 #-lucid'(real 0 1) #+lucid 'real :prompt "confidence level"))
  (let* ((values
	  (multiple-value-list (funcall
				#'confidence-interval-z
				(simplify column-1)
				real-2)))
	 (result (funcall 'make-confidence-interval-result-from-list values)))
    (setf (description result)
      (format nil "~A % Confidence interval of ~a"
	      (* real-2 100) (description column-1)))
    (assert (= (length values) 3) nil
      "`confidence-interval-z' returned ~s values, we were expecting 3"
      (length values))
    (present-with-update result :stream *standard-output*
			 :view +dialog-view+)))

(define-clasp-command (com-confidence-interval-t
		 :command-table clasp-tests :name t
		 :menu "Confidence Interval Using t Statistic")
    ((column-1 'numbers :prompt "x" :mapping-option :map)
     (real-2 #-lucid '(real 0 1)  #+lucid 'real :prompt "confidence level"))
  (let* ((values
	  (multiple-value-list (funcall
			       #'confidence-interval-t
			       (simplify column-1)
			       real-2)))
	 (result (funcall 'make-confidence-interval-result-from-list values)))
    (setf (description result)
      (format nil "~A % Confidence interval of ~a"
	      (* real-2 100) (description column-1)))
    (assert (= (length values) 3) nil
      "`confidence-interval-t' returned ~s values, we were expecting 3"
      (length values))
    (present-with-update result :stream *standard-output*
			 :view +dialog-view+)))

(define-clasp-command (com-confidence-interval-proportion
		 :command-table clasp-tests :name t
		 :menu "Confidence Interval of a Proportion")
    ((integer-1 #-lucid '(integer 0 *) #+lucid 'integer :prompt "successes")
     (integer-2 #-lucid '(integer 1 *) #+lucid 'integer :prompt "trials")
     (real-3 #-lucid '(real 0 1) #+lucid 'real :prompt "confidence level"))
  (let* ((values
	  (multiple-value-list (funcall
				#'confidence-interval-proportion
				integer-1
				integer-2
				real-3)))
	 (result (funcall 'make-confidence-interval-result-from-list values)))
    (setf (description result)
      (format nil "~A % Confidence interval of ~a success in ~a trials"
	      (* real-3 100) integer-1 integer-2))
    (assert (= (length values) 3) nil
      "`confidence-interval-proportion' returned ~s values, we were expecting 3"
      (length values))
    (present-with-update result :stream *standard-output*
			 :view +dialog-view+)))

(define-clasp-command (com-t-test-one-sample :command-table clasp-tests
				       :name t :menu "t-test One Sample")
    ((column-1 'numbers :prompt "x" :mapping-option :map)
     (mean 'number :prompt "mean" :mapping-option :map)
     (member-2 '(member :both :positive :negative)
		 :prompt "Tails: [Both, Positive or Negative]"))
  (let* ((values
	  (multiple-value-list (funcall
				#'t-test-one-sample
				(simplify column-1)
				member-2 mean)))
	 (result (funcall 'make-t-test-result-from-list values)))
    (setf (description result)
      (format nil "t-test of ~a with ~a tail(s)"
	      (description column-1) member-2))
    (assert (= (length values) 4) nil
      "`t-test-one-sample' returned ~s values, we were expecting ~s"
      (length values))
    (present-with-update result :stream *standard-output*
			 :view +dialog-view+)))

(define-clasp-command (com-t-test-two-samples :command-table clasp-tests :name t
			    :menu "t-test Two Samples")
    ((column-2 'numbers :prompt "y" :mapping-option :map)
     (column-1 'numbers :prompt "x" :mapping-option :map)
     (member-3 '(member :both :positive :negative)
		 :prompt "Tails: [Both, Positive or Negative]"))
  (let* ((values
	  (multiple-value-list (funcall
				#'t-test
				(simplify column-1)
				(simplify column-2)
				member-3)))
	 (result (funcall 'make-t-test-result-from-list values)))
    (setf (description result)
      (format nil "t-test of ~a and ~a with ~a tail(s)"
	      (description column-1) (description column-2) member-3))
    (assert (= (length values) 4) nil
      "`t-test' returned ~s values, we were expecting ~s"
      (length values) 4)
    (present-with-update result #+ignore 't-test-result
			 :stream *standard-output*
			 :view +dialog-view+)))

(define-clasp-command (com-t-test-matched :command-table clasp-tests :name t
				    :menu "t-test Matched Pairs")
    ((column-2 'numbers :prompt "y" :mapping-option :map)
     (column-1 'numbers :prompt "x" :mapping-option :map)
     (member-3 '(member :both :positive :negative)
		 :prompt "Tails: [Both, Positive or Negative]"))
  (let* ((values
	  (multiple-value-list (funcall
				#'t-test-matched
				(simplify column-1)
				(simplify column-2)
				member-3)))
	 (result (funcall 'make-t-test-result-from-list values)))
    (setf (description result)
      (format nil "matched pairs t-test of ~a and ~a with ~a tail(s)"
	      (description column-1) (description column-2) member-3))
    (assert (= (length values) 4) nil
      "`t-test-matched' returned ~s values, we were expecting ~s"
      (length values) 4)
    (fresh-line)
    (present-with-update result
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-d-test :command-table clasp-tests :name t
			    :menu "D test")
    ((column-2 'numbers :prompt "y" :mapping-option :map)
     (column-1 'numbers :prompt "x" :mapping-option :map)
     (member-3 '(member :both :positive :negative)
		 :prompt "Tails: [Both, Positive or Negative]"))
  (let* ((values
	  (multiple-value-list (funcall
				#'d-test
				(simplify column-1)
				(simplify column-2)
				member-3)))
	 (result (funcall 'make-d-test-result-from-list values)))
    (setf (description result)
      (format nil "D test of ~a and ~a with ~a tail(s)"
	      (description column-1) (description column-2) member-3))
    (assert (= (length values) 3) nil
      "`d-test' returned ~s values, we were expecting 3"
      (length values))
    (present-with-update result
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-anova-one-way-variables :command-table clasp-tests
					     :name t :menu "Anova - One Way")
    ((column-2 'numbers :prompt "y" :mapping-option :map)
     (column-1 'numbers :prompt "x" :mapping-option :map))
  (let* ((values
	  (multiple-value-list (funcall
				#'anova-one-way-variables
				(simplify-and-sort column-1)
				(simplify-and-sort-by column-2 column-1))))
	 (result (funcall 'make-anova-one-way-result-from-list values
			  :iv column-1 :dv column-2)))
    (setf (description result)
      (format nil "anova of ~a on ~a"
	      (description column-1) (description column-2)))
    (assert (= (length values) 4) nil
      "`anova-one-way-variables' returned ~s values, we were expecting 4"
      (length values))
    (present-with-update result
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-anova-two-way-variables :command-table clasp-tests
					     :name t :menu "Anova - Two Way")
    ((column-1 'numbers :prompt "y" :mapping-option :map)
     (column-2 'numbers :prompt "x 1" :mapping-option :map)
     (column-3 'numbers :prompt "x 2" :mapping-option :map))
  (let* ((values
	  (multiple-value-list (funcall
				#'anova-two-way-variables-unequal-cell-sizes
				(simplify column-1)
				(simplify column-2)
				(simplify column-3))))
	 (result (funcall 'make-anova-two-way-table-from-list (nth 0 values)
			  :iv1 (string (name column-2))
			  :iv2 (string (name column-3))
			  :dv (string (name column-1))
			  :cell-means (nth 1 values)
			  :row-totals (nth 2 values)
			  :column-totals (nth 3 values)
			  :grand-total (nth 4 values)
			  :a-labels (nth 5 values)
			  :b-labels (nth 6 values))))
    (setf (description result)
      (format nil "anova of ~a and ~a on ~a"
	      (description column-2) (description column-3)
	      (description column-1)))
    (assert (= (length values) 7) nil
      "`~a' returned ~s values, we were expecting ~s"
      'anova-two-way-variables (length values) 7)
    (present-with-update result
			 :stream *standard-output* :view +dialog-view+)))

#+ignore
(defclaspcom (anova-two-way-variables :menu "Anova - Two Way") tests
  (column column column) (simplify simplify simplify)
  (make-anova-two-way-table-from-list) (anova-two-way-table)
  :input-options ((:prompt "DV")  (:prompt "IV 1") (:prompt "IV 2"))) 

(define-clasp-command (com-chi-square-counts
                         :command-table clasp-tests :name t
			 :menu "Chi-Square Counts")
    ((a 'number :prompt "a")
     (b 'number :prompt "b")
     (c 'number :prompt "c")
     (d 'number :prompt "d"))
  (let* ((table (make-array '(2 2) :initial-contents
                                   (list (list a c) (list b d))))
         (values
	  (append
            (multiple-value-list (funcall #'chi-square-2x2-counts a b c d))
            (multiple-value-list (funcall #'clasp::g-test table))
	    (list table)
            '(("c1" "c2") ("r1" "r2"))))
	 (result (funcall 'make-chi-square-result-from-list values)))
    (setf (description result)
      (format nil "chi-square of ~a, ~a, ~a, ~a" a b c d))
    (assert (= (length values) 8) nil
      "`chi-square-2x2' returned ~s values, we were expecting 8"
      (length values))
    (present-with-update result
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-chi-square-2x2 :command-table clasp-tests :name t
				    :menu "Chi-Square 2x2")
    ((column-2 'numbers :prompt "y" :mapping-option :map)
     (column-1 'numbers :prompt "x" :mapping-option :map))
  (let* ((values
	  (multiple-value-list (funcall
				#'chi-square-2x2
				(simplify column-1)
				(simplify column-2))))
	 (result (funcall 'make-chi-square-result-from-list values)))
    (setf (description result)
      (format nil "chi-square of ~a and ~a"
	      (description column-1) (description column-2)))
    (assert (= (length values) 8) nil
      "`chi-square-2x2' returned ~s values, we were expecting 8"
      (length values))
    (present-with-update result
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-chi-square-rxc :command-table clasp-tests :name t
				    :menu "Chi-Square RxC")
    ((column-2 'numbers :prompt "y" :mapping-option :map)
     (column-1 'numbers :prompt "x" :mapping-option :map))
  (let* ((values
	  (multiple-value-list (funcall
				#'chi-square-rxc
				(simplify column-1)
				(simplify column-2))))
	 (result (funcall 'make-chi-square-result-from-list values)))
    (setf (description result)
      (format nil "chi-square of ~a and ~a"
	      (description column-1) (description column-2)))
    (assert (= (length values) 8) nil
      "`chi-square-rxc' returned ~s values, we were expecting 8"
      (length values))
    (present-with-update result
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-linear-regression-brief
		 :command-table clasp-tests :name t
		 :menu "Linear Regression - brief")
    ((column-1 'numbers :prompt "y" :mapping-option :map)
     (column-2 'numbers :prompt "x" :mapping-option :map))
  (let* ((values
	  (multiple-value-list (funcall
				#'linear-regression-brief
				(simplify column-1)
				(simplify column-2))))
	 (result (funcall 'make-linear-regression-brief-result-from-list
			  values)))
    (setf (description result)
      (format nil "linear regression of ~a on ~a"
	      (description column-1) (description column-2)))
    (assert (= (length values) 5) nil
      "`linear-regression-brief' returned ~s values, we were expecting 5"
      (length values))
    (present-with-update result
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-linear-regression-verbose
		 :command-table clasp-tests :name t
		 :menu "Linear Regression - verbose")
    ((column-1 'numbers :prompt "y" :mapping-option :map)
     (column-2 'numbers :prompt "x" :mapping-option :map))
  (let* ((values
	 (multiple-value-list (funcall
			       #'linear-regression-verbose
			       (simplify column-1)
			       (simplify column-2))))
	 (result (funcall 'make-linear-regression-verbose-result-from-list
			  values :dv column-1 :iv column-2)))
    (setf (description result)
      (format nil "linear regression of ~a on ~a"
	      (description column-1) (description column-2)))
    (assert (= (length values) 7) nil
      "`linear-regression-verbose' returned ~s values, we were expecting 7"
      (length values))
    (present-with-update result
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-multiple-linear-regression-verbose
		       :command-table clasp-tests :name t
		       :menu "Multiple Linear Regression - verbose")
    ((dv 'numbers :prompt "y")
     (ivs '(sequence numbers) :prompt "x(s)"))
  (let* ((values
	  (multiple-value-list (apply
				#'clasp::multiple-linear-regression
				(simplify dv)
				(mapcar #'simplify ivs))))
	 (result (funcall 'make-multiple-linear-regression-verbose-result-from-list
			  values
			  :dv dv
			  :ivs ivs)))
    (setf (description result)
      (format nil "multiple linear regression of ~a on ~{~a~^ and ~}"
	      (description dv) (mapcar #'description ivs)))
    (assert (= (length values) 13) nil
      "`multiple-linear-regression' returned ~s values, we were expecting 13"
      (length values))
    (present-with-update result
			 :stream *standard-output* :view +dialog-view+)))

;;; ---------------------------------------------------------------------------
;;; old Command definition

#+ignore
(defclaspcom (confidence-interval-z
	      :menu "Confidence Interval Using z Statistic") tests
  #-lucid (column (real 0 1))
  #+lucid (column real)
  (simplify t)
  (t t t) (((labeled-real) :label "Mean")
	   ((labeled-real) :label "Lower Bound")
	   ((labeled-real) :label "Upper Bound"))
  #+ignore(make-confidence-interval-result-from-list)
  #+ignore(confidence-interval-result)
  :input-options ((:prompt "Variable") (:prompt "Confidence level")))

#+ignore
(defclaspcom (confidence-interval-t
	      :menu "Confidence Interval Using t Statistic") tests
  #-lucid (column (real 0 1))
  #+lucid (column real)
  (simplify t)
  (t t t) (((labeled-real) :label "Mean")
	   ((labeled-real) :label "Lower Bound")
	   ((labeled-real) :label "Upper Bound"))
  #+ignore(make-confidence-interval-result-from-list)
  #+ignore(confidence-interval-result)
  :input-options ((:prompt "Variable") (:prompt "Confidence level")))

#+ignore
(defclaspcom (confidence-interval-proportion
	      :menu "Confidence Interval Using Proportion") tests
  #-lucid ((integer 0 *) (integer 1 *) (real 0 1))
  #+lucid (integer integer real)
  (t t t)
  (t t t) (((labeled-real) :label "P-Hat")
	   ((labeled-real) :label "Lower Bound")
	   ((labeled-real) :label "Upper Bound"))
  #+ignore(make-confidence-interval-result-from-list)
  #+ignore(confidence-interval-result)
  :input-options ((:prompt "Successes") (:prompt "Trials") (:prompt "Confidence level")))

#+ignore
(defclaspcom (t-test-one-sample :menu "T-test One Sample") tests
  (column (member :both :positive :negative)) (simplify t)
  (t t t t) (((labeled-real) :label "t Statistic")
	     ((labeled-real) :label "Significance")
	     ((labeled-real) :label "Sample Error")
	     ((labeled-real) :label "Degrees of Freedom"))
  #+ignore (make-t-test-result-from-list) #+ignore(t-test-result)
  :input-options ((:prompt "Sample")
		  (:prompt "Tails: [Both, Positive or Negative]")))

#+ignore
(defclaspcom (t-test :menu "T-test Two Samples") tests
  (column column (member :both :positive :negative)) (simplify simplify t)
  (t t t t) (((labeled-real) :label "t Statistic")
	     ((labeled-real) :label "Significance")
	     ((labeled-real) :label "Sample Error")
	     ((labeled-real) :label "Degrees of Freedom"))
  #+ignore (make-t-test-result-from-list) #+ignore(t-test-result)
  :input-options ((:prompt "Sample 1") (:prompt "Sample 2")
		  (:prompt "Tails: [Both, Positive or Negative]")))

#+ignore
(defclaspcom (t-test-matched :menu "T-test Matched Pairs") tests
  (column column (member :both :positive :negative)) (simplify simplify t)
  (t t t t) (((labeled-real) :label "t Statistic")
	     ((labeled-real) :label "Significance")
	     ((labeled-real) :label "Sample Error")
	     ((labeled-real) :label "Degrees of Freedom"))
  #+ignore (make-t-test-result-from-list) #+ignore(t-test-result)
  :input-options ((:prompt "Sample 1") (:prompt "Sample 2")
		  (:prompt "Tails: [Both, Positive or Negative]")))

#+ignore
(defclaspcom (d-test :menu "D test") tests
  (column column (member :both :positive :negative)) (simplify simplify t)
  (t t) (((labeled-real) :label "D Statistic")
	 ((labeled-real) :label "Significance"))
  #+ignore(make-d-test-result-from-list) #+ignore(d-test-result)
  :input-options ((:prompt "Sample 1") (:prompt "Sample 2")
		  (:prompt "Tails: [Both, Positive or Negative]")))

#+ignore
(defclaspcom (anova-one-way-variables :menu "Anova - One Way") tests
  (column column) (simplify-and-sort (simplify-and-sort-by 1 0))
  (make-anova-one-way-table-from-list t make-scheffe-table-from-list t)
  (anova-one-way-table
   (sequence number)
   scheffe-table
   ((labeled-real) :label "Alternate Sum-of-Squares Total"))
  :input-options ((:prompt "iv")  (:prompt "dv"))) 

#+ignore
(defclaspcom (chi-square-2x2 :menu "Chi-Square 2x2") tests
  (column column) (simplify simplify)
  (t t) (((labeled-real) :label "Chi-square Statistic")
	 ((labeled-real) :label "Significance"))
  :input-options ((:prompt "Sample 1") (:prompt "Sample 2")))

#+ignore
(defclaspcom (chi-square-rxc :menu "Chi-Square RxC") tests
  (column column) (simplify simplify)
  (t t) (((labeled-real) :label "Chi-square Statistic")
	 ((labeled-real) :label "Significance"))
  :input-options ((:prompt "Sample 1") (:prompt "Sample 2")))

#+ignore
(defclaspcom (linear-regression-brief :menu "Linear Regression - Brief") tests
  (column column) (simplify simplify)
  (t t t t t) (((labeled-real) :label "Slope")
	       ((labeled-real) :label "Intercept")
	       ((labeled-real) :label "R^2 (Coefficient of Determination)")
	       ((labeled-real) :label "Standard Error of the Slope")
	       ((labeled-real) :label "P Value"))
  :input-options ((:prompt "DV") (:prompt "IV")))

#+ignore
(defclaspcom (linear-regression-verbose
	      :menu "Linear Regression - Verbose") tests
  (column column) (simplify simplify)
  (t t t t t t make-anova-one-way-table-from-list)
  (((labeled-real) :label "Slope")
   ((labeled-real) :label "Intercept")
   ((labeled-real) :label "R^2 (Coefficient of Determination)")
   ((labeled-real) :label "Correlation")
   ((labeled-real) :label "Standard Error of the Slope")
   ((labeled-real) :label "Standard Error of the Intercept")
   anova-one-way-table)
  :input-options ((:prompt "DV") (:prompt "IV")))

#+ignore
(defstat f-test tests (column column) (simplify simplify)
  (t t) 
  (((labeled-real) :label "f")
   ((labeled-real) :label "f significance")))

#+ignore
(defstat U-test tests (column column) (simplify simplify) (t) (nice-real))


;;; ---------------------------------------------------------------------------
;;; EOF
