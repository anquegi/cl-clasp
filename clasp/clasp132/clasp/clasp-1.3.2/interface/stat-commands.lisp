;;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/v3dev/interface/data-manipulation-commands.lisp *-*
;;;; *-* Last-edit: Thursday, February 18, 1993  00:54:05; Edited-By: carlson *-* 
;;;; *-* Machine: Miles (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                        Data Description Commands                       *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: David L. Westbrook
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

(define-command-table clasp-description)

(add-menu-item-to-command-table 'clasp "Describe" 
                                :menu 'clasp-description 
				:documentation "Descriptive Statistics"
                                :errorp nil 
                                :after "graph")

;;; ---------------------------------------------------------------------------
;;; I ordered these the same way they are in the statistical-fns file.

(define-clasp-command (com-data-length :command-table clasp-description
				       :name t :menu "Data Length")
    ((column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values (multiple-value-list
		    (funcall #'data-length (simplify column-1)))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'data-length (length values) 1)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil "~@(data length ~a~)"
						(description column-1)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-mean :command-table clasp-description :name t
                                 :menu "Mean")
    ((column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values
	 (multiple-value-list (funcall #'mean (simplify column-1)))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'mean (length values) 1)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil "~@(mean ~a~)"
						(description column-1)))
			 :stream *standard-output* :view +dialog-view+)))
 
(define-clasp-command (com-variance :command-table clasp-description :name t
                                     :menu "Variance")
    ((column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values (multiple-value-list
		    (funcall #'variance (simplify column-1)))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'variance (length values) 1)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil "~@(variance ~a~)"
						(description column-1)))
			 :stream *standard-output* :view +dialog-view+)))
 
(define-clasp-command (com-standard-deviation :command-table clasp-description
					:name t :menu "Standard Deviation")
    ((column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values (multiple-value-list
		    (funcall #'standard-deviation (simplify column-1)))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'standard-deviation (length values) 1)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil
						"~@(standard deviation ~a~)"
						(description column-1)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-minimum :command-table clasp-description
			     :name t :menu "Minimum")
    ((column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values (multiple-value-list
		    (funcall #'minimum (simplify column-1)))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'minimum (length values) 1)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil "~@(minimum ~a~)"
						(description column-1)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-maximum :command-table clasp-description
			     :name t :menu "Maximum")
    ((column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values (multiple-value-list
		    (funcall #'maximum (simplify column-1)))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'maximum (length values) 1)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil "~@(maximum ~a~)"
						(description column-1)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-range :command-table clasp-description
			   :name t :menu "Range")
    ((column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values (multiple-value-list
		    (funcall #'range (simplify column-1)))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'range (length values) 1)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil "~@(range ~a~)"
						(description column-1)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-quantile :command-table clasp-description :name t
			      :menu "Quantile")
    ((column-1 'numbers :prompt "x" :mapping-option :map)
     (real-2 #-lucid '(real 0 1) #+lucid 'real :prompt "percentile (real from 0 to 1)"))
  (let ((values (multiple-value-list
		    (funcall #'quantile (simplify column-1) real-2))))
    (assert (= (length values) 3) nil
      "`~a' returned ~s values, we were expecting ~s"
      'quantile (length values) 3)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil "~@(quantile(~1,2f) ~a~)"
						real-2 (description column-1)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-median :command-table clasp-description 
			    :name t :menu "Median")
    ((column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values	(multiple-value-list
		    (funcall #'median (simplify column-1)))))
    (assert (= (length values) 3) nil
      "`~a' returned ~s values, we were expecting ~s"
      'median (length values) 3)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil "~@(median ~a~)"
						(description column-1)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-trimmed-mean :command-table clasp-description
				  :name t :menu "Trimmed Mean")
    ((column-1 'numbers :prompt "x" :mapping-option :map)
     (real-2 #-lucid '(real 0 1/2) #+lucid 'real :prompt "trimming factor (real from 0 to .5)"))
  (let ((values (multiple-value-list
		    (funcall #'trimmed-mean (simplify column-1) real-2))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'trimmed-mean (length values) 1)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil
						"~@(trimmed mean (~1,2f) ~a~)"
						real-2 (description column-1)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-mode :command-table clasp-description :name t
			  :menu "Mode")
    ((column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values
	 (multiple-value-list (funcall
			       #'mode
			       (simplify column-1)))))
    (assert (= (length values) 2) nil
      "`~a' returned ~s values, we were expecting ~s"
      'mode (length values) 2)
    (present-with-update (make-instance 'mode-result
			   :value (nth 0 values)
			   :number-of-occurences (nth 1 values)
			   :description (format nil "~@(mode ~a~)"
						(description column-1)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-interquartile-range
		       :command-table clasp-description
		       :name t :menu "Interquartile Range")
    ((column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values (multiple-value-list
		    (funcall #'interquartile-range (simplify column-1)))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'interquartile-range (length values) 1)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil
						"~@(interquartile range ~a~)"
						(description column-1)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-statistical-summary
		       :command-table clasp-description
		       :name t :menu "Statistical Summary")
    ((column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values (multiple-value-list
		    (funcall #'statistical-summary (simplify column-1)))))
    (assert (= (length values) 10) nil
      "`~a' returned ~s values, we were expecting ~s"
      'statistical-summary (length values) 10)
    (present-with-update
     (make-statistical-summary-result-from-list
      values :description (format nil "~@(statistical summary of ~a~)"
				  (description column-1)))
     :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-covariance :command-table clasp-description
				      :name t :menu "Covariance")
    ((column-2 'numbers :prompt "y" :mapping-option :map)
     (column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values (multiple-value-list
		    (funcall #'covariance
			     (simplify column-1) (simplify column-2)))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'covariance (length values) 1)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil
						"~@(covariance of ~a and ~a~)"
						(description column-1)
						(description column-2)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-correlation :command-table clasp-description
				 :name t :menu "Correlation")
    ((column-2 'numbers :prompt "y" :mapping-option :map)
     (column-1 'numbers :prompt "x" :mapping-option :map))
  (let ((values
	 (multiple-value-list (funcall
			       #'correlation
			       (simplify column-1)
			       (simplify column-2)))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'correlation (length values) 1)
    (present-with-update (make-instance 'numeric-result
			   :value (nth 0 values)
			   :description (format nil
						"~@(correlation of ~a and ~a~)"
						(description column-1)
						(description column-2)))
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-cross-correlation :command-table clasp-description
				       :name t :menu "Cross Correlation")
    ((column-1 'numbers :prompt "y" :mapping-option :map)
     (column-2 'numbers :prompt "x" :mapping-option :map)
     (min-lag 'integer :prompt "min lag")
     (max-lag 'integer :prompt "max lag"))
  (when (< max-lag min-lag)
    (let (temp)
      (setf temp column-1
	    column-1 column-2
	    column-2 temp)
      (setf temp min-lag
	    min-lag max-lag
	    max-lag temp)))
  (let* ((values
	 (multiple-value-list (funcall
			       #'cross-correlation
			       (simplify column-1)
			       (simplify column-2)
			       max-lag min-lag)))
	 (number-seq
	  (make-instance 'number-sequence
	    :value (nth 0 values)
	    :description
	    (format nil
		    "cross correlation of ~a and ~a with lags from ~a to ~a"
		    (description column-1)
		    (description column-2)
		    min-lag max-lag))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'cross-correlation (length values) 1)
    (present-with-update number-seq
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-autocorrelation :command-table clasp-description
				     :name t :menu "Autocorrelation")
    ((column-1 'numbers :prompt "x" :mapping-option :map)
     (min-lag 'integer :prompt "min lag" :default 0)
     (max-lag 'integer :prompt "max lag"))
  (when (< max-lag min-lag)
    (let ((temp max-lag))
      (setf max-lag min-lag)
      (setf min-lag temp)))
  (let* ((values
	  (multiple-value-list (funcall
				#'autocorrelation
				(simplify column-1)
				max-lag min-lag)))
	 (number-seq
	  (make-instance 'number-sequence
	   :value (nth 0 values)
	   :description
	   (format nil "autocorrelation of ~a with lags from ~a to ~a"
		   (description column-1)
		   min-lag max-lag))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'autocorrelation (length values) 1)
    (present-with-update number-seq
	     :stream *standard-output* :view +dialog-view+)))

;;; ---------------------------------------------------------------------------
;;; I ordered these the same way they are in the statistical-fns file.

#+ignore
(defclaspcom data-length description (column) (simplify) (t) (nice-real)
  :input-options ((:prompt "Column")))

#+ignore
(defclaspcom mean description (column) (simplify) (t) (nice-real)
  :input-options ((:prompt "Column")))

#+ignore
#+ignore
(defclaspcom sum-of-squares description (column) (simplify) (t) (nice-real)
  :input-options ((:prompt "Column")))

#+ignore
(defclaspcom variance description (column) (simplify) (t) (nice-real)
  :input-options ((:prompt "Column")))

#+ignore
(defclaspcom standard-deviation description (column) (simplify) (t) (nice-real)
  :input-options ((:prompt "Column")))

#+ignore
(defclaspcom minimum description (column) (simplify) (t) (nice-real)
  :input-options ((:prompt "Column")))

#+ignore
(defclaspcom maximum description (column) (simplify) (t) (nice-real)
 :input-options ((:prompt "Column")))

#+ignore
(defclaspcom range description (column) (simplify) (t) (nice-real)
  :input-options ((:prompt "Column")))

#+ignore
(defclaspcom quantile description
  #-lucid (column (real 0 1))
  #+lucid (column real)
  (simplify t) (t) (nice-real)
  :input-options ((:prompt "Column") (:prompt "Real from 0 to 1")))

#+ignore
(defclaspcom median description (column) (simplify) (t) (nice-real)
  :input-options ((:prompt "Column")))

#+ignore
(defclaspcom trimmed-mean description
  #-lucid (column (real 0 1/2))
  #+lucid (column real)
  (simplify t) (t) (nice-real)
  :input-options ((:prompt "Column") (:prompt "Real from 0 to .5")))

#+ignore
(defclaspcom mode description (column) (simplify) (t t) (nice-real integer)
  :input-options ((:prompt "Column")))

#+ignore
(defclaspcom interquartile-range description
  (column) (simplify) (t) (nice-real)
  :input-options ((:prompt "Column")))

;; This needs some work to label the output.
#+ignore
(defclaspcom statistical-summary description (column) (simplify) 
  (t t t t t t t t t t) (((labeled-real) :label "Length")
                       ((labeled-real) :label "Min")
                       ((labeled-real) :label "Max")
                       ((labeled-real) :label "Range")
                       ((labeled-real) :label "Median")
                       ((labeled-real) :label "Mode")
                       ((labeled-real) :label "Mean")
                       ((labeled-real) :label "Variance")
                       ((labeled-real) :label "SD")
		       ((labeled-real) :label "IQR"))
  :input-options ((:prompt "Column")))

#+ignore
(defclaspcom covariance  description
  (column column) (simplify simplify) (t) (nice-real)
  :input-options ((:prompt "Column 1") (:prompt "Column 2")))

#+ignore
(defclaspcom correlation description
  (column column) (simplify simplify) (t) (nice-real)
   :input-options ((:prompt "Column 1") (:prompt "Column 2")))

#+ignore
(defclaspcom standard-error description (column) (simplify) (t) (nice-real))

#+ignore
(defclaspcom spearmans-rho description (column column) (simplify simplify) 
  (t t) 
  (((labeled-real) :label "rho")
   ((labeled-real) :label "df")))

#+ignore
(defclaspcom cross-correlation description
  (column column integer) (simplify simplify t)
	 (t) ((sequence nice-real))
  :input-options ((:prompt "Column 1") (:prompt "Column 2") (:prompt "Lag")))

#+ignore
(defclaspcom autocorrelation description (column integer) (simplify t)
	 (t) ((sequence nice-real))
  :input-options ((:prompt "Column 1") (:prompt "Lag")))
	 

;;; ----------------------------------------------------------------------------     
     
#+Old
'(("Mean" :value mean)
    ("Median" :value median)
    ("Standard Deviation" :value std-deviation)
    ("Variance" :value variance)
    ("Correlation" :value r-score)
    ("Spearmans rank order correlation coefficient" :value spearmans-rho)
    ("T-Test Pooled variance" :value t-test)
    ("T-Test Matched Pairs" :value t-test-matched)
    ("One way ANOVA one treatment." :value one-way)
    ("Two way ANOVA two treatments." :value two-way)
    ("Univariate Linear Regression" :value linear-regression)
    ("Multivariate Linear Regression" :value multi-linear-regression)
    ("Scatter Plot of Y on X with linear regression" :value plot-y-on-x)
    ("Loglinear analysis Depth by Row by Column" :value log-linear)
    ("One or more histograms of a single column." :value histogram)
    ("Mann-Whitney U-test" :value u-test)
    ("Chi-Square RxC" :value chi**2)
    ("Chi-Square 2x2" :value chi-2-2)
    ("Statistical Summary" :value statistical-summary)
    #+NOT-THIS
    ("Exit" :value exit :documentation "Exit the menu loop"))
