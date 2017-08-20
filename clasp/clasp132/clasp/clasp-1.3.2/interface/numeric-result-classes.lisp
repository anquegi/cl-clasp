;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package:CLASP-INTERFACE -*-
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

;;;---------------------------------------------------------------------------

(defclass numeric-result (result)
	  ((value :initarg :value :accessor value))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Number"
    :target-form #'print-args))

(defmethod show-in-results-display ((self numeric-result))
  nil)

(defmethod display ((self numeric-result) stream)
  (present (value self) 'nice-real :stream stream :view +dialog-view+))

;;;---------------------------------------------------------------------------

(defclass mode-result (numeric-result)
	  ((number-of-occurences :initarg :number-of-occurences
				:accessor number-of-occurences))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Mode"
    :target-form #'print-args))

(defmethod display ((self mode-result) stream)
  (flet ((format-item (name)
	   (let ((value (slot-value self name)))
	     (formatting-cell
	      (stream :align-x :right)
	      (present value 'nice-real :stream stream))))
	 (format-label (label)
	   (formatting-cell
	    (stream :align-x :right)
	    (with-text-style
		(*label-text-style* stream)
	      (format stream label)))))
  (formatting-table
   (stream :inter-column-spacing '(2 :character))
   (formatting-row
    (stream)
    (format-label "Mode") (format-item 'value))
   (formatting-row
    (stream)
    (format-label "Number of occurences")
    (format-item 'number-of-occurences)))))

;;;---------------------------------------------------------------------------

(defclass statistical-summary-result (numeric-result)
	  ((column-length       :initarg :column-length)
	   (min                 :initarg :min)
	   (max                 :initarg :max)
	   (range               :initarg :range)
	   (median              :initarg :median)
	   (mode                :initarg :mode)
	   (mean                :initarg :mean)
	   (variance            :initarg :variance)
	   (standard-deviation  :initarg :standard-deviation)
	   (interquartile-range :initarg :interquartile-range))
  (:default-initargs
   :bitmap null-pattern
   :command-form #'print-args
   :command-documentation "Statistical Summary"
   :target-form #'print-args))

(defun make-statistical-summary-result (&rest options)
  (apply #'make-instance 'statistical-summary-result options))

(defun make-statistical-summary-result-from-list
    (list &rest initargs &key &allow-other-keys)
  (destructuring-bind 
      (column-length min max range median mode mean variance
		     standard-deviation interquartile-range)
      list
    (apply #'make-statistical-summary-result
     :column-length column-length :min min :max max :range range
     :median median :mode mode :mean mean :variance variance
     :standard-deviation standard-deviation
     :interquartile-range interquartile-range
     initargs)))


(defmethod display ((self statistical-summary-result) stream)
  (flet ((format-labeled-item (name &optional label)
	  (let ((value (slot-value self name)))
	    (formatting-row
	     (stream)
	     (formatting-cell
	      (stream :align-x :right)
	      (with-text-style
	       (*label-text-style* stream)
	       (format stream "~@(~a~)" (or label name))))
	     (formatting-cell
	      (stream :align-x :right)
	      (present value 'nice-real :stream stream))))))
      (formatting-table
       (stream :inter-column-spacing '(2 :character))
	(format-labeled-item 'column-length "length")
	(format-labeled-item 'min)
	(format-labeled-item 'max)
	(format-labeled-item 'range)
	(format-labeled-item 'median)
	(format-labeled-item 'mode)
	(format-labeled-item 'mean)
	(format-labeled-item 'variance)
	(format-labeled-item 'standard-deviation "standard deviation")
	(format-labeled-item 'interquartile-range "interquartile range"))))
