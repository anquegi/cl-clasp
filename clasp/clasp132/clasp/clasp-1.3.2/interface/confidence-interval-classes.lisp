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

(in-package :clasp-interface)

;;; ---------------------------------------------------------------------------
;;; Confidence-interval Result

(defclass confidence-interval-result (modifiable-display-option-mixin result)
  ((display-option :allocation :class :initform :display-in-interactor)
   (mean        :initarg :mean)
   (lower-bound :initarg :lower-bound)
   (upper-bound :initarg :upper-bound))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Open Confidence Interval"
    :target-form #'execute-on-target))

(defun make-confidence-interval-result (&rest options)
  (apply #'make-instance 'confidence-interval-result options))

(defun make-confidence-interval-result-from-list (list)
  (destructuring-bind 
    (mean lower-bound upper-bound)
    list
    (make-confidence-interval-result :mean mean :lower-bound lower-bound
			:upper-bound upper-bound)))

(defmethod display ((confidence-interval-result confidence-interval-result)
		    stream)
  (fresh-line stream)
  (flet ((format-item (name)
           (let ((value (slot-value confidence-interval-result name)))
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
      (format-label "Mean")
      (format-label "Lower Bound") (format-label "Upper Bound"))
     (formatting-row
      (stream)
      (format-item 'mean)
      (format-item 'lower-bound) (format-item 'upper-bound)))))

