;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package:CLASP-INTERFACE -*-
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

(in-package :clasp-interface)

(defvar *last-command-result* nil)

(defmethod present-with-update ((self cons)
				&key stream view type
				&allow-other-keys)
  (let (presentation)
    (cond
     ((eq (display-option self) :iconify)
      (setf presentation
	(present self (or type (list 'sequence (type-of (car self))))
		 :stream stream :view view)))
     ((eq (display-option self) :display-in-interactor)
      (setf presentation
	(with-output-as-presentation
	    (:object self :type (or type (list 'sequence (type-of (car self))))
		     :stream stream)
	  (mapcar #'(lambda (self-element)
		      (display self-element stream))
		  self))))
     ((eq (display-option self) :display-in-window)
      (setf presentation
	(present self (or type (list 'sequence (type-of (car self))))
		 :stream stream :view view))
      (dolist (self-element self) (open-it self-element :refresh t)))
     (t nil))
    (terpri stream)
    presentation))


(defmethod present-with-update ((self result)
				&key stream view type
				&allow-other-keys)
  (declare (special mapping-p))
  (when mapping-p
    (with-text-style
	(*label-text-style* stream)
      (format stream "~a~%" (description self))))
  (let (presentation)
    (cond
     ((eq (display-option self) :iconify)
      (setf presentation
	(present self (or type (type-of self)) :stream stream :view view)))
     ((eq (display-option self) :display-in-interactor)
      (setf presentation
	(with-output-as-presentation
	    (:object self :type (or type (type-of self)) :stream stream)
	  (display self stream))))
     ((eq (display-option self) :display-in-window)
      (setf presentation
	(present self (or type (type-of self)) :stream stream :view view))
      (open-it self :refresh t))
     (t nil))
    (terpri stream)
    presentation))

(defmethod present-with-update :after ((self cons)
				       &key stream view type
				       &allow-other-keys)
  (declare (ignore stream view type iconify))
  (update-datasets-display)
  (update-results-display))

(defmethod present-with-update :after ((self result)
				       &key stream view type
				       &allow-other-keys)
  (declare (ignore stream view type iconify))
  (update-results-display))

(defmethod present-with-update :after ((self dataset)
				       &key stream view type
				       &allow-other-keys)
  (declare (ignore stream view type iconify))
  (update-datasets-display))

(defmethod present-with-update :after ((self column)
				       &key stream view type
				       &allow-other-keys)
  (declare (ignore stream view type iconify))
  (update-datasets-display))

#+ignore
(defmethod initialize-instance :after
	   ((self result) &rest initargs)
  (declare (ignore initargs))
  (update-results-display)
  (auto-open self))

#+ignore
(defmethod initialize-instance :after
	   ((self dataset) &rest initargs)
  (declare (ignore initargs))
  (update-datasets-display)
  (auto-open self))

#+ignore
(defmethod initialize-instance :after
	   ((self column) &rest initargs)
  (declare (ignore initargs))
  (update-datasets-display)
  (auto-open self))

#+ignore
(defmethod (setf name) :after (name (self result))
  (declare (ignore name))
  (update-results-display))

#+ignore
(defmethod (setf name) :after (name (self dataset))
  (declare (ignore name))
  (update-datasets-display))

#+ignore
(defmethod (setf name) :after (name (self variable))
  (declare (ignore name))
  (update-datasets-display))
  
#+ignore
(defmethod initialize-instance :after
	   ((self dataset) &rest initargs)
  (declare (ignore initargs))
  (update-datasets-display)
  (auto-open self))

#+ignore
(define-presentation-method present :after
  (result (type result) stream view &key)
  (declare (ignore stream view))
  (update-results-display)
  (auto-open result))

#+ignore
(define-presentation-method present :after
  (dataset (type dataset) stream view &key)
  (declare (ignore stream view))
  (update-datasets-display)
  (auto-open dataset))

