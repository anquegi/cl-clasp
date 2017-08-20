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

(in-package :CLASP-INTERFACE)

(defclass result (dragged-icon clasp::named-object clasp::remember-instances)
  ((display-window :initform nil :initarg :window :accessor display-window)))

(defclass modifiable-display-option-mixin ()
	  ((display-option :initarg :display-option
			   :type (member :display-in-interactor
					 :display-in-window :iconify nil)
			   :accessor display-option
			   :allocation :class
			   :initform :display-in-interactor)))

(defmethod modifiable-display-option-p ((class t))
  nil)

(defmethod modifiable-display-option-p
    ((class modifiable-display-option-mixin))
  t)

(defmethod display-option ((self t))
  :iconify)

(defmethod display-option ((self result))
  :display-in-interactor)

(defmethod show-in-results-display ((self result))
  t)

(defclass dataset (result clasp::dataset)
  ()
  (:default-initargs
    :bitmap dataset-pattern
    :command-form #'print-args
    :command-documentation "Dataset"
    :target-form #'print-args))

(defmethod display-option ((self dataset))
  :iconify)

(defmethod show-in-results-display ((self dataset))
  nil)

(defclass numbers (result)
  ()
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Numbers"
    :target-form #'print-args))

(defclass column (numbers clasp::variable)
  ()
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Column"
    :target-form #'print-args))

(defmethod display-option ((self column))
  :iconify)

(defmethod show-in-results-display ((self column))
  nil)

(defclass number-sequence (numbers)
  ((value :initarg :value :accessor value))
  (:default-initargs
    :bitmap null-pattern
    :command-form #'print-args
    :command-documentation "Number Sequence"
    :target-form #'print-args))

(defclass graph-data-icon (modifiable-display-option-mixin result)
	  ((display-option :allocation :class :initform :display-in-interactor)
	   (graph-data :initarg :graph-data :reader graph-data)
           (x-label :initform nil :initarg :x-label :reader x-label)
           (y-label :initform nil :initarg :y-label :reader y-label)
	   (graph :initarg :graph :initform nil :reader graph))
  (:default-initargs
    :bitmap graph-pattern
    :command-form #'print-args
    :command-documentation "Graph this data"
    :target-form #'execute-on-target))

(defclass graph (result graph:annotated-graph) ()
  (:default-initargs
    :bitmap graph-pattern
    :command-form #'print-args
    :command-documentation "Graph"
    :target-form #'print-args))

(defmethod show-in-results-display ((self graph))
  nil)

(defmethod display-option ((self graph))
  nil)

(defclass window-point (standard-point)
	  ((window :initarg :window :accessor window)))
