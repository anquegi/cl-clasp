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

(in-package :CLASP-INTERFACE)

;;;****************************************************************
;;; copied some defs from icons.lisp
;;;
;;; This contains code for bitmap (Clim 'pattern') icons including
;;; stationary icons, mouse-movability, command-icons (buttons),
;;; and dragged-icons (drag icon and execute function on a target)
;;;
;;;****************************************************************


(defclass icon (clasp::named-object)
  ((file :initform nil :initarg :file :accessor file)
   (bitmap :initform nil :initarg :bitmap :accessor bitmap)
   (presentation :initform nil :initarg :presentation :accessor presentation)
   (label-p :initform t :initarg :label-p :accessor label-p)
   (height :initform nil :initarg :height :accessor height)
   (width :initform nil :initarg :width :accessor width)
   (x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)
   (icon-window :initform nil :initarg :icon-window :accessor icon-window))
  )

(defclass command-icon (icon)		; AKA a "button"
	  ((command-form :initform nil :initarg :command-form
			     :accessor command-form)
	   (command-documentation :initform (make-string 0)
			  :initarg :command-documentation
			  :accessor command-documentation)
	   ))

(defclass dragged-icon (command-icon)
	  ((target :initform nil
		   :initarg :target
		   :accessor target)
	   (target-form :initform nil
			    :initarg :target-form
			    :accessor target-form)
	   ))
