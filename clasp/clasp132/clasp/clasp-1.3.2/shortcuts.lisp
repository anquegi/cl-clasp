;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/v3dev/shortcuts.lisp *-*
;;;; *-* Last-edit: Monday, March 8, 1993  12:42:10; Edited-By: carlson *-* 
;;;; *-* Machine: Lester (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                           Shortcut Functions                           *
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
;;;  02-17-93 File Created.  (carlson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clasp)

;;; --*--
;;; ***************************************************************************

;;; These functions are either shortcuts for commonly used
;;; transformations, hacks for the clim interface or first drafts of
;;; functions which should eventually go into another file like
;;; statistical-fns.lisp

(defun add-a-constant (variable number)
  (create-new-column-from-function
    (variable-dataset variable)
    (list '+ (id variable) number)))

(defun natural-log (variable)
  (create-new-column-from-function
    (variable-dataset variable)
    (list 'log (id variable))))

(defun log10 (variable)
  (create-new-column-from-function
    (variable-dataset variable)
    (list 'log (id variable) 10)))

(defun smooth-variable-4253H (variable)
  (add-variable-to-dataset
   (variable-dataset variable)
   (smooth-4253H (variable-value variable))
   (format nil "Smooth of ~a" (id variable))))

(defun recode-categorical (variable old-values new-values)
  (create-new-column-from-function
    (variable-dataset variable)
    (list 'recode (id variable) (list (quote quote) old-values) 
	  (list (quote quote) new-values))))

(defun rebin (variable limits new-values)
  (create-new-column-from-function
   (variable-dataset variable)
   (list 'bin (id variable) (list (quote quote) limits)
	 (list (quote quote) new-values))))

(defun bin (value max-values bin-names)
  (let ((bins (mapcar #'list (append max-values (list most-positive-fixnum))
		      bin-names)))
    (setf bins (sort bins #'(lambda (x &rest y)
			      (apply #'< (car x)
				     (mapcar #'car y)))))
    (dolist (bin bins)
      (when (<= value (car bin))
	(return-from bin (cadr bin))))))

(defun formula (dataset expression)
  (create-new-column-from-function dataset expression))

(defun difference-a-column (variable)
  (add-variable-to-dataset
   (variable-dataset variable)
   (pairwise-difference (variable-value variable))
   (format nil "Differenced ~a" (id variable))))

(defun pairwise-difference (data)  
  (cons 0 (mapcar #'(lambda (x1 x2) (- x1 x2))
		  data
		  (cdr data))))

(defun sort-variable (variable index-variable)
  (when (not (eq (variable-dataset variable)
		 (variable-dataset index-variable)))
    (error 'variables-from-different-datasets))
  (add-variable-to-dataset
   (variable-dataset variable)
   (variable-value variable
		   :order-by (car (variable-attribute index-variable)))
   (if (eq variable index-variable)
       (format nil "sorted-~a" (id variable))
     (format nil "~a-sorted-by-~a" (id variable) (name index-variable)))
   (if (eq variable index-variable)
       (format nil "sorted-~a" (description variable))
     (format nil "~a sorted by ~a"
	     (description variable) (description index-variable)))))

;;; ***************************************************************************
;;; EOF
