;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clasp/clasp/development/translation-fns.lisp *-*
;;;; *-* Last-edit: Wednesday, July 21, 1993  17:04:32; Edited-By: carlson *-* 
;;;; *-* Machine: Miles (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                          Translation Functions                         *
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
;;;  06-30-92 File Created.  (Carlson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clasp)

;;; --*--
;;; ***************************************************************************


;;; ***************************************************************************
;;; dataset translation functions
;;; ***************************************************************************

(defun dataset-to-columns (data &key rows columns)
  "Takes a dataset `data' and extracts tuples representing the columns of data
in the dataset.  `rows' is a where-clause selecting which rows of the dataset
to select. `columns' is a list of variable names to return.  By default, all
rows and all columns are returned."
  (transpose (dataset-to-rows (get-dataset data) :rows rows :columns columns)))

(defun dataset-to-rows (data &key rows columns)
  "Takes a dataset `data' and extracts tuples representing the rows of
data in the dataset.  `rows' is a where-clause selecting which rows of
the dataset to select. `columns' is a list of variable names to
return.  By default, all rows and all columns are returned."
  (when columns
    (setf columns (mapcar #'(lambda (x)
			      (car (variable-attribute
				    (get-variable x 
						  (get-dataset data)))))
			  columns)))
  (unless columns
    (setf columns (mapcar #'(lambda (x)
			      (car (variable-attribute x)))
			  (dataset-variables data))))
  (reverse
   (list-selection :attrs columns
		   :from (dataset-read-data data)
		   :where (process-where-clause rows))))

;;; ***************************************************************************
;;; array & matrix translation functions
;;; ***************************************************************************


;;; ***************************************************************************
;;; other translation functions
;;; ***************************************************************************

(defun transpose (data)
  "Takes a list of lists, `data', and makes a list of tuples t where t[i]
contains all the i'th elements of the lists in `data'."
  (if data
      #+EXPLORER
      (let ((retval (make-list (length (car data)) :initial-element '())))
	(dolist (datum (reverse data))
	  (dotimes (n (length datum))
	    (push (nth n datum) (nth n retval))))
	retval)
      #-EXPLORER
      (apply #'mapcar #'list data)
      nil))

;;; ***************************************************************************
;;; EOF
