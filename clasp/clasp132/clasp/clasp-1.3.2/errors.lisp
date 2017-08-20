;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/development/errors.lisp *-*
;;;; *-* Last-edit: Sunday, March 14, 1993  17:52:25; Edited-By: Anderson *-* 
;;;; *-* Machine: Dizzy (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                        Error Handling Functions                        *
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
;;;  02-12-93 File Created.  (carlson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clasp)

;;; --*--
;;; ***************************************************************************
;;; CLASP ERROR CONDITIONS

(define-condition clasp-error (error) ()
  (:report "An unspecified CLASP error has occurred.")
  (:documentation
  "All errors that are defined in CLASP inherit from this error.
CLASP may signal other errors, such as `type-error' or `arithmetic-error.'"))

(define-condition clasp-warning (#-lucid warning #+lucid error) ()
  (:report "An unspecified CLASP warning has occurred.")
  (:documentation
  "All warnings that are defined in CLASP inherit from this warning.
CLASP may signal other warnings."))

;;; Statistical function errors

(define-condition missing-data (clasp-error) ()
  (:report "Missing data was encountered")
  (:documentation
   "This error is signaled when a statistical computation cannot proceed because of missing data."))

(define-condition missing-cell (clasp-error) ()
  (:report "A cell was missing in the anova calculation")
  (:documentation
   "This error is signaled when a statistical computation cannot proceed because of missing data."))

(define-condition no-data (clasp-error) ()
  (:report "There is no data.")
  (:documentation
    "This error is signaled when a statistical computation cannot proceed
because there is no data.  For example, the mean of no numbers signals this
error."))

(define-condition insufficient-data (clasp-error) ()
  (:report "There are too few data for this statistic.")
  (:documentation
    "This error is signaled when a statistical computation cannot proceed
because there is not enough data.  For example, the variance of only one number
signals this error."))

(define-condition zero-variance (clasp-error) ()
  (:report "This statistic cannot handle a sample with no variance.")
  (:documentation
    "This error is signaled when a statistical computation cannot proceed
because there is no variance in some of the data.  For example, some statistics,
such as the z or t statistics, are standardized by dividing by the standard
error (the standard deviation of the sampling distribution).  If there is no
variance, this results in dividing by zero.  Similarly, the correlation between
two samples involves division by zero if either sample has no variance."))

(define-condition unmatched-sequences (clasp-error) ()
  (:report "This statistic requires sequences with the same number of elements.")
  (:documentation
    "This error is signaled by statistical computations that involve matching
elements, such as computing pairwise differences in the `t-test-matched.' A
possible proceed type for this would be to drop additional elements, but the
real problem is likely to be at a higher, conceptual level."))

(define-condition not-binary-variables (clasp-error) ()
  (:report "This statistic requires categorial variables with just two unique
values, that is, binary variables.")
  (:documentation
    "This error is signaled by contingency-table computations that involve
binary variables, and which break down if there are more than two unique values
per variable.  There are usually more-general functions that can be substituted.
For example, `chi-square-2x2' is a special case of `chi-square-rxc' and the
former signals this error."))

(define-condition unequal-cell-sizes (clasp-error) ()
  (:report "This statistic requires the same number of dependent variable values
for each cell.")
  (:documentation
    "This error is signaled by functions such as the two-way anova, which for
mathematical reasons requires that each combination of the independent factors
has the same number of values for the dependent variable."))

;;; dataset function errors

(define-condition variable-not-found (clasp-error) ()
  (:report "This is not a valid variable")
  (:documentation
    "This error is signaled when an attempt is made to use a variable which
does not exist.  This can happen if the variable name is wrong or if the name
of a variable is used in a function which wants the actual variable."))

(define-condition dataset-not-found (clasp-error) ()
  (:report "This is not a valid dataset")
  (:documentation
    "This error is signaled when an attempt is made to use a dataset which
does not exist.  This can happen if the dataset name is wrong or if the name
of a dataset is used in a function which wants the actual dataset."))

(define-condition rtm-table-creation-error (clasp-error) ()
  (:report "Unable to create RTM table to store data for dataset")
  (:documentation
   "This error is signaled when an attempt to create an RTM table to store
the data in a dataset fails."))

(define-condition invalid-data-type (clasp-error) ()
  (:report "An attempt was made to create a variable with an invalid data
type.")
  (:documentation
    "The valid data types for CLASP variables are number, string and symbol.
This error is signaled when there is an attempt to produce variables of some
other type."))

(define-condition row-length-mismatch (clasp-warning) ()
  (:report "An attempt was made to add a row to a dataset with a different 
number of values as rows in the dataset.")
  (:documentation
    "This warning is signaled when an attempt is made to add a row of data to a 
dataset, but the length of the row is not equal to the number of variables in 
the dataset.  Variables whose values are not specified will be set to the 
default value for their type."))

(define-condition column-length-mismatch (clasp-warning) ()
  (:report "An attempt was made to add a column to a dataset whose length was 
less than the number of rows in the dataset.")
  (:documentation
    "This warning is signaled when an attempt is made to add a new variable to
a dataset, but the length of the column is less than the number of rows in the 
dataset.  The variable will be padded at the end with the default value for its 
type to insure that it is of equal length with the rest of the dataset."))

(define-condition variables-from-different-datasets (clasp-error) ()
		  (:report "This operation requires that the variables come from the same dataset")
		  (:documentation
		   "This error is signaled when an operation which requires variables from the same dataset is attempted with variables from different datasets."))

;;; io errors

(define-condition clasp-file-error (clasp-error)
  ;;Lucid is still doing this with defstruct so we need to make it happy.
  ((filename #-lucid :initarg #-lucid :filename))
  (:report 
   (lambda (condition stream)
     #-LUCID	
     (with-slots (filename) condition
       (format stream "Clasp file error; ~A" filename))
     #+LUCID	
     (format stream "Clasp file error; ~A"
	     (clasp-file-error-filename condition))
   ))
  (:documentation
   "This error is signaled when we want to."))
  
(define-condition file-does-not-exist (clasp-file-error)
  ()
  (:report 
   (lambda (condition stream)
     #-LUCID	
     (with-slots (filename) condition
       (format stream "File ~A does not exist." filename))
     #+LUCID	
     (format stream "File ~A does not exist."
             (file-does-not-exist-filename condition))
   ))
  (:documentation
   "This error is signaled when an attempt is made to open a file
which does not exist.  This can happen in the io functions `load-dataset' and 
`import-dataset'."))

(define-condition file-already-exists (clasp-file-error) ()
  (:report 
   (lambda (condition stream)
     #-LUCID	
     (with-slots (filename) condition
       (format stream "File ~A already exists." filename))
     #+LUCID	
     (format stream "File ~A already exists."
             (file-already-exists-filename condition))
   ))
  (:documentation
   "This error is signaled when an attempt is made to open a file  for writing
which already exists.  This can happen in the io functions `save-dataset' and 
`export-dataset'"))

(define-condition read-from-empty-file (clasp-file-error) ()
  (:report 
   (lambda (condition stream)
     #-LUCID	
     (with-slots (filename) condition
       (format stream "An attempt was made to read from ~A, which is empty."
	       filename))
     #+LUCID	
     (format stream "An attempt was made to read from ~A, which is empty."
             (read-from-empty-file-filename condition))
   ))
  (:documentation
   "This error is signaled when an attempt is made to read a dataset from an
empty file.  The error is only checked for in import dataset"))

;;; interface errors

(define-condition invalid-coloring-variable (clasp-error) ()
  (:report "Cannot make colored graph with selected data.")
  (:documentation
   "This error is signaled when an attempt is made to create a colored graph
using invalid data.  Colored graphs must be made from clasp variables, and all the variables, both for the data and the coloring variable, must come from the same dataset."))

;;; ***************************************************************************
;;; EOF
