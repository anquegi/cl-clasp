;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/v3dev/exports.lisp *-*
;;;; *-* Last-edit: Tuesday, March 9, 1993  16:31:24; Edited-By: carlson *-* 
;;;; *-* Machine: Lester (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                       Data Manipulation Functions                      *
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
;;;  02-25-93 File Created.  (carlson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clasp)

;;; --*--
;;; ***************************************************************************

(export
 '(
   ;; classes and definitions - defs.lisp
   data variable *dataset-class* *variable-class*
   name description id source dataset-variables variable-value
   
   ;; dataset and variable fns - dataset-fns.lisp
   make-dataset make-dataset-from-rows make-dataset-from-columns
   rename-dataset delete-dataset activate-dataset deactivate-dataset
   get-dataset
   make-variable add-variable-to-dataset add-variables-to-dataset
   rename-variable delete-variable get-variable
   add-row-to-dataset add-rows-to-dataset
   
   ;; statistical functions - statistical-fns.lisp
   data-length mean sum-of-squares variance standard-deviation minimum maximum
   range quantile median trimmed-mean mode multiple-modes interquartile-range
   statistical-summary
   t-test-one-sample t-test d-test t-test-matched
   chi-square-2x2 chi-square-2x2-counts
   chi-square-rxc chi-square-rxc-counts
   covariance correlation correlation-from-summaries
   cross-correlation autocorrelation lagged-correlation
   confidence-interval-z confidence-interval-t confidence-interval-proportion
   confidence-interval-z-summaries confidence-interval-t-summaries
   anova-one-way-variables anova-one-way-groups print-anova-table
   anova-two-way-variables anova-two-way-groups
   anova-two-way-variables-unequal-cell-sizes
   scheffe-test print-scheffe-table
   linear-regression-minimal linear-regression-minimal-summaries
   linear-regression-brief linear-regression-brief-summaries
   linear-regression-verbose linear-regression-verbose-summaries
   smooth-median-2 smooth-median-3 smooth-median-4
   smooth-median-5 smooth-hanning smooth-4253H
   
   ;; statistical functions - density-fns.lisp
   factorial factorial-ln binomial-coefficient binomial-probability
   error-function gaussian-cdf error-function-complement gaussian-significance
   poisson-cdf chi-square-significance students-t-significance f-significance
   binomial-cdf
   
   ;; statistical functions - find-critical.lisp
   find-critical-value
   
   ;; statistical functions - sampling-fns.lisp
   distribution binomial-distribution normal-distribution gamma-distribution
   poisson-distribution uniform-distribution
   sample-to-list sample-to-dataset
   
   ;; io functions - io-fns.lisp
   load-dataset save-dataset import-dataset export-dataset
   
   ;; data manipulation functions - manipulation-fns.lisp
   create-new-column-from-function recode recode-list
   partition-dataset merge-datasets .on. row-number
   
   ;; data manipulation functions - translation-fns.lisp
   dataset-to-rows dataset-to-columns transpose

   ;; conditions - errors.lisp
   invalid-coloring-variable
   )
 )

;;; ***************************************************************************
;;; EOF
