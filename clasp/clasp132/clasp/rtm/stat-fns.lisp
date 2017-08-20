;;; Version 2.2, July 1992.
;;; ***************************************************************************
;;;
;;; Written by: Paul Silvey
;;;             Department of Computer and Information Science
;;;             University of Massachusetts
;;;             Amherst, MA 01003
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
;;; -- * --

(cl:defpackage "RTM"
  (:use "COMMON-LISP"))

(in-package "RTM")

(export '(moments))

;;; -- * --

;;
;; Statistical Moments
;;

(defun moments (table-name attr-name)
"1st moment of the distribution is the a measure of central tendancy (mean),
 2nd moment about the mean is a measure of spread (variance),
 3rd moment (normalized) about the mean is a measure of distribution skew,
 4th moment (normalized) about the mean is a measure of distribution peakedness (kurtosis)."
  (let* ((sample (collect-sample :from table-name))
         (n (array-total-size (rtm-sample-tuple-array sample)))
         (n-1 (- n 1))
         (mean 0.0) (var 0.0) (std 0.0) (skew 0.0) (kurt 0.0))
    ;; Compute the mean of the distribution:
    (let ((sum 0.0))
      (do-sample (sample)
        (incf sum (attr-value attr-name)))
      (setf mean (/ sum n)))
    ;; Compute the 2nd, 3rd, and 4th moments about the mean:
    (let ((sos 0.0) (soc 0.0) (soq 0.0))
      (do-sample (sample)
        (let* ((diff (- mean (attr-value attr-name)))
               (diff-sq (* diff diff)))
          (incf sos diff-sq)
          (incf soc (* diff diff-sq))
          (incf soq (* diff-sq diff-sq))))
      (setf var (/ sos n-1)
            std (sqrt var)
            skew (/ (* soc std std std) n-1)
            kurt (/ (* soq var var) n-1)))
    (values mean var std skew kurt)))

