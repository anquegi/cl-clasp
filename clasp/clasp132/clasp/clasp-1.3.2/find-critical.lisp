;;;; -*- Mode:Common-Lisp; Package:clasp; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/clasp/development/find-critical.lisp *-*
;;;; *-* Last-edit: Friday, July 9, 1993  12:15:49; Edited-By: carlson *-* 
;;;; *-* Machine: Miles (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                   Find Critical Values of Statistics                   *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Scott D. Anderson
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
;;;  10-12-92 File Created.  (Anderson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clasp)

;;; --*--
;;; ***************************************************************************

(defun find-critical-value
       (p-function p-value &optional (x-tolerance .00001) (y-tolerance .00001))
  "Returns the critical value of some statistic.  The function `p-function'
should be a unary function mapping statistics---x values---to their
significance---p values.  The function will find the value of x such that the
p-value is `p-value.' The function works by binary search.  A secant method
might be better, but this seems to be acceptably fast.  Only positive values of
x are considered, and `p-function' should be monotonically decreasing from its
value at x=0.  The binary search ends when either the function value is within
`y-tolerance' of `p-value' or the size of the search region shrinks to less than
`x-tolerance.'"
  ;; The Allegro compiler barfs on this...
  ;; It generates this erroneous code...
  ;; (AND (TYPEP #:G3682 'REAL) (> (THE REAL #:G3682) (0)) ...)
  #-(or allegro lucid) (check-type p-value (real (0) (1)))
  (let* ((x-low 0.0)
	 (fx-low 1.0)
	 (x-high 1.0)
	 (fx-high (funcall p-function x-high)))
    ;; double up
    (do () (nil)
      ;; for general functions, we'd have to try the other way of bracketing,
      ;; and probably have another way to terminate if, say, y is not in the
      ;; range of f.
      (when (>= fx-low p-value fx-high)
	(return))
      (setf x-low x-high
	    fx-low fx-high
	    x-high (* 2.0 x-high)
	    fx-high (funcall p-function x-high)))
    ;; binary search
    (do () (nil)
      (let* ((x-mid  (/ (+ x-low x-high) 2.0))
	     (fx-mid (funcall p-function x-mid))
	     (y-diff (abs (- fx-mid p-value)))
	     (x-diff (- x-high x-low)))
	(when (or (< x-diff x-tolerance)
		  (< y-diff y-tolerance))
	  (return-from find-critical-value x-mid))
	;; Because significance is monotonically decreasing with x, if the
	;; function is above the desired p-value...
	(if (< p-value fx-mid)
	    ;; then the critical x is in the upper half
	    (setf x-low x-mid
		  fx-low fx-mid)
	    ;; otherwise, it's in the lower half
	    (setf x-high x-mid
		  fx-high fx-mid))))))

#+test
(defun test-find-critical-value ()
  (dolist (alpha '(.1 .05 .01))
    (format t "x to give alpha = ~4,2f is:  ~5,3f~%"
	    alpha
	    (find-critical-value #'(lambda (x) (gaussian-significance x :both))
				 alpha)))
  (dolist (dof '(2 5 10))
    (dolist (alpha '(.1 .05 .01))
      (format t "with ~2d dof, the t to give upper-tail alpha = ~4,2f is:  ~5,3f~%"
	      dof
	      alpha
	      (find-critical-value #'(lambda (x) (students-t-significance x dof :positive))
				   alpha)))))

;;; ***************************************************************************
;;; EOF
