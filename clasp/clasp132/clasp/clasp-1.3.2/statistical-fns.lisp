;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clasp/clasp/development/statistical-fns.lisp *-*
;;;; *-* Last-edit: Sunday, September 19, 1993  17:24:33; Edited-By: Anderson *-* 
;;;; *-* Machine: Chuck (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                          Statistics Functions                          *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Adam Carlson and Scott D. Anderson
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
;;;  06-26-92 File Created.  (Carlson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clasp)

;;; --*--
;;; ***************************************************************************
;;; CLASP ERROR CONDITIONS

#+test
(defmacro error-handling (form)
  "This test form just returns the name of the error that was signalled, just to
show that they are properly signalled and captured."
  `(cl:handler-case ,form
     (type-error (condition)
		 (declare (ignore condition))
		 ;; (format t "Condition ~s is a subtype of `type-error.'~%" (type-of condition))
		 :type-error)
     (simple-error () :simple-error)
     (eh:type-error () 'eh:type-error)
     (sys:failed-assertion () 'sys:failed-assertion)
     (no-data () :no-data)
     (insufficient-data () :insufficient-data)
     (zero-variance () :zero-variance)
     ;; Actually, nothing should signal this, because they should be caught earlier.
     (clasp-error () :clasp-error)
     (unmatched-samples () :unmatched-samples)
     (t (condition)
	;; ``uncaught'' conditions print and their name
	(format t "CONDITION ~S IS NOT EXPLICITLY CAUGHT!~%" (type-of condition))
	(error condition))))

;;; ============================================================================
;;; TEST-FUNCTION

#+test
(defun test-function (fname)
  "Call `fname' on a number of different arguments, some legal and some illegal.
Trap errors and report the result."
  (format t "~&Testing ~s~2%" fname)
  (let ((*print-array* t))
    (dolist (args `((a)
		    (())
		    ((nil))
		    ((nil nil))
		    ((1))
		    ((1 2 3 4 5))
		    (#(1 2 3 4 5))
		    ((1 2 3 4 5) :start 2)
		    (#(1 2 3 4 5) :end 3)
		    ((1 2 3 4 5) :start 1 :end 4)
		    (#(1 2 3 4 5) :key ,#'1+)
		    ((1 2 3 4 5) :key ,#'-)
		    ("ABCDE" :key ,#'char-int)
		    (((w 2) (e 4) (i 1) (r 5) (d 3)) :key ,#'cadr)))
      ;; Okay
      (let ((result (apply #'(lambda (data &rest other-args)
			       (error-handling
				 (apply (symbol-function fname)
					data
					other-args)))
			   args)))
	(if (null (cdr args))
	    (format t "~&~s~55t~s~%" (car args) result)
	    (format t "~&~s~35t~{~s ~}~55t~s~%"
		    (car args)
		    (mapcar #'(lambda (x)
				(if (functionp x) (ticl:function-name x) x))
			    (cdr args))
		    result))))))

;;; ============================================================================
;;; STATISTICAL FUNCTIONS

(defun data-length (data &key start end key)
  "Returns the number of data values in `data.' Essentially, this is the Common
Lisp `length' function, except it handles sequences where there is a `start' or
`end' parameter.  The `key' parameter is ignored."
  (declare (ignore key))
  (check-type data  sequence)
  (check-type start (or fixnum null))
  (check-type end   (or fixnum null))
  (if (null end)
      (let ((n (length data)))
	(if (null start)
	    n
	    (- n start)))
      (if (null start)
	  end
	  (- end start))))

#+test
(test-function 'data-length)


(defun mean (data &rest standard-args &key start end key)
  "Returns the arithmetic mean of `data,' which should be a sequence.

Signals `no-data' if there is no data."
  (declare (ignore start end key))
  (check-type data sequence)
  (let ((n (apply #'data-length data standard-args)))
    (if (plusp n)
	(/ (apply #'reduce #'+ data standard-args) n)
	(error 'no-data))))

#+test
(test-function 'mean)


(defun sum-of-squares (data &rest standard-args &key start end key)
  "Returns the sum of squared distances from the mean of `data'.

Signals `no-data' if there is no data."
  (declare (ignore start end key))
  (check-type data sequence)
  (let ((m (apply #'mean data standard-args)))
    (let ((key (getf standard-args :key nil)))
      (if (null key)
	  ;; special case for null key to avoid useless funcalls
	  (apply #'reduce #'+ data
		 :key #'(lambda (x) (square (- x m)))
		 standard-args)
	  ;; the general case
	  (apply #'reduce #'+ data
		 :key #'(lambda (x) (square (- (funcall key x) m)))
		 standard-args)))))

#+test
(test-function 'sum-of-squares)

(defun variance (data &rest standard-args &key start end key)
  "Returns the variance of `data,' that is, the `sum-of-squares' divided by
n-1. Signals `no-data' if there is no data.  Signals `insufficient-data' if
there is only one datum."
  (declare (ignore start end key))
  (check-type data sequence)
  (let ((n (apply #'data-length data standard-args)))
    (case n
      (0 (error 'no-data))
      (1 (error 'insufficient-data))
      (t (/ (apply #'sum-of-squares data standard-args) (- n 1))))))

#+test
(test-function 'variance)


(defun standard-deviation (data &rest standard-args &key start end key)
  "Returns the standard deviation of `data,' which is just the square root of
the variance.

Signals `no-data' if there is no data.  Signals `insufficient-data' if there is
only one datum."
  (declare (ignore start end key))
  (sqrt (apply #'variance data standard-args)))

#+test
(test-function 'standard-deviation)


(defun minimum (data &rest standard-args &key start end key)
  "Returns the element of the sequence `data' whose `key' is minimum.  Signals
`no-data' if there is no data.  If there is only one element in the data
sequence, that element will be returned, regardless of whether it is valid (a
number)."
  (declare (ignore start end))
  (check-type data sequence)
  (if (null key)
      (apply #'reduce #'min data standard-args)
      (apply #'reduce #'(lambda (x y)
			  (if (< (funcall key x)
				 (funcall key y))
			      x
			      y))
	     data
	     ;; have to override the key function, so we get the real item
	     :key nil
	     standard-args)))

#+test
(test-function 'minimum)


(defun maximum (data &rest standard-args &key start end key)
  "Returns the element of the sequence `data' whose `key' is maximum.  Signals
`no-data' if there is no data.  If there is only one element in the data
sequence, that element will be returned, regardless of whether it is valid (a
number)."
  (declare (ignore start end))
  (check-type data sequence)
  (if (null key)
      (apply #'reduce #'max data standard-args)
      (apply #'reduce #'(lambda (x y)
			  (if (> (funcall key x)
				 (funcall key y))
			      x
			      y))
	     data
	     ;; have to override the key function, so we get the real item
	     :key nil
	     standard-args)))

#+test
(test-function 'maximum)



(defun range (data &rest standard-args &key start end key)
  "Returns the range of the sequence `data.' Signals `no-data' if there is no
data.  The range is given by max - min."
  (declare (ignore start end))
  (if (null key)
      (- (apply #'maximum data standard-args)
	 (apply #'minimum data standard-args))
      (let ((big-elt   (apply #'maximum data standard-args))
	    (small-elt (apply #'minimum data standard-args)))
	(- (funcall key big-elt)
	   (funcall key small-elt)))))     

#+test
(test-function 'range)

;;; ============================================================================
;;; Should move this to utilities.

(defvar *temporary-vector* nil
  "A temporary vector for use by statistical functions such as `quantile,' which
uses it for sorting data.  This avoids consing or rearranging the user's data.")

(defmacro with-temp-vector ((temp min-size) &body forms)
  "Binds `temp' to a vector of length at least `min-size.' It's a vector of
pointers and has a fill-pointer, initialized to `min-size.'"
  ;; The following will NOT prevent sychronization problems in a parallel lisp,
  ;; but it will make them less likely, because the only timing problem will be
  ;; if a process swap allows another process to grab the same
  ;; *temporary-vector*, that is, before the variable gets set to nil.  The real
  ;; solution is critical sections, which is not in Common Lisp.
  `(let ((,temp *temporary-vector*))
     ;; no process swap between the previous LET and the next SETF, we hope!
     (setf *temporary-vector* nil) 
     (when (or (null ,temp)
	       (< (array-total-size ,temp) ,min-size))
       (setf ,temp (make-array ,min-size :fill-pointer ,min-size)))
     (setf (fill-pointer ,temp) ,min-size)
     (multiple-value-prog1 (progn . ,forms)
			   (setf *temporary-vector* ,temp))))

;;; ============================================================================

(defun quantile (data q &rest standard-args &key start end key)
  "Returns the element which is the q'th percentile of the data when accessed by
`key.' That is, it returns the element such that `q' of the data is smaller than
it and 1-`q' is above it, where `q' is a number between zero and one, inclusive.
For example, if `q' is .5, this returns the median; if `q' is 0, this returns
the minimum (although the `minimum' function is more efficient).

This function uses the bisection method, doing linear interpolation between
elements i and i+1, where i=floor(q(n-1)).  See the manual for more information.
The function returns three values: the interpolated quantile and the two
elements that determine the interval it was interpolated in.  If the quantile
was exact, the second two values are the same element of the data."
  (check-type data sequence)
  #-(or allegro lucid)(check-type q (real 0 1))
  (let ((n (apply #'data-length data standard-args)))
    (when (zerop n) (error 'no-data))
    (with-temp-vector (temp n)
      (replace temp data :end1 n :start2 (or start 0) :end2 (or end n))
      ;; This sorting could be replaced by an O(N) algorithm, but we'll defer that.
      (if (null key)
	  (sort temp #'<)
	  (sort temp #'< :key key))
      (multiple-value-bind (index lambda) (floor (* q (- n 1)))
	(if (zerop lambda)
	    (let ((elt (aref temp index)))
	      (values (if key (funcall key elt) elt) elt elt))
	    (let ((elt1 (aref temp index))
		  (elt2 (aref temp (1+ index))))
	      (values (+ (* (if key (funcall key elt1) elt1) (- 1 lambda))
			 (* (if key (funcall key elt2) elt2) lambda))
		      elt1
		      elt2)))))))

#+test
(defun error-test-quartile ()
  (let ((*print-array* t))
    ;; data constraints
    (spy (error-handling (quantile 'a 0)))
    (spy (error-handling (quantile 'nil 0)))
    (spy (error-handling (quantile '(a) 0)))
    (spy (error-handling (quantile '(a b c) 0)))
    ;; q constraints
    (spy (error-handling (quantile '(1 2 3 4 5) nil)))
    (spy (error-handling (quantile '(1 2 3 4 5) 'a)))
    (spy (error-handling (quantile '(1 2 3 4 5) -1)))
    (spy (error-handling (quantile '(1 2 3 4 5) 3)))))

#+test
(defun test-quartile ()
  (let ((*print-array* t))
    (spy (quantile '(1 2 3 4 5) 0))
    (spy (quantile '(1 2 3 4 5) 1/4))
    (spy (quantile '(1 2 3 4 5) 1/2))
    (spy (quantile '(1 2 3 4 5) 3/4))
    (spy (quantile '(1 2 3 4 5) 1))
    ;; interpolation
    (spy (quantile '(1 2 3 4 5) 1/3))
    (spy (quantile '(1 2 3 4 5) 2/3))
    ;; keyword processing
    (spy (quantile '(1 2 3 4 5) 1/2 :start 2))
    (spy (quantile '(1 2 3 4 5) 1/2 :end 4))
    (spy (quantile "ABCDE" 1/2 :key #'char-int))
    (spy (quantile '#((a . 1) (o . 2) (r . 3) (l . 4) (v . 5))
		   2/3
		   :key #'cdr))))

#+test
(defun plot-quantile ()
  "This is the example from the manual."
  (loop for q from 0.0 to 1.0 by (/ 128.0)
	collect (list (quantile '(2.0 3.0 5.0 8.0 13.0) q) q) into pts
	finally (plotter:plot-stuff
		  `(((:polyline-without-vertices) . ,pts)))))


(defun median (data &rest standard-args &key start end key)
  "Returns the median of the subsequence of `data' from `start' to `end', using
`key'.  The median is just the 0.5 quantile, and so this function returns the
same values as the `quantile' function."
  (declare (ignore start end key))
  (apply #'quantile data .5 standard-args))

#+test
(test-function 'median)

;;; ============================================================================

(defun trimmed-mean (data percentage &rest standard-args &key start end key)
  "Returns a trimmed mean of `data.' A trimmed mean is an ordinary, arithmetic
mean of the data, except that an outlying percentage has been discarded.  For
example, suppose there are ten elements in `data,' and `percentage' is 0.1: the
result would be the mean of the middle eight elements, having discarded the
biggest and smallest elements.  If `percentage' doesn't result in a whole number
of elements being discarded, then a fraction of the remaining biggest and
smallest is discarded.  For example, suppose `data' is '(1 2 3 4 5) and
`percentage' is 0.25: the result is (.75(2) + 3 + .75(4))/(.75+1+.75) or 3.  By
convention, the 0.5 trimmed mean is the median, which is always returned as a
number."
  (check-type data sequence)
  #-(or allegro lucid)(check-type percentage (real 0 1/2))
  (let ((n (apply #'data-length data standard-args)))
    (when (zerop n) (error 'no-data))
    (with-temp-vector (temp n)
      (replace temp data :end1 n :start2 (or start 0) :end2 (or end n))
      ;; This sorting could be replaced by calls to an O(N) algorithm, but we'll
      ;; defer that.
      (if (null key)
	  (sort temp #'<)
	  (sort temp #'< :key key))
      ;; Okay, here we go.  By convention, if there is nothing left after the
      ;; trimming, we return the median.  There is nothing left if (1) n is even
      ;; and there is less than 2 items left or (2) n is odd and there is less
      ;; than one item left.
      (if (if (evenp n)
	      (<= (- n (* 2 n percentage)) 2.0)
	      (<= (- n (* 2 n percentage)) 1.0))
	  ;; Not enough left, so take median
	  (if (evenp n)
	      (if (null key)
		  (/ (+ (elt temp (floor n 2))
			(elt temp (- (floor n 2) 1)))
		     2)
		  (/ (+ (funcall key (elt temp (floor n 2)))
			(funcall key (elt temp (- (floor n 2) 1))))
		     2))
	      (if (null key)
		  (elt temp (floor n 2))
		  (funcall key (elt temp (floor n 2)))))
	  ;; There is enough left, so add up the middle (whole) elements, plus a
	  ;; percentage of the end elements.  We compute q, which is the number
	  ;; of whole elements to trim off each end, and r, which is the
	  ;; fractional amount to trim off the next element.
	  (multiple-value-bind (q r) (truncate (* n percentage))
	    (setf r (- 1 r))
	    (let ((sum (+ (* r (if key
				   (funcall key (elt temp q))
				   (elt temp q)))
			  (* r (if key
				   (funcall key (elt temp (- n q 1)))
				   (elt temp (- n q 1))))
			  (if key
			      (reduce #'+ temp
					 :start (1+ q) :end (- n q 1) :key key)
			      (reduce #'+ temp
					 :start (1+ q) :end (- n q 1))))))
	      (/ sum (* n (- 1 (* 2 percentage))))))))))

#+test
(defun trimmed-mean-10 (data &rest standard-args)
  (apply #'trimmed-mean data .1 standard-args))

#+test
(test-function 'trimmed-mean-10)

#+test
(defun test-trimmed-mean ()
  (dotimes (i 21)
    (spy (trimmed-mean '(1 2 3 4) (/ i 40))))
  (dotimes (i 21)
    (spy (trimmed-mean '(1 2 3 4 5) (/ i 40))))
  (dotimes (i 21)
    (spy (trimmed-mean '(1.0 2.0 4.0 8.0) (/ i 40)))))

;;; ============================================================================

(defmacro start/end (call-form start-n end-n)
  `(if (null ,start-n)
       (if (null ,end-n)
	   ,call-form
	   (,@call-form :end ,end-n))
       (if (null ,end-n)
	   (,@call-form :start ,start-n)
	   (,@call-form :start ,start-n :end ,end-n))))

(defun mode (data &rest standard-args &key start end key)
  "Returns the most frequent element of `data,' which should be a sequence.  The
algorithm involves sorting, and so the data must be numbers or the `key'
function must produce numbers.  Consider `sxhash' if no better function is
available.  Also returns the number of occurrences of the mode.  If there is
more than one mode, this returns the first mode, as determined by the sorting of
the numbers."
  (declare (values element number-of-occurrences))
  (check-type data sequence)
  (let ((n (apply #'data-length data standard-args)))
    (when (zerop n) (error 'no-data))
    (with-temp-vector (temp n)
      (replace temp data :end1 n :start2 (or start 0) :end2 (or end n))
      (if (null key)
	  (sort temp #'<)
	  (sort temp #'< :key key))
      ;; Okay, the sorting has already grouped the data, so all we have to do is
      ;; keep a running record of the biggest group seen so far.
      (let* ((current-group      (aref temp 0))
	     (current-group-key  (when key (funcall key current-group)))
	     (current-group-size 0)
	     (biggest-group      nil)
	     (biggest-group-size 0))
	(if (null key)
	    (dotimes (i n)
	      (let ((elt (aref temp i)))
		(if (= elt current-group)
		    (incf current-group-size)
		    (progn (when (> current-group-size biggest-group-size)
			     (setf biggest-group      current-group
				   biggest-group-size current-group-size))
			   (setf current-group elt
				 current-group-size 1)))))
	    (dotimes (i n)
	      (let* ((elt     (aref temp i))
		     (elt-key (funcall key elt)))
		(if (= elt-key current-group-key)
		    (incf current-group-size)
		    (progn (when (> current-group-size biggest-group-size)
			     (setf biggest-group current-group
				   biggest-group-size current-group-size))
			   (setf current-group elt
				 current-group-key elt-key
				 current-group-size 1))))))
	;; This code is just in case the last group is the biggest group.  It
	;; won't be processed above, because groups are only considered for
	;; biggest when the next group starts, which works fine for every group
	;; except the last.
	(when (> current-group-size biggest-group-size)
	  (setf biggest-group      current-group
		biggest-group-size current-group-size))
	(values biggest-group biggest-group-size)))))

#+test
(test-function 'mode)

#+test
(defun test-mode ()
  (spy (mode '(1 1 1 1 1)))
  (spy (mode '(1 1 1 2 2 1 1)))
  (spy (mode '(1 1 1 2 2 4 4 4 4)))
  (spy (mode '(1 1 1 2 2 2 2 4 4)))
  (spy (mode '(foo bar bar baz baz baz quux quux quux quux) :key #'sxhash)))

;;; ============================================================================

(defun multiple-modes (data k &rest standard-args &key start end key)
  "Returns the `k' most frequent elements of `data,' which should be a sequence.
The algorithm involves sorting, and so the data must be numbers or the `key'
function must produce numbers.  Consider #'sxhash if no better function is
available.  Also returns the number of occurrences of each mode.  The value is
an association list of modes and their counts.  This function is a little more
computationally expensive than `mode,' so only use it if you really need
multiple modes."
  (check-type data sequence)
  (let ((n (apply #'data-length data standard-args)))
    (when (zerop n) (error 'no-data))
    (with-temp-vector (temp n)
      (replace temp data :end1 n :start2 (or start 0) :end2 (or end n))
      (if (null key)
	  (sort temp #'<)
	  (sort temp #'< :key key))
      ;; Okay, the sorting has already grouped the data, so all we have to do is
      ;; keep a running record of the biggest groups seen so far.
      (let* ((current-group      (aref temp 0))
	     (current-group-key  (when key (funcall key current-group)))
	     (current-group-size 0)
	     (modes              nil))
	;; with the exception of the update function, this code is identical to
	;; that of `mode.'
	(flet ((update ()
		 (if (< (length modes) k)
		     (push (cons current-group current-group-size)
			   modes)
		     (let ((min-mode-size (reduce #'min modes :key #'cdr)))
		       (when (< min-mode-size current-group-size)
			 (let ((min-mode (rassoc min-mode-size modes :test #'=)))
			   (setf (car min-mode) current-group
				 (cdr min-mode) current-group-size)))))))
	  (if (null key)
	      (dotimes (i n)
		(let ((elt (aref temp i)))
		  (if (= elt current-group)
		      (incf current-group-size)
		      (progn (update)
			     (setf current-group elt
				   current-group-size 1)))))
	      (dotimes (i n)
		(let* ((elt     (aref temp i))
		       (elt-key (funcall key elt)))
		  (if (= elt-key current-group-key)
		      (incf current-group-size)
		      (progn (update)
			     (setf current-group elt
				   current-group-key elt-key
				   current-group-size 1))))))
	  ;; This code is just in case the last group is the biggest group.  It
	  ;; won't be processed above, because groups are only considered for
	  ;; biggest when the next group starts, which works fine for every group
	  ;; except the last.
	  (update)
	  modes)))))

#+test
(defun test-multiple-modes ()
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 1))
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 2))
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 3))
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 4))
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 5)))

;;; ============================================================================

(defun interquartile-range (data &rest standard-args)
  "The interquartile range is similar to the variance of a sample because both
are statistics that measure out ``spread out'' a sample is.  The interquartile
range is the difference between the 3/4 quantile (the upper quartile) and the
1/4 quantile (the lower quartile)."
  (- (apply #'quantile data 3/4 standard-args)
     (apply #'quantile data 1/4 standard-args)))

#+test
(test-function 'interquartile-range)

;;; ============================================================================

(defun tukey-summary (data &rest standard-args &key start end key)
  "Computes a Tukey five-number summary of the data.  That is, it returns, in
increasing order, the extremes and the quartiles: the minimum, the 1/4 quartile,
the median, the 3/4 quartile, and the maximum."
  (declare (values minimum first-quartile median third-quartile maximum))
  (check-type data sequence)
  (let ((n (apply #'data-length data standard-args)))
    (when (zerop n) (error 'no-data))
    (with-temp-vector (temp n)
      (replace temp data :end1 n :start2 (or start 0) :end2 (or end n))
      (if (null key)
	  (sort temp #'<)
	  (sort temp #'< :key key))
      (flet ((get-quartile (i)
	       (let* ((index (/ (* i (- n 1)) 4))
		      (elt1 (aref temp (floor index)))
		      (elt2 (aref temp (ceiling index))))
		 (if (null key)
		     (/ (+ elt1 elt2) 2)
		     (/ (+ (funcall key elt1) (funcall key elt2)) 2)))))
	(values (aref temp 0)
		(get-quartile 1)
		(get-quartile 2)
		(get-quartile 3)
		(aref temp (- n 1)))))))

#+test
(test-function 'tukey-summary)

#+test
(defun test-tukey-summary ()
  (spy (tukey-summary '(1 2 3 4 5 6 7 8 9)))
  (spy (tukey-summary '(1 2 3 4 5 6 7 8 9 10))))

;;; ============================================================================

(defun statistical-summary (data &rest standard-args &key start end key)
  "Compute the length, minimum, maximum, range, median, mode, mean, variance,
standard deviation, and interquartile-range of `sequence' from `start' to `end',
accessed by `key'."
  (declare (ignore start end)
	   (values length minimum maximum range median mode mean
		   variance standard-deviation interquartile-range))
  (let* ((length   (apply #'data-length data standard-args))
	 (minimum  (apply #'minimum data standard-args))
	 (maximum  (apply #'maximum data standard-args))
	 (range    (if (null key)
		       (- maximum minimum)
		       (- (funcall key maximum)
			  (funcall key minimum))))
	 (median   (apply #'median data standard-args))
	 (mode     (apply #'mode data standard-args))
	 (mean     (apply #'mean data standard-args))
	 (variance (apply #'variance data standard-args))
	 (sd       (sqrt variance))
	 (iqr      (apply #'interquartile-range data standard-args)))
    (values length minimum maximum range median mode mean variance sd iqr)))

#+test
(test-function 'statistical-summary)

;;; ***************************************************************************
;;; Dave Fisher's statistics functions
;;; Rewritten by Adam Carlson and Scott D. Anderson
;;; ***************************************************************************

(defun t-test-one-sample
       (data tails &optional (h0-mean 0) &rest standard-args &key start end key)
  "Returns the t-statistic for the mean of the data, which should be a sequence
of numbers.  Let D be the sample mean.  The null hypothesis is that D equals the
`H0-mean.' The alternative hypothesis is specified by `tails': `:both' means D
/= H0-mean, `:positive' means D > H0-mean, and `:negative' means D < H0-mean.

The function also returns the significance, the standard error, and the degrees
of freedom.  Signals `zero-variance' if that condition occurs.  Signals
`insufficient-data' unless there are at least two elements in the sample."
  (declare (ignore start end key)
	   (values t-statistic significance sample-error dof))
  (let ((n (apply #'data-length data standard-args)))
    (when (zerop n)
      (error 'no-data))
    (let ((d (apply #'mean data standard-args))
	  (v (apply #'variance data standard-args)))
      (when (zerop v)
	(error 'zero-variance))
      (let* ((se  (sqrt (/ v n)))
	     (tt  (/ (- d h0-mean) se))
	     (sig (students-t-significance tt (- n 1) tails)))
	(values tt sig se (- n 1))))))

#+test
(defun test-t-test-one-sample ()
  "The following data is drawn from example 7.8, page 262, from Probability and
Statistics for Engineering and the Sciences, by Jay L.  DeVore, published by
Brooks/Cole Publishing Company, copyright 1982.  The function above computes the
correct t and does not reject H0."
  (spy (t-test-one-sample '(27.2 29.3 31.5 28.7 30.2 29.6) :negative 30))
  (spy (error-handling (t-test-one-sample 'a :both)))
  (spy (error-handling (t-test-one-sample '() :both)))
  (spy (error-handling (t-test-one-sample '(nil) :both)))
  (spy (error-handling (t-test-one-sample '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test-one-sample '(1 2 3 4 5) :both))))

;;; ============================================================================

(defun t-test (sample-1 sample-2 tails &optional (h0mean 0))
  "Returns the t-statistic for the difference in the means of two samples, which
should each be a sequence of numbers.  Let D=mean1-mean2.  The null hypothesis
is that D=0.  The alternative hypothesis is specified by `tails': `:both' means
D/=0, `:positive' means D>0, and `:negative' means D<0.  Unless you're using
:both tails, be careful what order the two samples are in: it matters!

The function also returns the significance, the standard error, and the degrees
of freedom.  Signals `standard-error-is-zero' if that condition occurs.  Signals
`insufficient-data' unless there are at least two elements in each sample."
  (declare (values t-statistic significance std-error dof))
  (check-type tails (member :both :positive :negative))
  (let ((n1 (data-length sample-1))
	(n2 (data-length sample-2)))
    (when (or (zerop n1) (zerop n2))
      (error 'no-data))
    (when (or (< n1 2) (< n2 2))
      (error 'insufficient-data))
    (let* ((mean1 (mean sample-1))
	   (mean2 (mean sample-2))
	   (ss1   (sum-of-squares sample-1))
	   (ss2   (sum-of-squares sample-2))
	   (dof   (+ n1 n2 -2))			; degrees of freedom
	   (sp    (/ (+ ss1 ss2) dof)))		; pooled sample variance
      (when (zerop sp)
	(error 'zero-variance))
      (let* ((std-error    (sqrt (* sp (+ (/ n1) (/ n2)))))
	     (t-statistic  (/ (- (- mean1 mean2) h0mean) std-error))
	     (significance (students-t-significance t-statistic dof tails)))
	(values t-statistic significance std-error dof)))))

#+test
(defun test-t-test ()
  ;; These first examples check that the basic processing is okay.
  (spy (error-handling (t-test '() '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test '(nil) '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test '(nil nil) '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test '(1) '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test '(1 nil 1 1 1 1) '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test '(1 1) '(2 2) :both)))
  ;; The first two should be highly significant, the last highly insignificant
  (spy (t-test '(11 12 13) '(1 2 3) :both))
  (spy (t-test '(11 12 13) '(1 2 3) :positive))
  (spy (t-test '(11 12 13) '(1 2 3) :negative))
  (spy (t-test '(11 12 13) '(1 2 3) :both  9))
  (spy (t-test '(11 12 13) '(1 2 3) :both 10))
  (spy (t-test '(11 12 13) '(1 2 3) :both 11))
  (spy (t-test '(11 12 13) '(1 2 3) :positive  9))
  (spy (t-test '(11 12 13) '(1 2 3) :positive 10))
  (spy (t-test '(11 12 13) '(1 2 3) :positive 11))
  (spy (t-test '(11 12 13) '(1 2 3) :negative  9))
  (spy (t-test '(11 12 13) '(1 2 3) :negative 10))
  (spy (t-test '(11 12 13) '(1 2 3) :negative 11))
  )

;;; ============================================================================

(defun d-test (sample-1 sample-2 tails &key (times 1000) (h0mean 0))
  "Two-sample test for difference in means.  Competes with the unmatched,
two-sample t-test.  Each sample should be a sequence of numbers.  We calculate
the mean of `sample-1' minus the mean of `sample-2'; call that D.  Under the null
hypothesis, D is zero.  There are three possible alternative hypotheses: D is
positive, D is negative, and D is either, and they are selected by the `tails'
parameter, which must be :positive, :negative, or :both, respectively.  We count
the number of chance occurrences of D in the desired rejection region, and
return the estimated probability."
  (declare (values D count times))
  (check-type sample-1 sequence)
  (check-type sample-2 sequence)
  (check-type tails (member :both :positive :negative))
  (let ((n1 (data-length sample-1))
	(n2 (data-length sample-2)))
    (when (or (zerop n1) (zerop n2))
      (error 'no-data))
    (when (or (< n1 2) (< n2 2))
      (error 'insufficient-data))
    (let* ((dt  (- (/ (reduce #'+ sample-1) n1)
		   (/ (reduce #'+ sample-2) n2)))
	   (n   (+ n1 n2)))
      (if (zerop h0mean)
	  (with-temp-vector (sample n)
	    (setf (fill-pointer sample) n)
	    (dotimes (i n1) (setf (aref sample i) (elt sample-1 i)))
	    (dotimes (i n2) (setf (aref sample (+ i n1)) (elt sample-2 i)))
	    (let ((count 0))
	      (dotimes (b times)
		(let ((d (- (loop for i from 1 to n1
				  summing (aref sample (random n)) into s1
				  finally (return (/ s1 n1)))
			    (loop for i from 1 to n2
				  summing (aref sample (random n)) into s2
				  finally (return (/ s2 n2))))))
		  (case tails
		    (:both (if (<= (abs dt) (abs d)) (incf count)))
		    (:positive (if (<= dt d) (incf count)))
		    (:negative (if (<= d dt) (incf count))))))
	      (values dt count times)))
	  (with-temp-vector (results times)
	    (setf (fill-pointer results) 0)
	    (dotimes (b times)
	      (let ((d (- (loop for i from 1 to n1
				summing (elt sample-1 (random n1)) into s1
				finally (return (/ s1 n1)))
			  (loop for i from 1 to n2
				summing (elt sample-2 (random n2)) into s2
				finally (return (/ s2 n2))))))
		(vector-push d results)))
	    (let ((boot-mean (clasp::mean results)))
	      (let ((count 0))
		(dotimes (i times)
		  (let ((d (aref results i)))
		    (case tails
		      (:both (if (<= (abs (- h0mean dt)) (abs (- boot-mean d))) (incf count)))
		      (:positive (if (<= (- dt h0mean) (- d boot-mean)) (incf count)))
		      (:negative (if (<= (- d boot-mean) (- dt h0mean)) (incf count))))))
		(values dt count times))))))))

#+test
(defun test-d-test ()
  ;; These first examples check that the basic processing is okay.
  (spy (error-handling (d-test '() '(2 2 2 2 2) :both)))
  (spy (error-handling (d-test '(nil) '(2 2 2 2 2) :both)))
  (spy (error-handling (d-test '(nil nil) '(2 2 2 2 2) :both)))
  (spy (error-handling (d-test '(1) '(2 2 2 2 2) :both)))
  (spy (error-handling (d-test '(1 nil 1 1 1 1) '(2 2 2 2 2) :both)))
  (spy (error-handling (d-test '(1 1) '(2 2) :both))))

#+test
(defun compare-d-test-to-t-test ()
  ;; We don't expect equality of the p-values, but similarity
  (let ((group1 '(4 5 6))
	(group2 '(1 2 3))
	tt t-sig dd d-count)
    (format t "~2&")
    '(dolist (tails '(:both :positive :negative))
      (dolist (h0mean '(0 2 3 4))
	(multiple-value-setq (tt t-sig) (t-test group1 group2 tails h0mean))
	(multiple-value-setq (dd d-count) (d-test group1 group2 tails :h0mean h0mean :times 1000))
	(format t "tails = ~s; h0mean = ~s~%t-test sig = ~5,3f~%d-test sig = ~5,3f (~d/1000)~2%"
		tails h0mean
		t-sig (/ d-count 1000.0) d-count)))
    '(dolist (tails '(:positive))
      (dolist (h0mean '(1 2 3 4 5))
	(multiple-value-setq (tt t-sig) (t-test group1 group2 tails h0mean))
	(multiple-value-setq (dd d-count) (d-test group1 group2 tails :h0mean h0mean :times 1000))
	(format t "tails = ~s; h0mean = ~s~%t-test sig = ~5,3f~%d-test sig = ~5,3f (~d/1000)~2%"
		tails h0mean
		t-sig (/ d-count 1000.0) d-count)))
    (dolist (tails '(:both))
      (dolist (h0mean '(1 2 3 4 5 6 7))
	(multiple-value-setq (tt t-sig) (t-test group1 group2 tails h0mean))
	(multiple-value-setq (dd d-count) (d-test group1 group2 tails :h0mean h0mean :times 1000))
	(format t "tails = ~s; h0mean = ~s~%t-test sig = ~5,3f~%d-test sig = ~5,3f (~d/1000)~2%"
		tails h0mean
		t-sig (/ d-count 1000.0) d-count)))))

#+test
(defun time-d-test ()
  (let ((s1 (loop repeat 20 collect (random 10.0)))
	(s2 (loop repeat 20 collect (+ 2.0 (random 10.0)))))
    (time:timeit (:cpu :cons :number-cons)
      (t-test s1 s2 :both))
    (time:timeit (:cpu :cons :number-cons)
      (d-test s1 s2 :both))))

;;; ============================================================================

(defun t-test-matched (sample1 sample2 tails)
  "Returns the t-statistic for two matched samples, which should be equal-length
sequences of numbers.  Let D=mean1-mean2.  The null hypothesis is that D=0.  The
alternative hypothesis is specified by `tails': `:both' means D/=0, `:positive'
means D>0, and `:negative' means D<0.  Unless you're using :both tails, be
careful what order the two samples are in: it matters!

The function also returns the significance, the standard error, and the degrees
of freedom.  Signals `standard-error-is-zero' if that condition occurs.  Signals
`insufficient-data' unless there are at least two elements in each sample."
  (declare (values t-statistic significance std-error dof))
  (let* ((n1 (data-length sample1))
	 (n2 (data-length sample2)))
    (unless (= n1 n2)
      (error 'unmatched-sequences))
    (when (< n1 2)
      (error 'insufficient-data))
    #-lucid
    (with-temp-vector (temp n1)
      (map-into temp #'- sample1 sample2)
      (t-test-one-sample temp tails))
    ;; The idiots at Lucid didn't implement `map-into.'
    #+lucid
    (t-test-one-sample (map 'list #'- sample1 sample2) tails)))

#+test
(defun test-t-test-matched ()
  "The following data is drawn from example 8.8, page 297, from Probability and
Statistics for Engineering and the Sciences, by Jay L.  DeVore, published by
Brooks/Cole Publishing Company, copyright 1982.  The function above computes the
correct t and does not reject H0."
  (spy (t-test-matched
	 '(30.99 31.47 30.00 30.64 35.25 30.62 31.91 31.37 13.22 21.14 27.21 28.27 29.75 24.90 27.86 31.33)
	 '(30.05 31.75 28.50 31.18 35.12 30.55 31.88 31.05 12.97 21.92 27.26 28.14 30.05 25.10 27.72 31.30)
	 :both)))

;;; ============================================================================

#+ignore
(defun z-test-one-sample
    (data tails &optional (h0-mean 0) (h0-std-dev 1)
     &rest standard-args &key start end key)
  (declare (ignore start end key)
	   (values z-statistic significance))
  (let ((n (apply #'data-length data standard-args)))
    (when (zerop n)
      (error 'no-data))
    (let ((d (apply #'mean data standard-args))
	  (v (/ h0-std-dev (sqrt n))))
      (when (zerop v)
	(error 'zero-variance))
      (let* ((zs (/ (- d h0-mean) v))
	     sig)
	(values zs sig)))))
	     
;;; ============================================================================
;;; Should go in utilities

(defun inner-product (sample1 sample2 &key start1 end1 start2 end2)
  "Returns the inner product of the two samples, which should be sequences of
numbers.  The inner product, also called the dot product or vector product, is
the sum of the pairwise multiplication of the numbers.  Stops when either sample
runs out; it doesn't check that they have the same length."
  ;; In the following implementation, we extend the `simple-do' implementation
  ;; to handle start/end arguments, but we try to collapse identical code
  ;; fragments, at least in notation if not in object code.  We do this by a
  ;; local macro that builds a `do,' with most parts conditionalized by whether
  ;; the associated `sample' is a list or a vector.
  (macrolet ((inner-product-loop (v/l-1 v/l-2)
	       `(let ((sum 0)
		      (index1 (or start1 0))
		      (index2 (or start2 0))
		      elt1 elt2)
		  ,(case v/l-1
		     (l `(progn (when start1
				  (setf sample1 (nthcdr start1 sample1)))
				(setf elt1 (car sample1))))
		     (v `(progn (when (null end1)
				  (setf end1 (length sample1)))
				(setf elt1 (aref sample1 index1)))))
		  ,(case v/l-2
		     (l `(progn (when start2
				  (setf sample2 (nthcdr start2 sample2)))
				(setf elt2 (car sample2))))
		     (v `(progn (when (null end2)
				  (setf end2 (length sample2)))
				(setf elt2 (aref sample2 index2)))))
		  (do () (nil)
		    (incf sum (* elt1 elt2))	; the work is done here
		    ,(case v/l-1
		       (l `(progn (setf sample1 (cdr sample1))
				  (incf index1)))
		       (v `(incf index1)))
		    ,(case v/l-2
		       (l `(progn (setf sample2 (cdr sample2))
				  (incf index2)))
		       (v `(incf index2)))
		    (when (or ,(case v/l-1
				 (l `(if (null end1) (endp sample1) (>= index1 end1)))
				 (v `(>= index1 end1)))
			      ,(case v/l-2
				 (l `(if (null end2) (endp sample2) (>= index2 end2)))
				 (v `(>= index2 end2))))
		      (return))
		    ,(case v/l-1
		       (l `(setf elt1 (car sample1)))
		       (v `(setf elt1 (aref sample1 index1))))
		    ,(case v/l-2
		       (l `(setf elt2 (car sample2)))
		       (v `(setf elt2 (aref sample2 index2)))))
		  sum)))
    (etypecase sample1
      (list   (etypecase sample2
		(list   (inner-product-loop l l))
		(vector (inner-product-loop l v))))
      (vector (etypecase sample2
		(list   (inner-product-loop v l))
		(vector (inner-product-loop v v)))))))


#+test
(defun test-inner-product ()
  (let ((*print-array* t))
    (spy (inner-product '(1 2 3) '(1 2 3)))
    (spy (inner-product '#(1 2 3) '(1 2 3)))
    (spy (inner-product '(1 2 3) '#(1 2 3)))
    (spy (inner-product '#(1 2 3) '#(1 2 3)))
    (spy (inner-product '(1 2 3) '(1 2 3) :start1 1 :end1 3 :start2 0 :end2 3))
    (spy (inner-product '#(1 2 3) '(1 2 3) :start1 1 :end1 3 :start2 0 :end2 3))
    (spy (inner-product '(1 2 3) '#(1 2 3) :start1 0 :end1 2 :start2 0 :end2 3))
    (spy (inner-product '#(1 2 3) '#(1 2 3) :start1 0 :end1 2 :start2 0 :end2 3)))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1)))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :start1 1))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :end1 4))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :start1 1 :end1 4))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :start2 1))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :start1 1 :start2 2))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :start1 1 :end2 4)))

;;; ============================================================================

(defun covariance (sample1 sample2 &rest args &key start1 end1 start2 end2)
  "Computes the covariance of two samples, which should be equal-length
sequences of numbers.  Covariance is the inner product of differences between
sample elements and their sample means.  For more information, see the manual."
  (let ((n1 (start/end (data-length sample1) start1 end1))
	(n2 (start/end (data-length sample2) start2 end2)))
    (cond ((< n2 n1)
	   (setf end1 (+ (or start1 0) n2)))
	  ((< n1 n2)
	   (setf end2 (+ (or start2 0) n1))))
    (/ (- (apply #'inner-product sample1 sample2 args)
	  (/ (* (start/end (reduce #'+ sample1) start1 end1)
		(start/end (reduce #'+ sample2) start2 end2))
	     n1))
       1 #+ignore (1- n1))))

(defun correlation (sample1 sample2 &rest args &key start1 end1 start2 end2)
  "Computes the correlation coefficient of two samples, which should be
equal-length sequences of numbers."
  (check-type sample1 sequence)
  (check-type sample2 sequence)
  (let ((n1 (start/end (data-length sample1) start1 end1))
	(n2 (start/end (data-length sample2) start2 end2)))
    (unless (= n1 n2)
      (error 'unmatched-sequences))
    (correlation-from-summaries
      n1
      (start/end (reduce #'+ sample1) start1 end1)
      (start/end (reduce #'+ sample1 :key #'square) start1 end1)
      (start/end (reduce #'+ sample2) start2 end2)
      (start/end (reduce #'+ sample2 :key #'square) start2 end2)
      (apply #'inner-product sample1 sample2 args))))
       
(defun correlation-from-summaries (n x x2 y y2 xy)
  "Computes the correlation of two variables given summary statistics of the
variables.  All of these arguments are summed over the variable: `x' is the sum
of the x's, `x2' is the sum of the squares of the x's, and `xy' is the sum of
the cross-products, which is also known as the inner product of the variables x
and y.  Of course, `n' is the number of data values in each variable."
  ;; The computing formula comes from DeVore, page 448, equation 12.18.
  (let ((denom (sqrt (* (- (* n x2) (square x))
			(- (* n y2) (square y))))))
    (when (zerop denom)
      (error 'zero-variance))
    (/ (- (* n xy) (* x y)) denom)))

#+test
(defun test-correlation ()
  "The following data is drawn from example 12.10, page 449, from Probability
and Statistics for Engineering and the Sciences, by Jay L.  DeVore, published by
Brooks/Cole Publishing Company, copyright 1982.  The function above computes the
correct correlation."
  (spy (correlation '(1.98 1.44 2.02 1.20 1.57 1.82 1.45 1.80)
		    '(5.6  7.7  8.8  5.1  6.8  3.9  4.5  5.8)))
  (spy (error-handling (correlation 'a '(2 3 4 5))))
  (spy (error-handling (correlation '() '(2 3 4 5))))
  (spy (error-handling (correlation '(nil) '(2 3 4 5))))
  (spy (error-handling (correlation '(nil nil) '(2 3 4 5))))
  (spy (error-handling (correlation '(1 1 1 1) '(2 3 4 5)))))

;;; ============================================================================

(defun lagged-correlation (sequence1 sequence2 lag)
  "Returns the correlations of `sequence1' with `sequence2' after
shifting `sequence1' by `lag'.  This means that for all n, element n
of `sequence1' is paired with element n+`lag' of `sequence2', where
both of those elements exist."
  (check-type sequence1 sequence)
  (check-type sequence2 sequence)
  (cond
   ((zerop lag)
    (correlation sequence1 sequence2))
   ((plusp lag)
    (correlation sequence1 sequence2 :end1 (- (length sequence1) lag)
		 :start2 lag))
   ((minusp lag)
    (correlation sequence1 sequence2 :start1 (abs lag)
		 :end2 (- (length sequence1) (abs lag))))))

(defun cross-correlation (sequence1 sequence2 max-lag &optional (min-lag 0))
  "Returns a list of the correlation coefficients for all lags from
`min-lag' to `max-lag,' inclusive, where the `i'th list element is the
correlation of the first \(length-of-sequence1 - i\) elements of
sequence1 with with the last i elements of sequence2.  Both sequences
should be sequences of numbers and of equal length."
  (check-type sequence1 sequence)
  (check-type sequence2 sequence)
  (loop for lag from min-lag to max-lag
      collect (lagged-correlation sequence1 sequence2 lag)))

#+test
(defun test-cross-correlation ()
  (let ((a1 (make-array 100))
	(a2 (make-array 100)))
    (dotimes (i 100)
      (setf (aref a1 i) i)
      (setf (aref a2 i) i))
    (format t "~%~{~f~%~}" (cross-correlation a1 a2 40))
    (time:timeit (:cpu :cons)
      (cross-correlation a1 a2 1)))
  (let ((a1 (make-list 100))
	(a2 (make-list 100)))
    (dotimes (i 100)
      (setf (elt a1 i) i)
      (setf (elt a2 i) i))
    (format t "~%~{~f~%~}" (cross-correlation a1 a2 40))
    (time:timeit (:cpu :cons)
      (cross-correlation a1 a2 1))))

;;; ============================================================================

(defun autocorrelation (sample max-lag &optional (min-lag 0))
  "Autocorrelation is merely a cross-correlation between a sample and itself.
This function returns a list of correlations, where the i'th element is the
correlation of the sample with the sample starting at `i.'"
  (cross-correlation sample sample max-lag min-lag))

#+test
(defun test-autocorrelation ()
  (let ((a1 (make-array 100)))
    (dotimes (i 100)
      (setf (elt a1 i) i))
    (spy (autocorrelation a1 40)))
  (let ((a1 (make-list 100)))
    (dotimes (i 100)
      (setf (elt a1 i) i))
    (spy (autocorrelation a1 40))))

;;; ============================================================================

(defun confidence-interval-z (data confidence)
  "Suppose you have a sample of 50 numbers and you want to compute a 90 percent
confidence interval on the population mean.  This function is the one to use.
Note that it makes the assumption that the sampling distribution is normal, so
it's inappropriate for small sample sizes.  Use confidence-interval-t instead.
It returns three values: the mean and the lower and upper bound of the
confidence interval.  True, only two numbers are necessary, but the confidence
intervals of other statistics may be asymmetrical and these values would be
consistent with those confidence intervals.  This function handles 90, 95 and 99
percent confidence intervals as special cases, so those will be quite fast.
`Sample' should be a sequence of numbers.  `Confidence' should be a number
between 0 and 1, exclusive."
  (declare (values mean lower upper))
  (check-type data sequence)
  ;; The Allegro compiler barfs on this...
  ;; It generates this erroneous code...
  ;; (AND (TYPEP #:G3682 'REAL) (> (THE REAL #:G3682) (0)) ...)
  #-(or allegro lucid) (check-type confidence (real (0) (1)))
  (confidence-interval-z-summaries
    (mean data)
    (sqrt (/ (variance data) (length data)))
    confidence))

(defun confidence-interval-z-summaries (mean standard-error confidence)
  "This function is just like `confidence-interval-z,' except that instead of
its arguments being the actual data, it takes the following summary statistics:
`mean', a point estimator of the mean of some normally distributed population;
and the `standard-error' of the estimator, that is, the estimated standard
deviation of the normal population.  `Confidence' should be a number between 0
and 1, exclusive."
  #-(or allegro lucid)(check-type mean real)
  #-(or allegro lucid)(check-type standard-error real)
  ;; The Allegro compiler barfs on this...
  ;; It generates this erroneous code...
  ;; (AND (TYPEP #:G3682 'REAL) (> (THE REAL #:G3682) (0)) ...)
  #-(or allegro lucid) (check-type confidence (real (0) (1)))
  ;; Can't use `case' because that uses `eql' and (eql 9/10 .9) is false.  Z is
  ;; the width of a confidence interval on the standard Gaussian, which we
  ;; multiply by the standard error to get the confidence interval on the true
  ;; sampling distribution.
  (let* ((z     (cond ((= confidence 90/100) 1.645)
		      ((= confidence 95/100) 1.96)
		      ((= confidence 99/100) 2.58)
		      (t (find-critical-value
			   #'(lambda (x)
			       (gaussian-significance x :both))
			   (- 1 confidence)))))
	 (w     (* standard-error z))
	 (upper (+ mean w))
	 (lower (- mean w)))
    (values mean lower upper)))

#+test
(defun test-confidence-interval-z ()
  (spy (confidence-interval-z '(1 2 3 4 5 6) .9))
  (spy (confidence-interval-z '(1 2 3 4 5 6) .95))
  (spy (confidence-interval-z '(1 2 3 4 5 6) .99))
  (spy (confidence-interval-z '(1 2 3 4 5 6) .999))
  (format t "~2&mean~%")
  (time:timeit (:cpu :cons)
    (mean '(1 2 3 4 5 6))
    (mean '(1 2 3 4 5 6))
    (mean '(1 2 3 4 5 6))
    (mean '(1 2 3 4 5 6)))
  (format t "~2&variance~%")
  (time:timeit (:cpu :cons)
    (variance '(1 2 3 4 5 6))
    (variance '(1 2 3 4 5 6))
    (variance '(1 2 3 4 5 6))
    (variance '(1 2 3 4 5 6)))
  (format t "~2&standard deviation~%")
  (time:timeit (:cpu :cons)
    (standard-deviation '(1 2 3 4 5 6))
    (standard-deviation '(1 2 3 4 5 6))
    (standard-deviation '(1 2 3 4 5 6))
    (standard-deviation '(1 2 3 4 5 6)))
  (format t "~2&confidence interval-z--cached~%")
  (time:timeit (:cpu :cons)
    (confidence-interval-z '(1 2 3 4 5 6) .9)
    (confidence-interval-z '(1 2 3 4 5 6) .95)
    (confidence-interval-z '(1 2 3 4 5 6) .99)
    '(confidence-interval-z '(1 2 3 4 5 6) .999))
  (format t "~2&confidence interval-z--computed~%")
  (time:timeit (:cpu :cons)
    (confidence-interval-z '(1 2 3 4 5 6) .9)
    (confidence-interval-z '(1 2 3 4 5 6) .95)
    (confidence-interval-z '(1 2 3 4 5 6) .99)
    (confidence-interval-z '(1 2 3 4 5 6) .999)))

;;; ============================================================================

(defun confidence-interval-t (data confidence)
  "Suppose you have a sample of 10 numbers and you want to compute a 90 percent
confidence interval on the population mean.  This function is the one to use.
This function uses the t-distribution, and so it is appropriate for small sample
sizes.  It can also be used for large sample sizes, but the function
`confidence-interval-z' may be computationally faster.  It returns three values:
the mean and the lower and upper bound of the confidence interval.  True, only
two numbers are necessary, but the confidence intervals of other statistics may
be asymmetrical and these values would be consistent with those confidence
intervals.  `Sample' should be a sequence of numbers.  `Confidence' should be a
number between 0 and 1, exclusive."
  (declare (values mean lower upper))
  (check-type data sequence)
  ;; The Allegro compiler barfs on this...
  ;; It generates this erroneous code...
  ;; (AND (TYPEP #:G3682 'REAL) (> (THE REAL #:G3682) (0)) ...)
  #-(or allegro lucid) (check-type confidence (real (0) (1)))
  (let* ((n (length data)))
    (confidence-interval-t-summaries
      (mean data)
      (- n 1)
      (sqrt (/ (variance data) n))
       confidence)))

(defun confidence-interval-t-summaries (mean dof standard-error confidence)
  "This function is just like `confidence-interval-t,' except that instead of
its arguments being the actual data, it takes the following summary statistics:
`mean,' which is the estimator of some t-distributed parameter; `dof,' which is
the number of degrees of freedom in estimating the mean; and the
`standard-error' of the estimator.  In general, `mean' is a point estimator of
the mean of a t-distribution, which may be the slope parameter of a regression,
the difference between two means, or other practical t-distributions.
`Confidence' should be a number between 0 and 1, exclusive."
  #-(or allegro lucid)(check-type dof (real 1 *))
  #-(or allegro lucid)(check-type mean real)
  #-(or allegro lucid)(check-type standard-error real)
  ;; The Allegro compiler barfs on this...
  ;; It generates this erroneous code...
  ;; (AND (TYPEP #:G3682 'REAL) (> (THE REAL #:G3682) (0)) ...)
  #-(or allegro lucid) (check-type confidence (real (0) (1)))
  (let* ((t-x   (find-critical-value
		  #'(lambda (x)
		      (students-t-significance x dof :both))
		  (- 1 confidence)))
	 (w     (* standard-error t-x))
	 (upper (+ mean w))
	 (lower (- mean w)))
    (values mean lower upper)))

#+test
(defun test-confidence-interval-t ()
  ;; From DeVore, page 335
  (spy (confidence-interval-t
	 '(26.7 25.8 24.0 24.9 26.4 25.9 24.4 21.7 24.1 25.9 27.3 26.9 27.3 24.8 23.6)
	 .95))
  (dolist (n '(10 20 30 50 70 90 120 200))
    (let ((data (loop for i from 1 to n collect (random 1.0)))
	  mean z-low z-high t-low t-high)
      (multiple-value-setq (mean z-low z-high) (confidence-interval-z data .95))
      (multiple-value-setq (mean t-low t-high) (confidence-interval-t data .95))
      (format t "~&dof: ~3d z-width: ~6f t-width: ~6f~%"
	      n (- z-high z-low) (- t-high t-low)))))

;;; ============================================================================

(defun confidence-interval-proportion (x n confidence)
  "Suppose we have a sample of `n' things and `x' of them are ``successes.'' We
can estimate the population proportion of successes as x/n; call it `p-hat.'
This function computes the estimate and a confidence interval on it.  This
function is not appropriate for small samples with p-hat far from 1/2: `x'
should be at least 5, and so should `n'-`x.' This function returns three values:
p-hat, and the lower and upper bounds of the confidence interval.  `Confidence'
should be a number between 0 and 1, exclusive."
  (declare (values p-hat lower upper))
  ;; This formula is from DeVore, page 331
  (let ((p-hat          (/ x n))
	(standard-error (sqrt (/ (* x (- n x)) (* n n n)))))
    (confidence-interval-z-summaries p-hat standard-error confidence)))

#+test
(defun test-confidence-interval-proportion ()
  ;; From DeVore, page 331
  (spy (confidence-interval-proportion 184 244 .90)))

;;; ============================================================================

(defun anova-one-way-variables (iv dv &optional (scheffe-tests-p t))
  "Performs a one-way analysis of variance (ANOVA) on the input data, which
should be two equal-length sequences: `iv' is the independent variable,
represented as a sequence of categories or group identifiers, and `dv' is the
dependent variable, represented as a sequence of numbers.  The `iv' variable
must be ``sorted,'' meaning that AAABBCCCCCDDDD is okay but ABCDABCDABDCDC is
not, where A, B, C and D are group identifiers.  Furthermore, each group should
consist of at least 2 elements.

The significance of the result indicates that the group means are not all equal;
that is, at least two of the groups have significantly different means.  If
there were only two groups, this would be semantically equivalent to an
unmatched, two-tailed t-test, so you can think of the one-way ANOVA as a
multi-group, two-tailed t-test.

This function returns four values: 1.  an ANOVA table; 2.  a list a group means;
3.  either a Scheffe table or nil depending on `scheffe-tests-p'; and 4.  an
alternate value for SST.  The fourth value is only interesting if you think
there are numerical accuracy problems; it should be approximately equal to the
SST value in the ANOVA table.  This function differs from `anova-one-way-groups'
only in its input representation.  See the manual for more information."
  (declare (values anova-table means-list scheffe-table sst-alt))
  (check-type iv sequence)
  (check-type dv sequence)
  (check-type iv sequence)
  (check-type dv sequence)
  (let ((n1 (length iv))
	(n2 (length dv)))
    (when (/= n1 n2) (error 'unmatched-sequences))
    (case n2
      (0 (error 'no-data))
      (1 (error 'insufficient-data)))
    ;; These variable names aren't intuitive without reading the discussion in
    ;; the manual.
    (let* ((N   n2)
	   (GT  (reduce #'+ dv))		; grand total
	   (A   (reduce #'+ dv :key #'square))
	   (B   (/ (square GT) N))
	   (C   0)
	   (NG  0)				; number of groups
	   (group-means nil)
	   (group-sizes nil))
      ;; The following loop calculates C and the group means by going through
      ;; the IV and DV looking for group boundaries.  Using '#:new as the
      ;; current group guarantees that the first element will start a new group,
      ;; because it can't be eql to anything.
      (let ((current-group '#:new) group-sum group-length)
	;; use `map' `nil' because inputs are sequences, not even guaranteed to be
	;; of the same type.  `Loop' would be more efficient, but would require a
	;; four-leaf type tree.  Yuck!
	(map nil #'(lambda (key value)
		     (unless (eql key current-group)
		       ;; process end of group and begin new group
		       (unless (null group-sum)
			 ;; a null group-sum means there is no previous group to end
			 (incf NG)
			 (when (< group-length 2) (error 'insufficient-data))
			 (push (/ group-sum group-length) group-means)
			 (push group-length group-sizes)
			 (incf C (/ (square group-sum) group-length)))
		       (setf current-group key
			     group-sum 0
			     group-length 0))
		     ;; normal group processing
		     (incf group-sum value)
		     (incf group-length))
	     IV DV)
	;; process last group
	(incf NG)
	(when (< group-length 1) (error 'insufficient-data))
	(push (/ group-sum group-length) group-means)
	(push group-length group-sizes)
	(setf group-means (nreverse group-means))
	(setf group-sizes (nreverse group-sizes))
	(incf C (/ (square group-sum) group-length)))
      (let ((SST (- A B))
	    (SSG (- C B))
	    (SSE (- A C)))
	(when (zerop sse) (error 'zero-variance))
	;; The following two computations are for error checking.  See discussion
	;; in the manual.  SST-alt is returned as the second value.
	(let* ((grand-mean (/ GT N))
	       (SST-alt    (reduce #'+ dv :key
				   #'(lambda (elt) (square (- elt grand-mean))))))
	  ;; final calculations
	  (let* ((DFG (1- NG))
		 (DFE (- N NG))
		 (DFT (+ DFG DFE))
		 (MSG (/ SSG DFG))
		 (MSE (/ SSE DFE)))
	    ;; Yes, it's correct to compute the f-significance by a one-tailed
	    ;; test, because if H0 is false, F is known to be biased on the
	    ;; large side, and so we're only interested in the upper tail of the
	    ;; F distribution.
	    (let* ((f (/ msg mse))
		   (p (f-significance (float f) dfg dfe t)))
	      (values `((,DFG ,SSG ,MSG ,f ,p)
			(,DFE ,SSE ,MSE)
			(,DFT ,SST))
		      group-means
		      (when scheffe-tests-p
			(scheffe-tests group-means group-sizes MSE DFE))
		      SST-alt))))))))

#+test
(defun test-anova-one-way-variables ()
  (spy (error-handling (anova-one-way-variables 'a nil)))
  (spy (error-handling (anova-one-way-variables '() 'b)))
  (spy (error-handling (anova-one-way-variables '(a a) '(nil nil))))
  (spy (error-handling (anova-one-way-variables '(a b) '(1 2 3))))
  (spy (error-handling (anova-one-way-variables '(a b) '(1 2))))
  (spy (error-handling (anova-one-way-variables '(a a b b) '(1 1 2 2))))
  )

#+obsolete
(defun two-variables->groups (iv dv)
  "For each unique element in `iv,' this function collects the corresponding
elements of `dv.' The inputs must be sequences.  The output is a list of lists.
Essentially, this function converts implicit grouping into structural grouping.
For example, with the following inputs
  '(a a a b b b c c c c)
  '(1 2 3 4 5 6 7 8 9 0)
it returns:
  '((1 2 3) (4 5 6) (7 8 9 0))
"
  ;; Using #:new as the current group guarantees that the first element will
  ;; start a new group, because it can't be eql to anything
  (let ((current-group '#:new)
	(groups nil))
    ;; reversing the groups is technically unnecessary, but it will make more
    ;; sense to users.
    (macrolet ((reverse-group ()
		 `(when (consp (car groups))
		    (setf (car groups) (nreverse (car groups))))))
      (map nil
	   #'(lambda (iv-elt dv-elt)
		 (if (eql iv-elt current-group)
		     (push dv-elt (car groups))
		     (progn (reverse-group)
			    (push (list dv-elt) groups)
			    (setf current-group iv-elt))))
	   iv
	   dv)
      (reverse-group)
      (setf groups (nreverse groups))
      groups)))

#+test
(defun test-two-variables->groups ()
  (spy (two-variables->groups '(a a a b b b c c c c)
			      '(1 2 3 4 5 6 7 8 9 0)))
  (spy (two-variables->groups '(a a a)
			      '(1 2 3)))
  (spy (two-variables->groups '(a a a b)
			      '(1 2 3 4))))

(defun anova-one-way-groups (data &optional (scheffe-tests-p t))
  "Performs a one-way analysis of variance (ANOVA) on the `data,' which should
be a sequence of sequences, where each interior sequence is the data for a
particular group.  Furthermore, each sequence should consist entirely of
numbers, and each should have at least 2 elements.

The significance of the result indicates that the group means are not all equal;
that is, at least two of the groups have significantly different means.  If
there were only two groups, this would be semantically equivalent to an
unmatched, two-tailed t-test, so you can think of the one-way ANOVA as a
multi-group, two-tailed t-test.

This function returns four values: 1.  an ANOVA table; 2.  a list a group means;
3.  either a Scheffe table or nil depending on `scheffe-tests-p'; and 4.  an
alternate value for SST.  The fourth value is only interesting if you think
there are numerical accuracy problems; it should be approximately equal to the
SST value in the ANOVA table.  This function differs from
`anova-one-way-variables' only in its input representation.  See the manual for
more information."
  (declare (values anova-table means-list scheffe-table sst-alt))
  (check-type data sequence)
  (case (length data)
    (0 (error 'no-data))
    (1 (error 'insufficient-data)))
  (unless (every #'(lambda (group) (< 1 (length group))) data)
    (error 'insufficient-data))
  ;; These variable names aren't intuitive without reading the discussion in the
  ;; manual.
  (let* ((N   (reduce #'+ data :key #'length))
	 (TG  (reduce #'+ data :key #'(lambda (group) (reduce #'+ group))))
	 (A   (reduce #'+ data :key #'(lambda (group)
					(reduce #'+ group :key #'square))))
	 (B   (/ (square TG) N))
	 (C   (reduce #'+ data :key #'(lambda (group)
					(/ (square (reduce #'+ group))
					   (length group)))))
	 (SST (- A B))
	 (SSG (- C B))
	 (SSE (- A C)))
    ;; The following two computations are for error checking.  See discussion in
    ;; the manual.  SST-alt is returned as the last value.
    (let* ((grand-mean (/ TG N))
	   (SST-alt    (reduce #'+ data :key
			       #'(lambda (group)
				   (reduce #'+ group :key
					   #'(lambda (x)
					       (square (- x grand-mean))))))))
      (when (zerop sse)
	(error 'zero-variance))
      ;; final calculations
      (let* ((DFG (1- (length data)))
	     (DFE (- N (length data)))
	     (DFT (+ DFG DFE))
	     (MSG (/ SSG DFG))
	     (MSE (/ SSE DFE)))
	;; Yes, it's correct to compute the f-significance by a one-tailed test,
	;; because if H0 is false, F is known to be biased on the large side,
	;; and so we're only interested in the upper tail of the F distribution.
	(let* ((f (/ msg mse))
	       (p (f-significance (float f) dfg dfe t)))
	  ;; Here's where we start consing.
	  (let ((group-means (map 'list #'mean data)))
	    (values `((,DFG ,SSG ,MSG ,f ,p)
		      (,DFE ,SSE ,MSE)
		      (,DFT ,SST))
		    group-means
		    (when scheffe-tests-p
		      (scheffe-tests group-means (map 'list #'length data)
				     MSE DFE))
		    sst-alt)))))))

#+test
(defun test-anova-one-way-groups ()
  (spy (error-handling (anova-one-way-groups 'a)))
  (spy (error-handling (anova-one-way-groups '())))
  (spy (error-handling (anova-one-way-groups '(a a))))
  (spy (error-handling (anova-one-way-groups '(() ()))))
  (spy (error-handling (anova-one-way-groups '((a) (b)))))
  (spy (error-handling (anova-one-way-groups '((nil nil) (a a)))))
  (spy (error-handling (anova-one-way-groups '((1 1) (2 2))))))


(defun print-anova-table (anova-table &optional (stream *standard-output*))
  "Prints `anova-table' on `stream.'"
  (case (length anova-table)
    ;; one-way anova has 3 lines
    (3 (destructuring-bind ((DFG SSG MSG f p)
			    (DFE SSE MSE)
			    (DFT SST))
			   anova-table
	 (format stream "~2&")
	 (format stream "~14@<~a~>~3@{~14:@<~a~>~}~%" "source of" "degrees of" "sum of" "mean")
	 (format stream "~14@<~a~>~5@{~14:@<~a~>~}~%" "variation" "freedom" "squares" "square" "F" "p")
	 (format stream "~14@<~a~>~14:@<~5:d~>~3@{ ~12,2f ~} ~12,10f~%" "group" dfg ssg msg f p)
	 (format stream "~14@<~a~>~14:@<~5:d~>~2@{ ~12,2f ~}~%" "error" dfe sse mse)
	 (format stream "~14@<~a~>~14:@<~5:d~>~1@{ ~12,2f ~}~%" "total" dft sst)
	 (format stream "~%")))
    ;; two-way anova has 5 lines
    (5 (destructuring-bind ((DFAB SSAB MSAB FAB PAB)
			    (DFA SSA MSA FA PA)
			    (DFB SSB MSB FB PB)
			    (DFE SSE MSE)
			    (DFT SST))
			   anova-table
	 (format stream "~2&")
	 (format stream "~14@<~a~>~3@{~14:@<~a~>~}~%" "source of" "degrees of" "sum of" "mean")
	 (format stream "~14@<~a~>~5@{~14:@<~a~>~}~%" "variation" "freedom" "squares" "square" "F" "p")
	 (format stream "~14@<~a~>~14:@<~5:d~>~3@{ ~12,2f ~} ~12,10f~%" "interaction" dfab ssab msab fab pab)
	 (format stream "~14@<~a~>~14:@<~5:d~>~3@{ ~12,2f ~} ~12,10f~%" "row"         dfa ssa msa fa pa)
	 (format stream "~14@<~a~>~14:@<~5:d~>~3@{ ~12,2f ~} ~12,10f~%" "column"      dfb ssb msb fb pb)
	 (format stream "~14@<~a~>~14:@<~5:d~>~2@{ ~12,2f ~}~%" "error"       dfe sse mse)
	 (format stream "~14@<~a~>~14:@<~5:d~>~1@{ ~12,2f ~}~%" "total"       dft sst)
	 (format stream "~%")))
    (t (error "anova tables have either 3 or 5 lines:  ~s" anova-table))))

#+test
(defun test-print-anova-table ()
  (let ((table (anova-one-way-groups '((1 2 3) (11 12 13)) nil)))
    (print-anova-table table)))

(defun scheffe-tests (group-means group-sizes ms-error df-error)
  "Performs all pairwise comparisons between group means, testing for
significance using Scheffe's F-test.  Returns an upper-triangular table in a
format described in the manual.  Also see the function `print-scheffe-table.'

`Group-means' and `group-sizes' should be sequences.  The arguments `ms-error'
and `df-error' are the mean square error within groups and its degrees of
freedom, both of which are computed by the analysis of variance.  An ANOVA test
should always be run first, to see if there are any significant differences."
  (check-type group-means sequence)
  (check-type group-sizes sequence)
  #-(or allegro lucid)(check-type ms-error real)
  ;; by using `elt,' we sacrifice some efficiency on lists, but it should be
  ;; fairly inconsequential, given that the algorihtm is already O(n^2).
  (let ((N (length group-means)))
    (when (/= N (length group-sizes))
      (error 'unmatched-sequences))
    (let ((N-1 (- N 1)))
      (loop for i from 0 below N-1 collecting
	    (let ((mean-i (elt group-means i))
		  (Ni     (elt group-sizes i)))
	      (loop for j from (1+ i) to N-1 collect
		    ;; Formula from Cohen.  Rather than compute 1/Ni + 1/Nj,
		    ;; which we know will turn into a rational because Ni and Nj
		    ;; are integers, we directly compute (Nj + Ni)/(* Ni Nj)
		    (let* ((mean-j (elt group-means j))
			   (Nj     (elt group-sizes j))
			   (F      (/ (square (- mean-i mean-j))
				      (* ms-error N-1 (/ (+ Ni Nj)
							 (* Ni Nj))))))
		      (list F (f-significance (float f) n-1 df-error t)))))))))

#+test
(defun test-scheffe-tests ()
  ;; From Cohen
  (let ((means '(4.241 3.754 2.847 2.345))
	(sizes '(29 118 59 59)))
    (spy (scheffe-tests means sizes 1.811 261))))

(defun print-scheffe-table
       (scheffe-table &optional group-means (stream *standard-output*))
  "Prints `scheffe-table' on `stream.' If the original one-way anova data had N
groups, the Scheffe table prints as an n-1 x n-1 upper-triangular table.  If
`group-means' is given, it should be a list of the group means, which will be
printed along with the table."
  (let ((N (length scheffe-table)))
    (if (null group-means)
	;; First column is 4 chars wide; the rest are 18 chars wide.
	(progn (format stream "~2&~5t")
	       (loop for row-number from 1 to N do
		     (format stream "~18:@<~d~>| " row-number))
	       (format stream "~%")
	       (loop for row in scheffe-table
		     for row-number from 0 do
		     (format stream "~2d: ~vt~{~{~8,1f ~8,6f | ~}~}~%"
			     row-number (+ 5 (* 20 row-number)) row)))
	;; First column is 12 chars wide; the rest are 18 chars wide.
	(progn (format stream "~2&~13t")
	       (loop for row-number from 1 to N do
		     (format stream "~18:@<~10g~>| "
			     (elt group-means row-number)))
	       (format stream "~%")
	       (loop for row in scheffe-table
		     for row-number from 0 do
		     (format stream "~10g: ~vt~{~{~8,1f ~8,6f | ~}~}~%"
			     (elt group-means row-number)
			     (+ 13 (* 20 row-number))
			     row))))))

#+test
(defun test-print-scheffe-table ()
  ;; From Cohen
  (let ((means '(4.241 3.754 2.847 2.345))
	(sizes '(29 118 59 59)))
    (print-scheffe-table (scheffe-tests means sizes 1.811 261))
    (print-scheffe-table (scheffe-tests means sizes 1.811 261)
			 means)))

#+test
(defun test-anova-one-way ()
  (spy (t-test '(1 2 3) '(11 12 13) :both))
  (spy (anova-one-way-groups '((1 2 3) (11 12 13))))
  (spy (anova-one-way-variables '(a a a b b b) '(1 2 3 11 12 13)))
  ;; From DeVore, example 10.1---equal cell sizes.
  (macrolet ((test (call)
	       `(multiple-value-bind (atab means stab) ,call
		  (print-anova-table atab)
		  (spy means)
		  (print-scheffe-table stab))))
    ;; with arrays of integers
    (test (anova-one-way-groups
	    '#(#(187 211 179 195 221 183)
	       #(199 176 182 200 169 175)
	       #(186 203 217 197 209 208)
	       #(191 189 184 188 177 205))))
    ;; with lists of floats
    (test (anova-one-way-groups
	    '((18.7 21.1 17.9 19.5 22.1 18.3)
	      (19.9 17.6 18.2 20.0 16.9 17.5)
	      (18.6 20.3 21.7 19.7 20.9 20.8)
	      (19.1 18.9 18.4 18.8 17.7 20.5))))
    ;; with arrays of integers
    (test (anova-one-way-variables
	    '#(a a a a a a b b b b b b c c c c c c d d d d d d)
	    '#(187 211 179 195 221 183
		   199 176 182 200 169 175
		   186 203 217 197 209 208
		   191 189 184 188 177 205)))
    ;; with lists of floats
    (test (anova-one-way-variables	
	    '(a a a a a a b b b b b b c c c c c c d d d d d d)
	    '(18.7 21.1 17.9 19.5 22.1 18.3
		   19.9 17.6 18.2 20.0 16.9 17.5
		   18.6 20.3 21.7 19.7 20.9 20.8
		   19.1 18.9 18.4 18.8 17.7 20.5)))
    ))

;;; ============================================================================

(defun anova-two-way-groups (data-array)
  "Calculates the analysis of variance when there are two factors that may
affect the dependent variable.  Because the input is represented as an array, we
can refer to these two factors as the row-effect and the column effect.  Unlike
the one-way ANOVA, there are mathematical difficulties with the two-way ANOVA if
there are unequal cell sizes; therefore, we require all cells to be the same
size, and so the input is a three-dimensional array.

The result of the analysis is an anova-table, as described in the manual.  This
function differs from `anova-two-way-variables' only in its input
representation.  See the manual for further discussion of analysis of variance."
  (destructuring-bind (I J K) (array-dimensions data-array)
    ;; Computing formulas from Devore, page 387, except I've pulled out the
    ;; common subexpressions.  I know there's a lot of rightward creep here,
    ;; because I used `let' instead of `let*' but I like making the data
    ;; dependencies clear.
    (let ((IJK (* I J K))
	  (IJ  (* I J))
	  (JK  (* J K))
	  (IK  (* I K)))
      (let ((x+ (loop for index from 0 below IJK
		      summing (row-major-aref data-array index)))
	    (x2+ (loop for index from 0 below IJK
		       summing (square (row-major-aref data-array index))))
	    (e2  (loop for ii from 0 below I summing
		       (loop for jj from 0 below J summing
			     (square (loop for kk from 0 below K
					   summing (aref data-array ii jj kk))))))
	    (a2  (loop for ii from 0 below I summing
		       (square (loop for jj from 0 below J summing
				     (loop for kk from 0 below K
					   summing (aref data-array ii jj kk))))))
	    (b2  (loop for jj from 0 below J summing
		       (square (loop for ii from 0 below I summing
				     (loop for kk from 0 below K
					   summing (aref data-array ii jj kk)))))))
	(let ((x+2/IJK (/ (square x+) IJK)))
	  ;; Should we multiply through by IJK?  It doesn't save us computation, but
	  ;; it may improve numerical accuracy.  
	  (let ((SST (- x2+ x+2/IJK))
		(SSE (- x2+ (/ e2 K)))
		(SSA (- (/ a2 JK) x+2/IJK))
		(SSB (- (/ b2 IK) x+2/IJK)))
	    (let ((SSAB (- SST SSA SSB SSE)))
	      (let ((df-T  (- IJK 1))
		    (df-E  (- IJK IJ))		; IJ*(K-1)
		    (df-A  (- I 1))
		    (df-B  (- J 1)) 
		    (df-AB (- IJ I J -1)))	; (I-1)*(J-1)
		(let ((MSE  (/ SSE df-E))
		      (MSA  (/ SSA df-A))
		      (MSB  (/ SSB df-B))
		      (MSAB (/ SSAB df-AB)))
		  (let ((FA  (/ MSA MSE))
			(FB  (/ MSB MSE))
			(FAB (/ MSAB MSE)))
		    (let ((pA  (f-significance (float FA) df-A df-E t))
			  (pB  (f-significance (float FB) df-B df-E t))
			  (pAB (f-significance (float FAB) df-AB df-E t)))
		      `((,df-AB ,SSAB ,MSAB ,FAB ,pAB)
			(,df-A ,SSA ,MSA ,FA ,pA)
			(,df-B ,SSB ,MSB ,FB ,pB)
			(,df-E ,SSE ,MSE)
			(,df-T ,SST)))))))))))))

#+test
(defun test-anova-two-way-groups ()
  ;; example 11.4 from Devore
  (let* ((data  '#(10.5 9.2 7.9 12.8 11.2 13.3 12.1 12.6 14.0 10.8 9.1 12.5
			8.1 8.6 10.1 12.7 13.7 11.5 14.4 15.4 13.7 11.3 12.5 14.5
			16.1 15.3 17.5 16.6 19.2 18.5 20.8 18.0 21.0 18.4 18.9 17.2))
	 (data3 (make-array '(3 4 3) :displaced-to data)))
    (loop for i from 0 below 3 do
	  (loop for j from 0 below 4 do
		(loop for k from 0 below 3 do (format t "~4,1f " (aref data3 i j k)))
		(format t " | "))
	  (format t "~%"))
    (let ((table (anova-two-way-groups data3)))
      (print-anova-table table))))

(defun make-3d-table (dv iv1 iv2)
  "Collects the `dv' values for each unique combination of an element of `v1'
and an element of `v2.' Returns a three-dimensional table of dv values."
  (let ((n (length dv)))
    (unless (= n (length iv1) (length iv2))
      (error 'unmatched-sequences))
    ;; Faster implementations may be possible.
    (let ((iv1-values (remove-duplicates iv1))
	  (iv2-values (remove-duplicates iv2)))
      (let ((K (let ((k-temp 0)
		     (iv1-first (elt iv1-values 0))
		     (iv2-first (elt iv2-values 0)))
		 (map nil #'(lambda (iv1-elt iv2-elt)
			      (when (and (eql iv1-first iv1-elt)
					 (eql iv2-first iv2-elt))
				(incf k-temp)))
		      iv1
		      iv2)
		 k-temp)))
	(let ((table (make-array (list (length iv1-values)
				       (length iv2-values)
				       K)
				 :element-type 't
				 :initial-element nil)))
	  ;; construct data table
	  (map nil #'(lambda (dv-value iv1-event iv2-event)
		       (let ((i (position iv1-event iv1-values))
			     (j (position iv2-event iv2-values)))
			 ;; have to search for the first unfilled position.
			 ;; Could be made more efficient with row-major-aref.
			 (let ((pos (dotimes (x K)
				      (when (null (aref table i j x))
					(return x)))))
			   (if (null pos)
			       (error 'unequal-cell-sizes)
			       (setf (aref table i j pos) dv-value)))))
	       dv iv1 iv2)
	  ;; check that there aren't any nil's left
	  (loop for x from 0 below (array-total-size table)
		when (null (row-major-aref table x))
		do (error 'unequal-cell-sizes))
	  table)))))

(defun anova-two-way-variables-unequal-cell-sizes (dv iv1 iv2)
  "Calculates the analysis of variance when there are two factors that may
affect the dependent variable, specifically `iv1' and `iv2.'

The result of the analysis is an anova-table, as described in the manual.  This
function differs from `anova-two-way-groups' only in its input representation.
See the manual for further discussion of analysis of variance.  If you use
`print-anova-table,' the row effect is `iv1' and the column effect is `iv2.'"
  (declare (values ((dfaxb SSAxB MSAxB FAxB pAxB)
		    (dfa   SSA   MSA   FA   pA)
		    (dfb   SSB   MSB   FB   pB)
		    (dfe   SSE   MSE)
		    (dfT   SST))
		   ab-matrix row-totals column-totals grand-total
		   a-labels b-labels))
  (let ((iv1-hash (make-hash-table))
	iv2-hash cell
	num-a num-b a-count b-count
	(harmonic-mean 0)
	(within-groups-subtractand 0)
	(sum-squared-cells 0)
	a-list b-list)
    ;;accumulate cell summations
    ;; in the following a refers to columns, b to rows
    ;; put each value in a nested hash table, selected by a and b
    ;; the cells contain the sum of elements and the number of elements
    (loop
	for a in iv1
	for b in iv2
	for c in dv
	do
	  (setf iv2-hash (gethash a iv1-hash))
	  (when (null iv2-hash)
	    (setf iv2-hash (make-hash-table))
	    (setf (gethash a iv1-hash) iv2-hash))
	  (setf cell (gethash b iv2-hash))
	  (when (null cell)
	    (setf cell (make-list 2 :initial-element 0))
	    (setf (gethash b iv2-hash) cell))
	  (incf (first cell) c)
	  (incf (second cell)))
    ;; get the number of rows and columns and build the column-list
    (setf num-a (hash-table-count iv1-hash))
    (setf cell (gethash (car iv1) iv1-hash))
    (setf num-b (hash-table-count cell))
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (push key a-list))
	     iv1-hash)
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (push key b-list))
	     cell)
    (setf a-list (sort a-list #'string-lessp :key #'(lambda (x) (if (symbolp x) (string x) (format nil "~a" x)))))
    (setf b-list (sort b-list #'string-lessp :key #'(lambda (x) (if (symbolp x) (string x) (format nil "~a" x)))))
    ;; Build the matrix by visiting each cell in each hash table and 
    ;; calculating the means, at the same time, calculate the
    ;; harmonic mean of the sample sizes.
    ;; The harmonic mean is given by the formula
    ;; ab/sum\{1/sij\} where ab is the number of rows times the number
    ;; of columns and sij is the number of elements in cell i,j
    ;; the within-groups-subtractand is calculated here because the
    ;; cell counts are discarded after the ab-matrix is calculated and
    ;; sij is in the denominator of the subtractand for the SS within
    ;; groups.
    (let (row-totals
	  column-totals
	  (grand-total 0)
	  squared-row-totals
	  squared-column-totals
	  squared-grand-total
	  ab-matrix
	  a-value b-value)
      (setf ab-matrix (make-array `(,num-b ,num-a) :initial-element 0))
      (setf a-count 0)
      (dolist (a-key a-list)
	(setf a-value (gethash a-key iv1-hash))
	(when (/= num-b (hash-table-count a-value))
	  (error 'missing-cell))
	(setf b-count 0)
	(dolist (b-key b-list)
	  (setf b-value (gethash b-key a-value))
	  (when (not (member b-key b-list))
	    (error 'missing-cell))
	  (setf (aref ab-matrix b-count a-count)
	    (apply #'/ b-value))
	  (incf harmonic-mean (/ 1 (second b-value)))
	  (incf within-groups-subtractand
		(/ (square (first b-value))
		   (second b-value)))
	  (incf b-count))
	(incf a-count))
      (setf harmonic-mean (* num-a num-b (/ 1 harmonic-mean)))
      (setf row-totals (make-list num-b :initial-element 0))
      (setf column-totals (make-list num-a :initial-element 0))
      (dotimes (a num-a)
	(dotimes (b num-b)
	  (setf cell (aref ab-matrix b a))
	  (incf (nth b row-totals) cell)
	  (incf (nth a column-totals) cell)
	  (incf sum-squared-cells (square cell))))
      (setf grand-total (reduce #'+ row-totals))
      (setf squared-row-totals (reduce #'+ (mapcar #'square row-totals)))
      (setf squared-column-totals (reduce #'+ (mapcar #'square column-totals)))
      (setf squared-grand-total (square grand-total))
      ;; compute the anova values from the summaries caculated above and
      ;; return them in an anova table-like list.
      (let* ((ab (* num-a num-b))
	     (abs2 (reduce #'+ (mapcar #'square dv)))
	     (ssa (* harmonic-mean (- (/ squared-column-totals num-b)
				      (/ squared-grand-total ab))))
	     (ssb (* harmonic-mean (- (/ squared-row-totals num-a)
				      (/ squared-grand-total ab))))
	     (ssaxb (* harmonic-mean
		       (+ (- sum-squared-cells
			     (/ squared-column-totals num-b)
			     (/ squared-row-totals num-a))
			  (/ squared-grand-total ab))))
	     (sse (- abs2 within-groups-subtractand))
	     (dfa (- num-a 1))
	     (dfb (- num-b 1))
	     (dfaxb (* dfa dfb))
	     (dfe (- (length dv) ab))
	     (msa (/ ssa dfa))
	     (msb (/ ssb dfb))
	     (msaxb (/ ssaxb dfaxb))
	     (mse (/ sse dfe))
	     (fa (/ msa mse))
	     (fb (/ msb mse))
	     (faxb (/ msaxb mse))
	     (pa (f-significance (float fa) dfa dfe t))
	     (pb (f-significance (float fb) dfb dfe t))
	     (paxb (f-significance (float faxb) dfaxb dfe))
	     (dft (+ dfa dfb dfaxb dfe))
	     (sst (+ ssa ssb ssaxb sse)))
	(values `((,dfaxb ,SSAxB ,MSAxB ,FAxB ,pAxB)
		  (,dfa ,SSA ,MSA ,FA ,pA)
		  (,dfb ,SSB ,MSB ,FB ,pB)
		  (,dfe ,SSE ,MSE)
		  (,dfT ,SST))
		ab-matrix row-totals column-totals grand-total
		a-list b-list)))))

#+ignore
(defun anova-two-way-variables-unequal-cell-sizes (dv iv1 iv2)
  "Calculates the analysis of variance when there are two factors that may
affect the dependent variable, specifically `iv1' and `iv2.'

The result of the analysis is an anova-table, as described in the manual.  This
function differs from `anova-two-way-groups' only in its input representation.
See the manual for further discussion of analysis of variance.  If you use
`print-anova-table,' the row effect is `iv1' and the column effect is `iv2.'"
  (declare (values ((dfaxb SSAxB MSAxB FAxB pAxB)
		    (dfa   SSA   MSA   FA   pA)
		    (dfb   SSB   MSB   FB   pB)
		    (dfe   SSE   MSE)
		    (dfT   SST))
		   ab-matrix row-totals column-totals grand-total
		   a-labels b-labels))
  (let ((iv1-hash (make-hash-table))
	iv2-hash cell
	num-a num-b a-count b-count
	(harmonic-mean 0)
	(within-groups-subtractand 0)
	(sum-squared-cells 0)
	a-list b-list)
    ;;accumulate cell summations
    ;; in the following a refers to columns, b to rows
    ;; put each value in a nested hash table, selected by a and b
    ;; the cells contain the sum of elements and the number of elements
    (loop
	for a in iv1
	for b in iv2
	for c in dv
	do
	  (setf iv2-hash (gethash a iv1-hash))
	  (when (null iv2-hash)
	    (setf iv2-hash (make-hash-table))
	    (setf (gethash a iv1-hash) iv2-hash))
	  (setf cell (gethash b iv2-hash))
	  (when (null cell)
	    (setf cell (make-list 2 :initial-element 0))
	    (setf (gethash b iv2-hash) cell))
	  (incf (first cell) c)
	  (incf (second cell)))
    ;; get the number of rows and columns and build the column-list
    (setf num-a (hash-table-count iv1-hash))
    (setf cell (gethash (car iv1) iv1-hash))
    (setf num-b (hash-table-count cell))
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (push key b-list))
	     cell)
    ;; Build the matrix by visiting each cell in each hash table and 
    ;; calculating the means, at the same time, calculate the
    ;; harmonic mean of the sample sizes.
    ;; The harmonic mean is given by the formula
    ;; ab/sum\{1/sij\} where ab is the number of rows times the number
    ;; of columns and sij is the number of elements in cell i,j
    ;; the within-groups-subtractand is calculated here because the
    ;; cell counts are discarded after the ab-matrix is calculated and
    ;; sij is in the denominator of the subtractand for the SS within
    ;; groups.
    (let (row-totals
	  column-totals
	  (grand-total 0)
	  squared-row-totals
	  squared-column-totals
	  squared-grand-total
	  ab-matrix)
      (setf ab-matrix (make-array `(,num-b ,num-a) :initial-element 0))
      (setf a-count 0)
      (maphash #'(lambda (a-key a-value)
		   (push a-key a-list)
		   (when (/= num-b (hash-table-count a-value))
		     (error 'missing-cell))
		   (setf b-count 0)
		   (maphash #'(lambda (b-key b-value)
				(when (not (member b-key b-list))
				  (error 'missing-cell))
				(setf (aref ab-matrix b-count a-count)
				  (apply #'/ b-value))
				(incf harmonic-mean (/ 1 (second b-value)))
				(incf within-groups-subtractand
				      (/ (square (first b-value))
					 (second b-value)))
				(incf b-count))
			    a-value)
		   (incf a-count))
	       iv1-hash)
      (setf harmonic-mean (* num-a num-b (/ 1 harmonic-mean)))
      (setf row-totals (make-list num-b :initial-element 0))
      (setf column-totals (make-list num-a :initial-element 0))
      (dotimes (a num-a)
	(dotimes (b num-b)
	  (setf cell (aref ab-matrix b a))
	  (incf (nth b row-totals) cell)
	  (incf (nth a column-totals) cell)
	  (incf sum-squared-cells (square cell))))
      (setf grand-total (reduce #'+ row-totals))
      (setf squared-row-totals (reduce #'+ (mapcar #'square row-totals)))
      (setf squared-column-totals (reduce #'+ (mapcar #'square column-totals)))
      (setf squared-grand-total (square grand-total))
      ;; compute the anova values from the summaries caculated above and
      ;; return them in an anova table-like list.
      (let* ((ab (* num-a num-b))
	     (abs2 (reduce #'+ (mapcar #'square dv)))
	     (ssa (* harmonic-mean (- (/ squared-column-totals num-b)
				      (/ squared-grand-total ab))))
	     (ssb (* harmonic-mean (- (/ squared-row-totals num-a)
				      (/ squared-grand-total ab))))
	     (ssaxb (* harmonic-mean
		       (+ (- sum-squared-cells
			     (/ squared-column-totals num-b)
			     (/ squared-row-totals num-a))
			  (/ squared-grand-total ab))))
	     (sse (- abs2 within-groups-subtractand))
	     (dfa (- num-a 1))
	     (dfb (- num-b 1))
	     (dfaxb (* dfa dfb))
	     (dfe (- (length dv) ab))
	     (msa (/ ssa dfa))
	     (msb (/ ssb dfb))
	     (msaxb (/ ssaxb dfaxb))
	     (mse (/ sse dfe))
	     (fa (/ msa mse))
	     (fb (/ msb mse))
	     (faxb (/ msaxb mse))
	     (pa (f-significance (float fa) dfa dfe t))
	     (pb (f-significance (float fb) dfb dfe t))
	     (paxb (f-significance (float faxb) dfaxb dfe))
	     (dft (+ dfa dfb dfaxb dfe))
	     (sst (+ ssa ssb ssaxb sse)))
	(values `((,dfaxb ,SSAxB ,MSAxB ,FAxB ,pAxB)
		  (,dfa ,SSA ,MSA ,FA ,pA)
		  (,dfb ,SSB ,MSB ,FB ,pB)
		  (,dfe ,SSE ,MSE)
		  (,dfT ,SST))
		ab-matrix row-totals column-totals grand-total
		a-list b-list)))))

(defun anova-two-way-variables (dv iv1 iv2)
    "Calculates the analysis of variance when there are two factors that may
affect the dependent variable, specifically `iv1' and `iv2.' Unlike the one-way
ANOVA, there are mathematical difficulties with the two-way ANOVA if there are
unequal cell sizes; therefore, we require all cells to be the same size; that
is, the same number of values (of the dependent variable) for each combination
of the independent factors.

The result of the analysis is an anova-table, as described in the manual.  This
function differs from `anova-two-way-groups' only in its input representation.
See the manual for further discussion of analysis of variance.  If you use
`print-anova-table,' the row effect is `iv1' and the column effect is `iv2.'"
  (check-type dv sequence)
  (check-type iv1 sequence)
  (check-type iv2 sequence)
  (let ((table (make-3d-table dv iv1 iv2)))
    (anova-two-way-groups table)))

#+test
(defun test-anova-two-way-variables ()
  (let ((iv1 '(H H H H H H H H H H H H
		 IFE IFE IFE IFE IFE IFE IFE IFE IFE IFE IFE IFE
		 P P P P P P P P P P P P))
	(iv2 '(10 10 10 20 20 20 30 30 30 40 40 40
		  10 10 10 20 20 20 30 30 30 40 40 40
		  10 10 10 20 20 20 30 30 30 40 40 40))
	(dv  '(10.5 9.2 7.9 12.8 11.2 13.3 12.1 12.6 14.0 10.8 9.1 12.5 
		    8.1 8.6 10.1 12.7 13.7 11.5 14.4 15.4 13.7 11.3 12.5 14.5 
		    16.1 15.3 17.5 16.6 19.2 18.5 20.8 18.0 21.0 18.4 18.9 17.2)))
    (let ((data3 (make-3d-table dv iv1 iv2)))
      (loop for i from 0 below 3 do
	    (loop for j from 0 below 4 do
		  (loop for k from 0 below 3 do (format t "~4,1f " (aref data3 i j k)))
		  (format t " | "))
	    (format t "~%")))
    (let ((table (anova-two-way-variables dv iv1 iv2)))
      (print-anova-table table))))

;;; ============================================================================

(defun linear-regression-minimal-summaries (n x y x2 y2 xy)
  "Calculates the slope and intercept of the regression line.  This function
differs from `linear-regression-minimal' in that it takes summary statistics:
`x' and `y' are the sums of the independent variable and dependent variables,
respectively; `x2' and `y2' are the sums of the squares of the independent
variable and dependent variables, respectively; and `xy' is the sum of the
products of the independent and dependent variables.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (declare (values slope intercept)
	   (ignore y2))
  (let ((NSSX  (- (* n x2) (* x x)))
	(NSSXY (- (* n xy) (* x y))))
    (when (zerop NSSX)
      (error 'zero-variance))
    (let* ((slope         (/ NSSXY NSSX))
	   (intercept     (/ (- y (* slope x)) n)))
      (values slope intercept))))

(defun linear-regression-minimal (dv iv)
  "Calculates the slope and intercept of the regression line.  This function
takes two equal-length sequences of raw data.  Note that the dependent variable,
as always, comes first in the argument list.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (declare (values slope intercept))
  (check-type dv sequence)
  (check-type iv sequence)
  (let ((ny (length dv))
	(nx (length iv)))
    (when (/= nx ny)
      (error 'unmatched-sequences))
    (linear-regression-minimal-summaries
      nx
      (reduce #'+ iv)
      (reduce #'+ dv)
      (reduce #'+ iv :key #'square)
      (reduce #'+ dv :key #'square)
      (inner-product iv dv))))

#+test
(defun test-linear-regression-minimal ()
  ;; example 12.1 from Devore
  (let ((iv '(2.5 5 10 15 17.5 20 25 30 35 40))
	(dv '(63 58 55 61 62 37 38 45 46 19)))
    (spy (linear-regression-minimal dv iv))
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  ;; example 12.2 from Devore
  (let ((iv '(300 350 400 400 450 450 480 480 530 530 580 580 620 620 670 700))
	(dv '(5.8 4.5 5.9 6.2 6.0 7.5 6.1 8.6 8.9 8.2 14.2 11.9 11.1 11.5 14.5 14.8)))
    (spy (linear-regression-minimal dv iv))
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  ;; example 12.3 from Devore
  (let ((iv '(8.3 8.3 12.1 12.1 17.0 17.0 17.0 24.3 24.3 24.3 33.6))
	(dv '(227 312 362 521 640 539 728 945 738 759 1263)))
    (spy (linear-regression-minimal dv iv))
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  )

(defun linear-regression-brief-summaries (n x y x2 y2 xy)
  "Calculates the main statistics of a linear regression: the slope and
intercept of the line, the coefficient of determination, also known as r-square,
the standard error of the slope, and the p-value for the regression.  This
function differs from `linear-regression-brief' in that it takes summary
variables: `x' and `y' are the sums of the independent variable and dependent
variables, respectively; `x2' and `y2' are the sums of the squares of the
independent variable and dependent variables, respectively; and `xy' is the sum
of the products of the independent and dependent variables.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (declare (values slope intercept determination std-err-slope p-value))
  (let ((NSSX  (- (* n x2) (* x x)))
	(NSSY  (- (* n y2) (* y y)))
	(NSSXY (- (* n xy) (* x y))))
    (when (or (zerop NSSX) (zerop NSSY))
      (error 'zero-variance))
    (let* ((slope         (/ NSSXY NSSX))
	   (intercept     (/ (- y (* slope x)) n))
	   (NSSR          (* slope NSSXY))
	   (NSSE          (- NSSY NSSR))
	   (determination (/ NSSR NSSY))
	   (dof           (- n 2))
	   (std-err-slope (sqrt (/ NSSE (* dof NSSX))))
	   (p-value       (if (zerop NSSE) 0.0 (students-t-significance (/ slope std-err-slope) dof :both))))
      (values slope intercept determination std-err-slope p-value))))

(defun linear-regression-brief (dv iv)
  "Calculates the main statistics of a linear regression: the slope and
intercept of the line, the coefficient of determination, also known as r-square,
the standard error of the slope, and the p-value for the regression.  This
function takes two equal-length sequences of raw data.  Note that the dependent
variable, as always, comes first in the argument list.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (declare (values slope intercept determination std-err-slope p-value))
  (check-type dv sequence)
  (check-type iv sequence)
  (let ((ny (length dv))
	(nx (length iv)))
    (when (/= nx ny)
      (error 'unmatched-sequences))
    (linear-regression-brief-summaries
      nx
      (reduce #'+ iv)
      (reduce #'+ dv)
      (reduce #'+ iv :key #'square)
      (reduce #'+ dv :key #'square)
      (inner-product iv dv))))

#+test
(defun test-linear-regression-brief-summaries ()
  ;; example 12.1 from Devore
  (let ((iv '(2.5 5 10 15 17.5 20 25 30 35 40))
	(dv '(63 58 55 61 62 37 38 45 46 19)))
    (spy (linear-regression-brief dv iv))
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  ;; example 12.2 from Devore
  (let ((iv '(300 350 400 400 450 450 480 480 530 530 580 580 620 620 670 700))
	(dv '(5.8 4.5 5.9 6.2 6.0 7.5 6.1 8.6 8.9 8.2 14.2 11.9 11.1 11.5 14.5 14.8)))
    (spy (linear-regression-brief dv iv))
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  ;; example 12.3 from Devore
  (let ((iv '(8.3 8.3 12.1 12.1 17.0 17.0 17.0 24.3 24.3 24.3 33.6))
	(dv '(227 312 362 521 640 539 728 945 738 759 1263)))
    (spy (linear-regression-brief dv iv))
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  ;; example 12.4 from Devore.  The function's p-value doesn't match Devore's
  ;; because his example is a one-tailed test for whether slope is positive,
  ;; rather than non-zero, which is what we calculate.
  (let ((vals (multiple-value-list
		(linear-regression-brief-summaries 32 3893 290 478537 4160 36473))))
    (format t "~%~@{~f~}~%" (mapcar #'float vals))
    (destructuring-bind (slope ignore r2 se-slope) vals
      (let ((t-stat (/ slope se-slope)))
	(spy t-stat (students-t-significance t-stat 30 :positive)))
      (spy (sqrt r2)))
    (time:timeit (:cpu :cons :number-cons)
      (linear-regression-brief-summaries 32 3893 290 478537 4160 36473))
    (time:timeit (:cpu :cons :number-cons)
      (linear-regression-brief-summaries 32 3893.0 290.0 478537.0 4160.0 36473.0)))
  ;; example 12.5 from Devore.  The p-value doesn't match Devore's because his
  ;; example is testing whether slope=20, rather than whether slope=0.
  (let ((iv '(78 75 78 81 84 86 87))
	(dv '(850 775 750 975 915 1015 1030)))
    (let ((vals (multiple-value-list
		  (linear-regression-brief dv iv))))
      (format t "~%~@{~f~}~%" (mapcar #'float vals))
      (destructuring-bind (slope ignore ignore se-slope) vals
	(let ((dof    (- (length iv) 2))
	      (t-stat (/ (- slope 20) se-slope)))
	  (spy t-stat
	       dof
	       (students-t-significance t-stat dof :positive)
	       (confidence-interval-t-summaries slope dof se-slope .95)))))
    (time:timeit (:cpu :cons :number-cons)
      (linear-regression-brief dv iv)))
  ;; example 12.10 from Devore
  (let ((iv '(1.98 1.44 2.02 1.20 1.57 1.82 1.45 1.80))
	(dv '(5.6 7.7 8.8 5.1 6.8 3.9 4.5 5.8)))
    (multiple-value-bind (ignore ignore r2) (linear-regression-brief dv iv)
      (spy (sqrt r2)))
    (time:timeit (:cpu :cons :number-cons)
      (linear-regression-brief dv iv)))
  )

;;; ============================================================================

(defun linear-regression-verbose-summaries (n x y x2 y2 xy)
  "Calculates almost every statistic of a linear regression: the slope and
intercept of the line, the standard error on each, the correlation coefficient,
the coefficient of determination, also known as r-square, and an ANOVA table as
described in the manual.

If you don't need all this information, consider using the ``-brief'' or
``-minimal'' functions, which do less computation.

This function differs from `linear-regression-verbose' in that it takes summary
variables: `x' and `y' are the sums of the independent variable and dependent
variables, respectively; `x2' and `y2' are the sums of the squares of the
independent variable and dependent variables, respectively; and `xy' is the sum
of the products of the independent and dependent variables.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (declare (values slope intercept determination correlation
		   std-err-slope std-err-intercept anova-table))
  (let ((NSSX  (- (* n x2) (* x x)))
	(NSSY  (- (* n y2) (* y y)))
	(NSSXY (- (* n xy) (* x y))))
    (when (or (zerop NSSX) (zerop NSSY))
      (error 'zero-variance))
    (let* ((slope             (/ NSSXY NSSX))
	   (intercept         (/ (- y (* slope x)) n))
	   (NSSR              (* slope NSSXY))
	   (NSSE              (- NSSY NSSR))
	   (determination     (/ NSSR NSSY))
	   (correlation       (sqrt determination))
	   (dof               (- n 2))
	   (std-err-slope     (sqrt (/ NSSE (* dof NSSX))))
	   (std-err-intercept nil)
	   ;; F = (SSR/1)/(SSE/dof) => (SSR*dof)/SSE => (NSSR*dof)/NSSE
	   (F                 (float (/ (* dof NSSR) NSSE)))
	   (p-value           (f-significance F 1 dof t))
	   (ssr               (/ NSSR n))
	   (sse               (/ NSSE n))
	   (anova-table      `((1 ,ssr ,ssr ,F ,p-value)
			       (,dof ,sse ,(/ sse dof))
			       (,(1+ dof) ,(/ NSSY n)))))
      (values slope intercept determination correlation
	      std-err-slope std-err-intercept anova-table))))

(defun linear-regression-verbose (dv iv)
  "Calculates almost every statistic of a linear regression: the slope and
intercept of the line, the standard error on each, the correlation coefficient,
the coefficient of determination, also known as r-square, and an ANOVA table as
described in the manual.

This function takes two equal-length sequences of raw data.  Note that the
dependent variable, as always, comes first in the argument list.  If you don't
need all this information, consider using the ``-brief,'' or ``-minimal''
functions, which do less computation.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (declare (values slope intercept determination correlation
		   std-err-slope std-err-intercept anova-table))
  (check-type dv sequence)
  (check-type iv sequence)
  (let ((ny (length dv))
	(nx (length iv)))
    (when (/= nx ny)
      (error 'unmatched-sequences))
    (linear-regression-verbose-summaries
      nx
      (reduce #'+ iv)
      (reduce #'+ dv)
      (reduce #'+ iv :key #'square)
      (reduce #'+ dv :key #'square)
      (inner-product iv dv))))

#+test
(defun test-linear-regression-verbose ()
  ;; From Devore, example 12.13
  (let ((iv '(1000 1100 1200 1250 1300 1400 1450))
	(dv '(220 280 350 375 450 470 500)))
    (multiple-value-bind (slope intercept determination correlation std-err-slope std-err-intercept anova-table)
	(linear-regression-verbose dv iv)
      (format t "~%~@{~a ~30t~f~%~}"
	      "slope" slope
	      "intercept" intercept
	      "determination" determination
	      "correlation" correlation
	      "std-err-slope" std-err-slope
	      "std-err-intercept" std-err-intercept)
      (spy anova-table)
      (setf anova-table (mapcar #'(lambda (row)
				    (mapcar #'(lambda (value)
						(if (integerp value) value (float value)))
					    row))
				anova-table))
      (format t "~2%~{~s~%~}" anova-table))))

;;; ============================================================================

(defun multiple-linear-regression (dv &rest ivs)
  "Performs linear regression of the dependent variable, `dv,' on multiple
independent variables, `ivs.' Y on multiple X's, calculating the intercept and
regression coefficient.  Calculates the F statistic, intercept and the
correlation coefficient for Y on X's."
  (declare (values intercept coefficients r-list t-bs betas 
		   r-square f ss-reg-list  ss-percent-list
		   ss-reg ss-res mse-reg mse-res))
  (let* ((num-x (length ivs))
	 (rows (+ 2 num-x))
	 (cols (length dv))
	 (x-0 (make-list cols :initial-element 1))
	 (item-list ()))
      
    ;;Missing values must be handled with care, if they are not paired
    ;; the regression may not be valid. Advise deleting rows with missing
    ;; values.
    (when (member 'nil dv)
      (error 'missing-data))

    ;;build the array initial-element list
    (push x-0 item-list)
    (dolist (x ivs)
      ;;As above for the independent variables.
      (when (member 'nil x)
	(error 'missing-data))
      (push x item-list))
    (push dv item-list)
      
    ;;make the sum-squares cross-product arrays
    (let* ((z-trans-mat (make-array 
			 (list rows cols) 
			 :initial-contents (nreverse item-list)))
	   (z-mat (transpose-matrix z-trans-mat))
	   (z-trans-z (multiply-matrices z-trans-mat z-mat))
	   (x-dim (+ num-x 1))
	   (x-trans-x (make-array (list x-dim x-dim)))
	   (x-trans-y (make-array (list x-dim 1)))
	   (y-trans-y (make-array '(1 1))))
	 
      ;;X transpose X SSCP X-1..X-n X-1..X-n 
      (dotimes (i (array-dimension x-trans-x 0))
	(dotimes (j (array-dimension x-trans-x 1))
	  (setf (aref x-trans-x i j)  
	    (aref z-trans-z i j))))
	 
      ;; X transpose Y SSCP X-1..X-N Y
      (dotimes (i (array-dimension x-trans-y 0))
	(dotimes (j (array-dimension x-trans-y 1))
	  (setf (aref x-trans-y i j)  
	    (aref z-trans-z  i (+ j x-dim)))))
	 
      ;; Y transpose Y SSCP Y Y
      (setf (aref y-trans-y 0 0 )  
	(aref z-trans-z  x-dim x-dim))
	 
      ;;Calculate the regression statistics.
      (let* ((inv-x-t-x (invert-matrix x-trans-x))
	     (b-mat (multiply-matrices 
		     inv-x-t-x 
		     x-trans-y))
	     (intercept (aref b-mat 0 0))
	     (coefficients '())
	     (x-0 (make-array 
		   (list (array-dimension z-mat 0) 1) 
		   :initial-element 1))
	     (x-0-t (transpose-matrix x-0))
	     (y (make-array 
		 (list (array-dimension z-mat 0) 1))))
	;;coefficients of regression for eq. Y = b-1X-1 + b-2X-2 +...+b-nX-n + a
	;; where a is the intercept of the equation.
	(dotimes (i (array-dimension b-mat 0))
	  (unless (= i 0)
	    (push (aref b-mat i 0) coefficients)))
	(setf coefficients (nreverse coefficients))
	    
	;;Raw score array for Y 
	(dotimes (i (array-dimension z-mat 0))
	  (setf (aref y i 0) 
	    (aref z-mat i x-dim)))
	    
	(let* ((x-0-y (multiply-matrices x-0-t y))
	       (n (aref z-trans-z 0 0))	;number of observations
	       (b-trans (transpose-matrix b-mat))
	       (b-t-x-t-y (multiply-matrices 
			   b-trans 
			   x-trans-y))
	       (e-t-e (- 
		       (aref y-trans-y 0 0) 
		       (aref 
			b-t-x-t-y 
			0 0)))		;Sum of squares of the residual

	       ;;This is a miscalculation, mse-reg should be the
	       ;; ss-REG / df-reg, this is ss-RES / a number that
	       ;; is close to df-res i.e. [n - 1], df-res is [n - 1 - df-reg]
	       #+ignore
	       (mse (/ e-t-e (- n 1)))	;mean square error of the regression

	       (sum-sq-y-dev (- (aref y-trans-y 0 0) 
				(/ 
				 (square (aref x-0-y 0 0))
				 n)))	;Sum of squared deviations Y
	       (sum-sq-x-dev nil)
	       (s-b (scalar-matrix-multiply (/ e-t-e (- n num-x 1)) inv-x-t-x)) ;error of the b's
	       (var-y (/ 
		       sum-sq-y-dev  
		       (- n 1)))	;variance of y
	       (s-y (sqrt var-y))	;standard deviation of y
	       (s-x nil)
	       (r-list nil)
	       (t-bs nil)
	       (ss-reg-list nil)
	       (ss-percent-list nil)
	       (s-bs nil)
	       (sum-y-sq (square (aref x-trans-y 0 0))) ;sum of Y squared
	       (sum-y-sq-n (/ sum-y-sq  n)) ;sum of Y squared divided by n
					;sum of squares of the regression
	       (ss-reg (- (aref b-t-x-t-y 0 0) sum-y-sq-n ))
					;sum of squares of the residual
	       (ss-res (- sum-sq-y-dev ss-reg))
					;f-statistic for the regression
	       (mse-reg (/ ss-reg num-x))
	       (mse-res (/ ss-res (- cols num-x 1)))
	       (f (/ mse-reg mse-res))
	       (betas nil)
					;R squared, proportion of the variance
					; accounted for by the independent variables.
	       (r-square (/ ss-reg sum-sq-y-dev )))
	       
	       
	  ;;list the errors of the b's
	  (dotimes (i x-dim)
	    (push (sqrt (aref s-b i i)) s-bs))
	  (setf s-bs (nreverse s-bs))
	       
	  ;;list the sums of squared deviations for each X
	  (dotimes (i x-dim)
	    (unless (= i 0)
	      (push (- (aref x-trans-x i i)
		       (/ (square (aref x-trans-x 0 i)) n))  
		    sum-sq-x-dev )))
	  (setf sum-sq-x-dev (nreverse sum-sq-x-dev))
	       
	  ;;list the standard deviations for each X
	  (dolist (dev sum-sq-x-dev)
	    (push (sqrt (/ dev (- n 1))) s-x ))
	  (setf s-x (nreverse s-x))
	       
	  ;;Calculate and list the standardized coefficients {Betas}
	  (do ((sx s-x (cdr sx))
	       (b coefficients (cdr b)))
	      ((null sx))
	    (push (* (car b) (/ (car sx) s-y)) betas))
	  (setf betas (nreverse betas))
	       
	  ;;list the correlation coefficients for each X and Y
	  (dolist (x ivs)
	    (push (r-score x dv) r-list))
	  
	  ;;list the correlation coefficients for each X and X
	  (dolist (x ivs)
	    (dolist (x-1 ivs)
	      (push (r-score x x-1) r-list)))
	  (setf r-list (nreverse r-list))
	  
	  ;;calculate and list the t-scores for the b's 
	  ;; t > 2 significant at 0.005 level.
	  (dotimes (i num-x 1)
	    (push  (/ (nth i coefficients) (nth (+ i 1) s-bs)) t-bs))
	  (setf t-bs (nreverse t-bs))
	       
	  ;;calculate and list the portion of the sum of squares of the regression due 
	  ;; to each X.
	       
	  (dotimes (i num-x )
	    (push  (* (nth i coefficients) 
		      (- (aref x-trans-y (+ i 1) 0)  
			 ;;either ref (+ i 1) 0
			 ;; or 0 (+ i 1) 
			 ;; access the same cell on the Explorer.  
			 ;; This is bad.
			 (/ (* (aref x-trans-y 0 0)
			       (aref x-trans-x 0 (+ i 1))) n)))
		   ss-reg-list))
	  (setf ss-reg-list  (nreverse  ss-reg-list))
	       
	  ;;calculate and list the percentages for the SS of regression. R(XnY)^2 = percent variance due to Xn
	  (dolist (r r-list)
	    (push (square r) ss-percent-list))
	  (setf ss-percent-list  (nreverse  ss-percent-list))
	       
	  (values intercept coefficients r-list t-bs betas 
		  r-square f ss-reg-list  ss-percent-list
		  ss-reg ss-res mse-reg mse-res))))))

(defun cross-product (number-list-1 number-list-2)
  "Takes two sequences of numbers and returns a sequence of cross products.
Formula XYi = Xi * Yi."
  (unless (or (member 'nil number-list-1) 
	      (member 'nil number-list-2)
	      (not (equal (length  number-list-1) 
			  (length  number-list-2))))
    (mapcar #'* number-list-1 number-list-2)))

(defun r-score (number-list-1 number-list-2)
  "Takes two sequences and returns the correlation coefficient.
Formula:  Sum (Cross-product (Difference-list (number-list-1)
   			Difference-list (number-list-2)) /
	    (Sqrt (Sum-of-Squares (number-list-1) *
		Sum-of-Squares (number-list-2))))."
  
  (let* ((x-diff (difference-list number-list-1))
	 (y-diff (difference-list number-list-2))
	 (sum-cross-product (sum-list (cross-product x-diff y-diff)))
	 (sum-squares-product (* (sum-of-squares number-list-1)
				 (sum-of-squares number-list-2))))
    (if  (zerop sum-squares-product)
	 0
	 (/ sum-cross-product (sqrt sum-squares-product)))))

(defun difference-list (number-list)
  "Takes a sequence of numbers and returns a sequence of differences 
from the mean.
Formula: xi = Xi - Mean (X)."
  (let* ((result ())
	 (numbers (remove-if 'null number-list))
	 (mean (mean numbers)))
    (dolist (number numbers (nreverse result)) 
      (push (- number mean) result))))

(defun sum-list (number-list)
  "Takes a sequence of numbers and returns their sum.
Formula: Sum(X)."
  (reduce #'+ (remove-if 'null number-list)))

;;; ============================================================================

(defun smooth-median-2 (data)
  "Smooths `data' by replacing each element with the median of it and its
neighbor on the left.  A median of two elements is the same as their mean.  The
end is handled by duplicating the end element.  This function is not
destructive; it returns a list the same length as `data,' which should be a
sequence of numbers."
  ;; This is a special case, but we might as well take advantage of it.
  (let ((prev (car data)))
    (map 'list
	 #'(lambda (x)
	     (prog1 (/ (+ x prev) 2)
		    (setf prev x)))
	 data)))

#+test
(defun test-smooth-median-2 ()
  (spy (smooth-median-2 '(1 3 5 7 9)))
  (spy (smooth-median-2 '(1 5 2 6 3 7 4 8))))

(defun smooth-median-3 (data)
  "Smooths `data' by replacing each element with the median of it and its two
neighbors.  The ends are handled by duplicating the end elements.  This function
is not destructive; it returns a list the same length as `data,' which should be
a sequence of numbers."
  (flet ((median-3 (a b c)
	   ;; special code for median of three elts
	   (if (< a b)
	       (if (< a c)
		   (if (< b c) b c)
		   a)
	       (if (< a c)
		   a
		   (if (< b c) c b)))))
    ;; (spy (median-3 1 1 2))
    ;; (spy (median-3 1 2 1))
    ;; (spy (median-3 2 1 1))
    ;; (spy (median-3 1 3 2))
    ;; (spy (median-3 1 2 3))
    ;; (spy (median-3 2 1 3))
    ;; (spy (median-3 1 3 2))
    ;; (spy (median-3 3 1 2))
    ;; (spy (median-3 2 3 1))
    ;; (spy (median-3 3 2 1))
    (let ((n (data-length data)))
      (with-temp-vector (temp (+ n 2))
	(setf (aref temp 0) (elt data 0))
	(replace temp data :start1 1)
	(setf (aref temp (1+ n)) (elt data (- n 1)))
	(map 'list
	     #'median-3
	     (make-array n :displaced-to temp :displaced-index-offset 0)
	     (make-array n :displaced-to temp :displaced-index-offset 1)
	     (make-array n :displaced-to temp :displaced-index-offset 2))))))

#+test
(defun test-smooth-median-3 ()
  (spy (smooth-median-3 '(1 2 3 4 5 6)))
  (spy (smooth-median-3 '(1 1 3 1 1 1)))
  (spy (smooth-median-3 '(1 2 3 2 1 0)))
  (spy (smooth-median-3 '(1 2 3 3 2 1))))

(defun smooth-median-4 (data)
  "Smooths `data' by replacing each element with the median of it, its left
neighbor, and its two right neighbors.  The ends are handled by duplicating the
end elements.  This function is not destructive; it returns a list the same
length as `data,' which should be a sequence of numbers."
  ;; the `median-4' function could probably be improved.
  (flet ((median-4 (a b c d)
	   ;; make `a' the biggest
	   (when (< a b) (rotatef a b))
	   (when (< a c) (rotatef a c))
	   (when (< a d) (rotatef a d))
	   ;; make `d' the smallest
	   (when (< b d) (rotatef b d))
	   (when (< c d) (rotatef c d))
	   ;; median is mean of `b' and `c'
	   (/ (+ b c) 2)))
    (let ((n (data-length data)))
      (with-temp-vector (temp (+ n 3))
	(setf (aref temp 0) (elt data 0))
	(replace temp data :start1 1)
	(let ((last (elt data (- n 1))))
	  (setf (aref temp (+ n 1)) last)
	  (setf (aref temp (+ n 2)) last))
	(map 'list
	     #'median-4
	     (make-array n :displaced-to temp :displaced-index-offset 0)
	     (make-array n :displaced-to temp :displaced-index-offset 1)
	     (make-array n :displaced-to temp :displaced-index-offset 2)
	     (make-array n :displaced-to temp :displaced-index-offset 3))))))

#+test
(defun test-smooth-median-4 ()
  (spy (smooth-median-4 '(1 2 3 4 5 6)))
  (spy (smooth-median-4 '(1 1 3 1 1 1)))
  (spy (smooth-median-4 '(1 2 3 2 1 0)))
  (spy (smooth-median-4 '(1 2 3 3 2 1))))

(defun smooth-median-5 (data)
  "Smooths `data' by replacing each element with the median of it, its two left
neighbors and its two right neighbors.  The ends are handled by duplicating the
end elements.  This function is not destructive; it returns a list the same
length as `data,' which should be a sequence of numbers."
  ;; median-5 can definitely be improved.
  (flet ((median-5 (a b c d e)
	   ;; tournament to make `a' biggest
	   (when (< a b) (rotatef a b))
	   (when (< c d) (rotatef c d))
	   (when (< a c) (rotatef a c))
	   (when (< a e) (rotatef a e))
	   ;; smallest is now b, d, or e; make e smallest
	   (when (< b e) (rotatef b e))
	   (when (< d e) (rotatef b e))
	   ;; now, median is b, c, or d.  Sort them
	   (when (< b c) (rotatef b c))
	   (when (< b d) (rotatef b d))
	   (when (< c d) (rotatef c d))
	   c))
    (let ((n (data-length data)))
      (with-temp-vector (temp (+ n 4))
	(let ((first (elt data 0)))
	  (setf (aref temp 0) first)
	  (setf (aref temp 1) first))
	(replace temp data :start1 2)
	(let ((last (elt data (- n 1))))
	  (setf (aref temp (+ n 2)) last)
	  (setf (aref temp (+ n 3)) last))
	(map 'list
	     #'median-5
	     (make-array n :displaced-to temp :displaced-index-offset 0)
	     (make-array n :displaced-to temp :displaced-index-offset 1)
	     (make-array n :displaced-to temp :displaced-index-offset 2)
	     (make-array n :displaced-to temp :displaced-index-offset 3)
	     (make-array n :displaced-to temp :displaced-index-offset 4))))))

#+test
(defun test-smooth-median-5 ()
  (spy (smooth-median-5 '(1 2 3 4 5 6)))
  (spy (smooth-median-5 '(1 1 3 1 1 1)))
  (spy (smooth-median-5 '(1 2 3 2 1 0)))
  (spy (smooth-median-5 '(1 2 3 3 2 1))))

(defun smooth-hanning (data)
  "Smooths `data' by replacing each element with the weighted mean of it and its
two neighbors.  The weights are 1/2 for itself and 1/4 for each neighbor.  The
ends are handled by duplicating the end elements.  This function is not
destructive; it returns a list the same length as `data,' which should be a
sequence of numbers."
  (flet ((weighted-mean (a b c)
	   (/ (+ a (* 2 b) c) 4)))
    (let ((n (data-length data)))
      (with-temp-vector (temp (+ n 2))
	(let ((first (elt data 0)))
	  (setf (aref temp 0) first))
	(replace temp data :start1 1)
	(let ((last (elt data (- n 1))))
	  (setf (aref temp (+ n 1)) last))
	(map 'list
	     #'weighted-mean 
	     (make-array n :displaced-to temp :displaced-index-offset 0)
	     (make-array n :displaced-to temp :displaced-index-offset 1)
	     (make-array n :displaced-to temp :displaced-index-offset 2))))))

#+test
(defun test-smooth-hanning ()
  (spy (smooth-hanning '(1 2 3 4 5 6)))
  (spy (smooth-hanning '(1 1 3 1 1 1)))
  (spy (smooth-hanning '(1 2 3 2 1 0)))
  (spy (smooth-hanning '(1 2 3 3 2 1))))

(defun smooth-4253H (data)
  "Smooths `data' by successive smoothing: 4,median; then 2,median; then
5,median; then 3,median; then hanning.  The ends are handled by duplicating the
end elements.  This function is not destructive; it returns a list the same
length as `data,' which should be a list of numbers."
  (smooth-hanning
    (smooth-median-3
      (smooth-median-5
	(smooth-median-2
	  (smooth-median-4
	    data))))))

#+test
(defun test-smooth-4253H ()
  ;; I don't know if it's correct, but at least it runs.
  (spy (smooth-4253h '(1 2 3 4 5))))

;;; ============================================================================

(defun chi-square-2x2-counts (a b c d &optional (yates t))
  "Runs a chi-square test for association on a simple 2 x 2 table.  If `yates'
is nil, the correction for continuity is not done; default is t.

Returns the chi-square statistic and the significance of the value."
  (declare (values chi2 p-value))
  (check-type a integer)
  (check-type b integer)
  (check-type c integer)
  (check-type d integer)
  ;; formula from Bohrnstedt and Knoke, page 329.  Just a shortcut for the
  ;; general chi-square formula
  (let ((N     (+ a b c d))
	(denom (* (+ a b) (+ c d) (+ a c) (+ b d)))
	(numer (- (* b c) (* a d))))
    (when yates
      (setf numer (- (abs numer) (/ N 2))))
    ;; finish doing numerator
    (setf numer (* N (square numer)))
    (let* ((chi2    (/ numer denom))
	   (p-value (chi-square-significance (float chi2) 1)))
      (values chi2 p-value))))

#+test
(defun test-chi-square-2x2-counts ()
  ;; From B&K, page 330
  (multiple-value-bind (chi2 p) (chi-square-2x2-counts 521 283 130 384 nil)
    (format t "~2&chi^2 = ~f; p = ~f~%" chi2 p)))

(defun chi-square-2x2 (v1 v2)
  "Performs a chi-square test for independence of the two variables, `v1' and
`v2.' These should be categorial variables with only two values; the function
will construct a 2x2 contingency table by counting the number of occurrences of
each combination of the variables.  See the manual for more details."
  ;; Better, non-consing algorithms certainly exist, but I don't have time now.
  (declare (values chi-square significance contingency-table
		   v1-values v2-values))
  (multiple-value-bind (2x2-table v1-values v2-values)
      (make-contingency-table v1 v2)
    (unless (equal '(2 2) (array-dimensions 2x2-table))
      (error 'not-binary-variables))
    (let ((a (aref 2x2-table 0 0))
	  (b (aref 2x2-table 0 1))
	  (c (aref 2x2-table 1 0))
	  (d (aref 2x2-table 1 1)))
      (multiple-value-call #'values
	(chi-square-2x2-counts a b c d)
	2x2-table v1-values v2-values))))

#+test
(defun test-chi-square-2x2 ()
  (let* ((events (nconc (loop repeat 521 collecting '(a 0))
			(loop repeat 283 collecting '(a 1))
			(loop repeat 130 collecting '(b 0))
			(loop repeat 384 collecting '(b 1))))
	 (v1     (mapcar #'first events))
	 (v2     (mapcar #'second events)))
    (let ((*print-array* t)) (spy (make-contingency-table v1 v2)))
    (multiple-value-bind (chi2 p table v1-values v2-values)
	(chi-square-2x2 v1 v2)
      (format t "~2&chi^2 = ~f; p = ~f~%" chi2 p))))

;;; ============================================================================

(defun chi-square-rxc-counts (contingency-table)
  "Calculates the chi-square statistic and corresponding p-value for the given
contingency table.  The result says whether the row factor is independent of the
column factor.  Does not apply Yate's correction."
  (declare (values chi-square p-value))
  (check-type contingency-table (array * 2))
  (destructuring-bind (rows cols) (array-dimensions contingency-table)
    (macrolet ((row-sum (row)
		 `(loop for jj from 0 below cols
			summing (aref contingency-table ,row jj)))
	       (col-sum (col)
		 `(loop for ii from 0 below rows
			summing (aref contingency-table ii ,col))))
      (let ((chi-2 0)
	    (total (loop for i from 0 below (array-total-size contingency-table)
			 summing (row-major-aref contingency-table i))))
	;; this re-calculates a lot of column sums, but that's okay for now.
	;; We'll worry about efficiency in the next release.
	(dotimes (i rows)
	  (let ((row-sum (row-sum i)))
	    (dotimes (j cols)
	      (let ((expected (/ (* row-sum (col-sum j)) total))
		    (observed (aref contingency-table i j)))
		(incf chi-2
		      (/ (square (- expected observed))
			 expected))))))
	(values chi-2 (chi-square-significance (float chi-2)
					       (* (- rows 1) (- cols 1))))))))

#+test
(defun test-chi-square-rxc-counts ()
  (macrolet ((test (array)
	       `(multiple-value-bind (chi2 p) (chi-square-rxc-counts ,array)
		  (format t "~2&chi-square = ~f~%significance = ~f~%" chi2 p))))
    ;; Devore, example 14.12
    (test #2a((198 202)
	      (140 210)
	      (133 217)))
    ;; Devore, example 14.13
    (test #2a((20 28 23 14 12)
	      (14 34 21 14 12)
	      (04 12 10 20 53)))
    ;; Devore, example 14.14
    (test #2a((24 15 17)
	      (52 73 80)
	      (58 86 36)))
    ))

(defun make-contingency-table (v1 v2)
  "Counts each unique combination of an element of `v1' and an element of `v2.'
Returns a two-dimensional table of integers."
  (declare (values table v1-values v2-values))
  (let ((n (length v1)))
    (when (/= n (length v2))
      (error 'unmatched-sequences))
    ;; Faster implementations may be possible.
    (let ((v1-values (remove-duplicates v1))
	  (v2-values (remove-duplicates v2)))
      (let ((table (make-array (list (length v1-values)
				     (length v2-values))
			       :element-type `(integer 0 ,n)
			       :initial-element 0)))
	;; construct contingency table
	(map nil #'(lambda (v1-event v2-event)
		     (let ((i (position v1-event v1-values))
			   (j (position v2-event v2-values)))
		       (incf (aref table i j))))
	     v1 v2)
	(values table v1-values v2-values)))))

#+test
(defun test-make-contingency-table ()
  (let ((*print-array* t))
    (spy (make-contingency-table '(a a a b b b b c c c c c)
				 '(e f g e f g e f g e f g)))))

(defun chi-square-rxc (v1 v2)
  "Performs a chi-square test for independence of the two variables, `v1' and
`v2.' These should be categorial variables; the function will construct a
contingency table by counting the number of occurrences of each combination of
the variables.  See the manual for more details."
  (declare (values chi-square significance contigency-table
		   v1-values v2-values))
  (check-type v1 sequence)
  (check-type v2 sequence)
  (multiple-value-bind (table v1-values v2-values)
      (make-contingency-table v1 v2)
    (multiple-value-call #'values
      (chi-square-rxc-counts table)
      (g-test table)
      table v1-values v2-values)))

#+test
(defun test-chi-square-rxc ()
  ;; These should be independent
  (loop repeat 20 do
	(let ((v1 (loop repeat 50 collecting (elt '(a b c d) (random 4))))
	      (v2 (loop repeat 50 collecting (elt '(a b c d) (random 4)))))
	  (multiple-value-bind (chi2 p table v1-values v2-values)
	      (chi-square-rxc v1 v2)
	    (format t "chi^2 = ~6,3f; p = ~8,6f~%" chi2 p)))))

;;; ============================================================================

(defun g-test (contingency-table &optional expected-value-matrix)

  "Calculates the G-test for a contingency table.  The formula for the
G-test statistic is

2 * sum[f_ij log [f_ij/f-hat_ij]]

where f_ij is the ith by jth cell in the table and f-hat_ij is the
expected value of that cell.  If an expected-value-matrix is supplied,
it must be the same size as table and it is used for expected values,
in which case the G-test is a test of goodness-of-fit.  If the
expected value matrix is unsupplied, it is calculated from using the
formula

e_ij = [f_i* * f_*j] / f_**

where f_i*, f_*j and f_** are the row, column and grand totals
respectively.  In this case, the G-test is a test of independence.  The degrees of freedom is the same as for the chi-square statistic and the significance is obtained by comparing "

  (declare (values g-score g-significance dof))
  (destructuring-bind (rows columns) (array-dimensions contingency-table)
    (let ((row-totals (make-list rows :initial-element 0))
	  (column-totals (make-list columns :initial-element 0))
	  (g-sum 0)
	  grand-total cell expected-value)
      (dotimes (row rows)
	(dotimes (column columns)
	  (setf cell (aref contingency-table row column))
	  (incf (nth row row-totals) cell)
	  (incf (nth column column-totals) cell)))
      (setf grand-total (reduce #'+ row-totals))
      (dotimes (row rows)
	(dotimes (column columns)
	  (setf cell (aref contingency-table row column))
	  (if expected-value-matrix
	      (setf expected-value (aref expected-value-matrix row column))
	    (setf expected-value (/ (* (nth row row-totals)
				       (nth column column-totals))
				    grand-total)))
	  (incf g-sum (* cell (log (/ cell expected-value))))))
      (let* ((g-score (* 2 g-sum))
	     (dof (* (- rows 1) (- columns 1)))
	     (g-sig (chi-square-significance g-score dof)))
	(values g-score g-sig dof)))))

;;; ***************************************************************************
;;; EOF
