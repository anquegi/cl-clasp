;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/carlson/clasp/v3dev/sampling-fns.lisp *-*
;;;; *-* Last-edit: Monday, November 23, 1992  12:11:33; Edited-By: carlson *-* 
;;;; *-* Machine: Lester (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                           Sampling Functions                           *
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
;;;  08-31-92 File Created.  (carlson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clasp)

;;; --*--
;;; ***************************************************************************

;;; ***************************************************************************
;;; Class definitions
;;; ***************************************************************************

(defclass distribution () ())

(defclass binomial-distribution (distribution)
  ((p
     :type #-EXPLORER (real 0 1) #+EXPLORER real
     :documentation "The probability of success for each Bernoulli trial"
     :initarg :p
     :accessor binomial-distribution-p)
   (n
     :type (integer 1 *)
     :documentation "The number of Bernoulli trials per experiment"
     :initarg :n
     :accessor binomial-distribution-n)))

#+EXPLORER
(defmethod (setf p) (p (binomial-distribution binomial-distribution))
  (check-type p (real 0 1))
  (setf (slot-value binomial-distribution 'p) p))

(defclass normal-distribution (distribution)
  ((mean
     :type float
     :documentation "The mean of the distribution"
     :initarg :mean
     :accessor normal-distribution-mean)
   (standard-deviation
     :type float
     :documentation "The standard deviation of the distribution"
     :initarg :standard-deviation
     :accessor normal-distribution-standard-deviation)))

(defclass gamma-distribution (distribution)
  ((ia
     :type (integer 1 *)
     :documentation "The integer order of the distribution"
     :initarg :ia
     :accessor gamma-distribution-ia)))

(defclass poisson-distribution (distribution)
  ((mean
     :type float
     :documentation "The mean of the distribution"
     :initarg :mean
     :accessor poisson-distribution-mean)))
  
(defclass uniform-distribution (distribution)
	  ((minimum :type float
		    :documentation "The minimum of the distribution"
		    :initarg :minimum
		    :accessor uniform-distribution-minimum)
	   (maximum :type float
		    :documentation "The maximum of the distribution"
		    :initarg :maximum
		    :accessor uniform-distribution-maximum)))


;;; ***************************************************************************
;;; generic methods
;;; ***************************************************************************

(defmethod sample-to-list ((dist distribution) size-of-sample)
  "Gets a sample from the distribution `dist' of size `size-of-sample'"

  (let (values)
    (dotimes (i size-of-sample)
      (push (deviate dist) values))
    values))


(defmethod sample-to-dataset
    ((dist distribution) size-of-samples
     &optional (number-of-samples 1)
	       (dataset-name (format nil "~a" (class-name (class-of dist))))
	       (variable-names
		(make-list
		 number-of-samples
		 :initial-element
		 (format nil "~a" (class-name (class-of dist))))))
  "Gets a sample from the distribution `dist' of size `size-of-sample'
and creates a dataset out of it.  If `number-of-samples' is > 1 then
multiple samples are taken from the distribution and each one forms
one column of the dataset."
  (let ((new-dataset (make-dataset :name dataset-name)))
    (dotimes (i number-of-samples)
      (add-variable-to-dataset
       new-dataset
       (sample-to-list dist size-of-samples)
       (pop variable-names)))
    new-dataset)
  )

(defmethod deviate ((dist distribution))
  "Return a single deviate from the distribution `dist'"
  )

;;; ***************************************************************************
;;; Distribution specific methods
;;; ***************************************************************************


;;; ***************************************************************************
;;; Binomial distribution
;;; ***************************************************************************

(defconstant fpi (coerce pi 'single-float) "Pi as a single-float.")

(defparameter *binomial-deviate-algorithm-threshold* 25
  "When computing a binomial deviate with parameters `p' and `n', this
is the minimum `n' for preferring the complicated rejection algorithm
over the simple sum-of-random-bits algorithm.")
(defvar *binomial-deviate-old-n* -1 
  "A static variable for use by `binomial-deviate.' The previous value of `n.'") 
(defvar *binomial-deviate-old-g* nil 
  "A static variable for use by `binomial-deviate.' `gammaln' of n+1.")
(defvar *binomial-deviate-old-p* -1 
  "A static variable for use by `binomial-deviate.' The previous value of `p.'")
(defvar *binomial-deviate-plog* nil 
  "A static variable for use by `binomial-deviate.' The log of `p'.")
(defvar *binomial-deviate-pclog* nil 
  "A static variable for use by `binomial-deviate.' The log of 1-p.")

(defmethod deviate ((dist binomial-distribution))
  "Returns a single value sampled from a Bernoulli experiment with
Pr(success)=`p' and trials=`n'.  The implementation follows Numerical
Recipes in C, section 7.3"
  (let* ((p (binomial-distribution-p dist))
	 (n (binomial-distribution-n dist))
	 (pp  (if (<= p 0.5) p (- 1 p)))   ; Binomial is symmetric around .5
	 (am  (* n pp))			   ; expected value of deviate to be produced
	 (dev nil))			   ; deviate to be produced
    (cond ((< n *binomial-deviate-algorithm-threshold*)
	   ;; This algorithm is just the sum of random bits
	   (setf dev 0)
	   (dotimes (j n)
	     (if (< (random 1.0) p)
		 (incf dev))))
	  ((< am 1.0)
	   ;; Distribution is accurately Poisson, so use direct Poisson method.
	   (let ((g  (exp (- am)))
		 (tt 1.0))
	     (setf dev
	       ;; NB: `dotimes' is being called for value here: j if the
	       ;; `return' happens, otherwise `n'
		   (dotimes (j n n)
		     (setf tt (* tt (random 1.0)))
		     (if (< tt g) (return j))))))
	  (t
	   ;; Use the rejection method.
	   ;; Compute and store stuff.  
	   ;; This helps if the algorithm is called many times with the same
	   ;; p and n
	   (when (/= n *binomial-deviate-old-n*)	   
	     (setf *binomial-deviate-old-n* n)
	     (setf *binomial-deviate-old-g* (gammaln (coerce (1+ n)
							     'single-float))))
	   (when (/= pp *binomial-deviate-old-p*)
	     (setf *binomial-deviate-old-p* pp)
	     (setf *binomial-deviate-plog* (log pp))
	     (setf *binomial-deviate-pclog* (log (- 1.0 pp))))
	   (let* ((en (coerce n 'single-float))
		  (pc (- 1.0 pp))
		  (sq (sqrt (* 2.0 am pc)))
		  (tt    nil)
		  (angle nil)
		  (y     nil)
		  (em    nil))
	     ;; We implement C's do-while as do-unless-return
	     (do () (nil)
	       (do () (nil)
		 (setf angle (* fpi (random 1.0)))
		 (setf y (tan angle))
		 (setf em (+ am (* sq y)))
		 (unless (or (< em 0.0) (>= em (+ en 1.0)))
		   (return)))
	       (setf em (float (floor em) em))
	       ;; For greater speed, these globals should be cached in locals.
	       (setf tt (* 1.2 sq (+ 1.0 (* y y))
			   (exp (+ *binomial-deviate-old-g*
				   (- (gammaln (+ em 1.0)))
				   (- (gammaln (+ en (- em) 1.0)))
				   (* em *binomial-deviate-plog*)
				   (* (- en em) *binomial-deviate-pclog*)))))
	       (unless (> (random 1.0) tt)
		 (return)))
	     (setf dev em))))
    (if (/= pp p)
	(setf dev (- n dev)))
    dev))


;;; ***************************************************************************
;;; Normal distribution
;;; ***************************************************************************

(defvar *standard-normal-deviate-storage* nil 
  "Store second value calculated by `normal-deviate' so that only half
the calls actually require computation")

(defmethod deviate ((dist normal-distribution))
  "Get's a single value sampled from the normal distribution with mean
`mean' and standard devation `standard-deviation.'  This uses the
algorithm from Numerical Recipes in C"
  ;; extract parameters of distribution from dist
  (let ((mean (normal-distribution-mean dist))
	(standard-deviation (normal-distribution-standard-deviation dist)))
    ;; if there is a stored standard-normal-deviate from a previous
    ;; iteration, use it, otherwise, calculate two more, returning one and
    ;; saving the other
    (if *standard-normal-deviate-storage*
	(let* ((retval (+ mean (* standard-deviation
				  *standard-normal-deviate-storage*))))
	  (setf *standard-normal-deviate-storage* nil)
	  retval)
	(do* ((v1 (1- (* 2 (random 1.0))) (1- (* 2 (random 1.0))))
	      (v2 (1- (* 2 (random 1.0))) (1- (* 2 (random 1.0))))
	      (r (+ (* v1 v1) (* v2 v2)) (+ (* v1 v1) (* v2 v2))))
	    ;; if (v1,v2) lie in the unit circle, return
	    ;; v1*sqrt(-2.0*log(r)/r) and store v2*sqrt(-2.0*log(r)/r)
	    ;; otherwise, try again
	     ((and (> r 0) (< r 1))
	      (setf *standard-normal-deviate-storage*
		(* v2 (sqrt (* -2.0 (/ (log r) r)))))
	      (+ mean (* standard-deviation
			 (* v1 (sqrt (* -2.0 (/ (log r) r))))))))
	)))


;;; ***************************************************************************
;;; Gamma distribution
;;; ***************************************************************************

(defparameter *gamma-deviate-algorithm-threshold* 6
  "The minimum integer order for using the complicated rejection
method rather than the simpler sum-of-exponentials method.")

(defmethod deviate ((dist gamma-distribution))
  "Returns a single sample from a gamma distribution of integer order `ia'"
    ;; extract parameters of distribution from dist
  (let ((ia (gamma-distribution-ia dist)))
    (cond ((< ia 1) (error "ia must be >= 1"))
	  ;; If ia is less than *gamma-deviate-algorithm-threshold*,
	  ;; just take the product of a bunch of uniform deviates and then
	  ;; take the log
	  ((< ia *gamma-deviate-algorithm-threshold*)
	   (let ((retval 1.0))
	     (dotimes (j ia (log retval))
	       (setf retval (* retval (random 1.0))))))
	  ;; Finally, in the non-trivial case, you actually have to figure
	  ;; stuff out... using the rejection method described in Numerical
	  ;; Recipes in C, this means: Pick a value for x, and calculate f(x),
	  ;; pick a new value under f(x) and compare it to p(x).  If it is
	  ;; greater, reject it and try again, otherwise, that is your
	  ;; deviate.  That's the theory, now how do we do that in real life?
	  ;; The comparison function for a Gamma distribution is
	  ;; f(x)={C0/[1+(X-X0)^2/A0^2]} and A0*C0 must be kept as small as
	  ;; possible to keep f(x) everywhere greater than p(x). This function
	  ;; is based on a distribution whose inverse indefinite integral is
	  ;; tan, so, working backwards, x=A0 tan(pi U) + X0 where U is a
	  ;; uniform deviate in [0,1], X0 is ia - 1, and A0 is
	  ;; sqrt(2*(ia-1) + 1).  Once you have your x, pick another uniform
	  ;; random deviate and compare it to p(x) and loop if it is bigger.
	  (t (do* ((tangent (tan (* pi (random 1.0)))
			    (tan (* pi (random 1.0))))
		   (am (1- ia) (1- ia))
		   (s (sqrt (+ (* 2.0 am) 1)) (sqrt (+ (* 2.0 am) 1)))
		   (retval (+ (* s tangent) am) (+ (* s tangent) am)))
		 ((and (> retval 0.0)
		       (> (random 1.0) (* 
					(+ 1.0 (* tangent tangent)) 
					(exp (- (* am (log 
						       (/ retval am))) 
						(* s tangent))))))
		  retval)
	       )
	     )
	  )
    )
  )


;;; ***************************************************************************
;;; Poisson distribution
;;; ***************************************************************************

(defvar *sq* nil)
(defvar *alxm* nil)
(defvar *g* nil)
(defvar *oldm* -1.0)
(defconstant *minimum-exp-able-double*  (log least-positive-double-float))
	    
(defmethod deviate ((dist poisson-distribution))
  "Returns a single sample from a poisson distribution with mean `mean'"
  ;; extract parameters of distribution from dist
  (let ((mean (poisson-distribution-mean dist)))
    ;; for small values of mean, do what poisson means, i.e.  count the the
    ;; number of cumulative random products are needed to pass (exp -mean)
    (if (< mean 12.0)
	(progn
	   ;; if the mean has changed, recalculate some useful stuff
	  (if (/= mean *oldm*)
	      (progn
		(setf *oldm* mean)
		(setf *g* (exp (- mean)))))
	  (do ((em -1.0 (+ em 1.0))
	       (t1 1.0 (* t1 (random 1.0))))
	      ((<= t1 *g*) em)))
	;; for bigger values of mean, a rejection method must be used
	(progn
	   ;; if the mean has changed, recalculate some useful stuff
	  (if (/= mean *oldm*)
	      (progn
		(setf *oldm* mean)
		(setf *sq* (sqrt (* 2.0 mean)))
		(setf *alxm* (log mean))
		(setf *g* (- (* mean *alxm*) (gammaln (+ mean 1.0))))))
	  (do* ((y)
		(em)
		(t1)
		(tmp))
	       ((progn 
		  ;; y is a deviate from a Lorentzian comparison function em
		  ;; is y shifted (by mean) and scaled (by sqrt of 2*mean).
		  ;; If em < 0.0 go through the loop again
		  (setf em (do* ((y1 (tan (* pi (random 1.0)))
				     (tan (* pi (random 1.0))))
				 (em1 (+ (* *sq* y1) mean)
				      (+ (* *sq* y1) mean)))
			       ((>= em1 0.0) (setf y y1) (floor em1))))
		  ;; This is a stupid kludge designed to get around the fact
		  ;; that exp of a very low number (like -709) will bomb with a
		  ;; double-to-small error.  When this is the case, just
		  ;; substitute a 0.0 (since if the double is to small, it is
		  ;; something like 0.000000000005616, which is close to 0.0).
		  ;; If tmp is not to small to be exp'ed, then calculate t1.
		  (setf tmp (- (* em *alxm*) (gammaln (+ em 1.0)) *g*))
		  (setf t1 (* 0.9 (+ 1.0 (* y y))
			      (if (< tmp *minimum-exp-able-double*)
				  0.0
				(exp tmp))))
		  ;; If the transformation is above a uniform deviate, then
		  ;; return em, otherwise try again.
		  (>= t1 (random 1.0))) em))))))


;;; ***************************************************************************
;;; Uniform distribution
;;; (included for completeness more than usefullness)
;;; ***************************************************************************

(defmethod deviate ((dist uniform-distribution))
  "Returns a single sample from the uniform distribution between 0
\(inclusive\) and 1 \(exclusive\)"
  (with-slots (minimum maximum) dist
    (if (and (slot-boundp dist 'minimum)
	     (slot-boundp dist 'maximum)
	     (not (null minimum))
	     (not (null maximum)))
	(+ (* (random 1.0) (- maximum minimum)) minimum)
      (random 1.0))))


;;; ***************************************************************************
;;;; Support functions
;;; ***************************************************************************
;;; ln(Gamma) function
;;; ***************************************************************************

;;; Some of the rejection technique sampling functions require a call to
;;; gammaln which returns the ln of the gamma function.  The one part of this
;;; that I'm particularly unsure about is the use of the reflection formula.
;;; However, since the arguments to gammaln seem always to be > 1 for my
;;; purposes, this formula is unnecessary (it is used when the argument to
;;; gammaln is >0 and < 1).

(defun gammaln (xx)
  "Returns the ln of the gamma function of `xx'.  Don't use for `xx' <
1 until I figure out how to use the reflection formula given in
Numerical Recipes in C, chap 6.1, formula 6.1.4"
  (let* ((x (1- xx))
	 (tmp (+ x 5.5))
	 (ser 1.0)
	 (cof '(76.18009173l0 -86.50532033l0 24.01409822l0 
		-1.231739516l0 0.00120858003l0 -0.00000536382l0)))
    (setf tmp (- tmp (* (+ x 0.5) (log tmp))))
    (dolist (j cof)
      (incf x)
      (setf ser (+ ser (/ j x))))
    (+ (- tmp) (log (* 2.50662827465l0 ser)))))

;;; ***************************************************************************
;;; EOF
