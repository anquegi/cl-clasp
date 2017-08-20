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

(define-command-table clasp-sample)

(add-menu-item-to-command-table 'clasp "Sample" :menu 'clasp-sample
				:errorp nil :after "Test"
				:documentation
				"Sample from theoretical distributions")

(define-command (com-sample-uniform :command-table clasp-sample :name t
				    :menu ("Sample from Uniform Distribution"
					   :documentation
					   "Sample from Uniform Distribution"))
    ((minimum 'integer :prompt "Minimum" :default 0)
     (maximum 'integer :prompt "Maximum" :default 1)
     (integer-1 #-lucid '(integer 1 *) #+lucid 'integer
		:prompt "Size of Samples")
     (integer-2 #-lucid '(integer 1 *) #+lucid 'integer
		:prompt "Number of Samples"))
  (present-with-update
   (funcall
    #'(lambda (size-of-samples number-of-samples)
	(sample-to-dataset (make-instance 'uniform-distribution
			     :minimum minimum
			     :maximum maximum)
			   size-of-samples
			   number-of-samples))
    integer-1 integer-2)
   :stream *standard-output*
   :view +dialog-view+))

(define-command (com-sample-normal :command-table clasp-sample :name t
				   :menu ("Sample from Normal Distribution"
					  :documentation
					  "Sample from Normal Distribution"))
    ((real-1 'real :prompt "Mean")
     (real-2 'real :prompt "Standard Deviation")
     (integer-1 #-lucid '(integer 1 *) #+lucid 'integer
		  :prompt "Size of Samples")
     (integer-2 #-lucid '(integer 1 *) #+lucid 'integer
		  :prompt "Number of Samples"))
  (present-with-update
   (funcall
    #'(lambda (mean std-dev size-of-samples number-of-samples)
	(sample-to-dataset (make-instance 'normal-distribution :mean mean
					  :standard-deviation std-dev)
			   size-of-samples
			   number-of-samples))
    real-1 real-2 integer-1 integer-2)
   :stream *standard-output*
   :view +dialog-view+))

(define-command (com-sample-binomial :command-table clasp-sample :name t
				     :menu
				     ("Sample from Binomial Distribution"
				      :documentation
				      "Sample from Binomial Distribution"))
    ((real-1 'real :prompt "P")
     (integer-1 #-lucid '(integer 1 *) #+lucid 'integer
		:prompt "N")
     (integer-2 #-lucid '(integer 1 *) #+lucid 'integer
		:prompt "Size of Samples")
     (integer-3 #-lucid '(integer 1 *) #+lucid 'integer
		:prompt "Number of Samples"))
  (present-with-update
   (funcall
    #'(lambda (p n size-of-samples number-of-samples)
	(sample-to-dataset (make-instance 'binomial-distribution :p p :n n)
			   size-of-samples
			   number-of-samples))
    real-1 integer-1 integer-2 integer-3)
   :stream *standard-output*
   :view +dialog-view+))

(define-command (com-sample-poisson :command-table clasp-sample :name t
				    :menu ("Sample from Poisson Distribution"
					   :documentation
					   "Sample from Poisson Distribution"))
    ((real-1 'real :prompt "Mean")
     (integer-1 #-lucid '(integer 1 *) #+lucid 'integer
		:prompt "Size of Samples")
     (integer-2 #-lucid '(integer 1 *) #+lucid 'integer
		:prompt "Number of Samples"))
  (present-with-update
   (funcall
    #'(lambda (mean size-of-samples number-of-samples)
	(sample-to-dataset (make-instance 'poisson-distribution :mean mean)
			   size-of-samples
			   number-of-samples))
    real-1 integer-1 integer-2)
   :stream *standard-output*
   :view +dialog-view+))

(define-command (com-sample-gamma :command-table clasp-sample :name t
				  :menu ("Sample from Gamma Distribution"
					 :documentation
					 "Sample from Gamma Distribution"))
    ((integer-1 #-lucid '(integer 1 *) #+lucid 'integer
		:prompt "Integer Order")
     (integer-2 #-lucid '(integer 1 *) #+lucid 'integer
		:prompt "Size of Samples")
     (integer-3 #-lucid '(integer 1 *) #+lucid 'integer
		:prompt "Number of Samples"))
  (present-with-update
   (funcall
    #'(lambda (ia size-of-samples number-of-samples)
	(sample-to-dataset (make-instance 'gamma-distribution :ia ia)
			   size-of-samples
			   number-of-samples))
    integer-1 integer-2 integer-3)
   :stream *standard-output*
   :view +dialog-view+))


;;; ---------------------------------------------------------------------------
;;; old sampling commands

#+ignore
(defclaspcom ("Sample From Uniform Distribution"
	   :function
	   #'(lambda (size-of-samples number-of-samples)
	       (sample-to-dataset
		(make-instance 'uniform-distribution)
		size-of-samples number-of-samples)))
    sample
    ((integer 1 *) (integer 1 *)) (t t) (t) (dataset)
    :input-options ((:prompt "Size of Samples") (:prompt "Number of Samples")))


#+ignore
(defclaspcom ("Sample From Normal Distribution"
	   :function
	   #'(lambda (mean standard-deviation size-of-samples number-of-samples)
	       (sample-to-dataset
		(make-instance 'normal-distribution :mean mean
					  :standard-deviation standard-deviation)
		size-of-samples number-of-samples)))
    sample
  (real real (integer 1 *) (integer 1 *)) (t t t t) (t) (dataset)
  :input-options ((:prompt "Mean") (:prompt "Standard Deviation")
		  (:prompt "Size of Samples") (:prompt "Number of Samples")))


#+ignore
(defclaspcom ("Sample From Binomial Distribution"
	   :function
	   #'(lambda (p n size-of-samples number-of-samples)
	       (sample-to-dataset
		(make-instance 'binomial-distribution :p p :n n)
				  size-of-samples number-of-samples)))
    sample
    (real real (integer 1 *) (integer 1 *)) (t t t t) (t) (dataset)
    :input-options ((:prompt "P") (:prompt "N")
		    (:prompt "Size of Samples") (:prompt "Number of Samples")))


#+ignore
(defclaspcom ("Sample From Poisson Distribution"
	   :function
	   #'(lambda (mean size-of-samples number-of-samples)
	       (sample-to-dataset
		(make-instance 'poisson-distribution :mean mean)
				  size-of-samples number-of-samples)))
    sample
    (real (integer 1 *) (integer 1 *)) (t t t) (t) (dataset)
    :input-options ((:prompt "Mean")
		    (:prompt "Size of Samples") (:prompt "Number of Samples")))


#+ignore
(defclaspcom ("Sample From Gamma Distribution"
	   :function
	   #'(lambda (ia size-of-samples number-of-samples)
	       (sample-to-dataset
		(make-instance 'gamma-distribution :ia ia)
		size-of-samples number-of-samples)))
    sample
    ((integer 1 *) (integer 1 *) (integer 1 *)) (t t t) (t) (dataset)
    :input-options ((:prompt "Integer Order")
		    (:prompt "Size of Samples") (:prompt "Number of Samples")))
