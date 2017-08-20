;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-
;;;; *-* Last-edit: Wednesday, June 23, 1993  14:08:20; Edited-By: StAmant *-* 
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


(in-package :clasp)

(defun integer-sequence (&optional (start 0) (end 0) &key (by (signum (- end start))))
  (loop for counter from start to end by by
	collect counter))

(defun real-sequence (&optional start end &key number-of-points (interval (signum (- end start))))
  (when number-of-points
    (setf interval (/ (- end start) number-of-points)))
  (loop for counter from start to end by interval
	collect counter))


(defmacro dovector ((loop-variable vector &optional return-value) &body body)
  (let ((loop-counter (gensym)))
    `(let (,loop-variable)
       (dotimes (,loop-counter (length ,vector) ,return-value)
	 (setf ,loop-variable (aref ,vector ,loop-counter))
	 ,@body))))

(defun access (vector &rest args)
  (let (predicate-args)
    (with-keywords-bound
	((filter
	  index
	  predicate)
	 args
	 rest
	 (or predicate-args (setf predicate-args rest)))
      (cond
       ((and filter predicate)
	(let ((new-vector (make-array '(0) :adjustable t :fill-pointer t)))
	  (unless index
	    (setf index (integer-sequence 0 (1- (length vector)))))
	  (dolist (element index)
	    (if (and (pop filter)
		     (apply predicate
			    (mapcar #'(lambda (x)
					(aref x element))
				    predicate-args)))
		(vector-push-extend (aref vector element) new-vector)))
	  new-vector))
       (filter
	(let ((new-vector (make-array '(0) :adjustable t :fill-pointer t)))
	  (unless index
	    (setf index (integer-sequence 0 (1- (length vector)))))
	  (dolist (element index)
	    (if (pop filter)
		(vector-push-extend (aref vector element) new-vector)))
	  new-vector))
       (predicate
	(let ((new-vector (make-array '(0) :adjustable t :fill-pointer t)))
	  (unless index
	    (setf index (integer-sequence 0 (1- (length vector)))))
	  (dolist (element index)
	    (if (apply predicate
		       (mapcar #'(lambda (x)
				   (aref x element))
			       predicate-args))
		(vector-push-extend (aref vector element) new-vector)))
	  new-vector))
       (index
	(let ((new-vector (make-array (list (length index))
				      :initial-contents index)))
	  (dotimes (element (length index))
	    (setf (aref new-vector element)
	      (aref vector (aref new-vector element))))
	  new-vector))
       (t
	vector)))))

#+ignore
(defmacro do-sequence ((element-variable sequence &rest args) &body body)
  (let (predicate-args
	(n (gensym))
	(element (gensym))
	(index-var (gensym)))
    (with-keywords-bound
	((filter
	  index
	  predicate)
	 args
	 rest
	 (or predicate-args (setf predicate-args rest)))
      `(cond
	((and ,filter ,predicate)
	 (setf ,index-var
	   (or ,index (integer-sequence 0 (1- (length ,sequence)))))
	 (dotimes (,element (length ,index-var))
	   (when (and (elt ,filter ,element)
		      (apply ,predicate
			     (mapcar #'(lambda (,n)
					 (elt ,n (elt ,index-var ,element)))
				     (list ,@predicate-args))))
	     (setf ,element-variable (elt ,sequence ,element))
	     ,@body)))
	(,filter
	 (setf ,index-var
	   (or ,index (integer-sequence 0 (1- (length ,sequence)))))
	 (dotimes (,element (length ,index))
	   (when (elt ,filter ,element)
	     (setf ,element-variable (elt ,sequence (elt ,index ,element)))
	     ,@body)))
	(,predicate
	 (setf ,index-var
               (or ,index (integer-sequence 0 (1- (length ,sequence)))))
	 (dotimes (,element (length ,index))
	   (when (apply ,predicate
			(mapcar #'(lambda (,n)
				    (elt ,n (elt ,index ,element)))
				(list ,@predicate-args)))
	     (setf ,element-variable (elt ,sequence (elt ,index ,element)))
             ,@body)))
	(,index
	 (dotimes (,element (length ,index))
	   (setf ,element-variable (elt ,sequence (elt ,index ,element)))
	   ,@body))
	(t
	 (dotimes (,element (length ,sequence))
	   (setf ,element-variable (elt ,sequence ,element))
	   ,@body))))))

#| Testing do-sequence
(setf a '(4 2 1654 23 253 256 1 5))
(setf b '(5 4 0 2 3 1 6 7))
(do-sequence (a-elt a)
  (print a-elt))
(do-sequence (a-elt a :index b)
  (print a-elt))
(do-sequence (a-elt a :filter '(t t t nil nil nil t nil))
  (print a-elt))
(do-sequence (a-elt a :predicate #'> a b)
  (print a-elt))
(do-sequence (a-elt a :index (coerce b 'vector))
  (print a-elt))
(do-sequence (a-elt a :filter #(t t t nil nil nil t nil))
  (print a-elt))
(do-sequence (a-elt a :predicate #'> (coerce a 'vector) (coerce b 'vector))
  (print a-elt))
(do-sequence (a-elt a :predicate #'> a 5)
  (print a-elt))
|#

(defmacro do-sequence ((element-variable sequence &rest args)
		       &body body)
  (let (predicate-args
	(n (gensym))			; should use utils:make-variables
	(element (gensym))
	(position (gensym))
	(index-var (gensym))
	(filter-var (gensym))
	(predicate-var (gensym)))
    (with-keywords-bound
	((filter
	  index
	  predicate)
	 args
	 rest
	 (or predicate-args (setf predicate-args rest)))
      `(let (,element-variable
	     (,index-var ,index)
	     (,filter-var ,filter)
	     (,predicate-var ,predicate))
	 (labels ((execute (,position)
		    (setf ,element-variable (elt ,sequence ,position))
		    ,@body)
		  (do-filtering (,position)
		    (if ,filter
			(if ,predicate
			    (when (and (elt ,filter-var ,position)
				       (funcall #'do-predicate ,predicate-var
						,position ,@predicate-args))
			      (execute ,position))
			  (when (elt ,filter-var ,position)
			    (execute ,position)))
		      (if ,predicate-var
			  (when (funcall #'do-predicate ,predicate-var
					 ,position ,@predicate-args)
			    (execute ,position))
			(execute ,position)))))
	   (if ,index-var
	       (dotimes (,n (length ,index-var))
		 (setf ,element (elt ,index-var ,n))
		 (do-filtering ,element))
	     (dotimes (,element (length ,sequence))
	       (do-filtering ,element))))))))

(defun do-predicate (predicate position &rest args)
  (apply predicate
	 (mapcar #'(lambda (arg)
		     (cond
                       ((typep arg 'sequence)
                        (elt arg position))
                       (t
                        arg)))
                 args)))

(defsetf access (vector &rest args) (sequence)
  `(let (predicate-args)
     (with-keywords-bound
       ((filter
	  index
	  predicate)
	(list ,@args)
	rest
	(or predicate-args (setf predicate-args rest)))
       (cond
	 ((and filter predicate)
	  (let (element)
	    (unless index
	      (setf index (integer-sequence 0 (1- (length ,vector)))))
	    (dotimes (counter (length ,sequence))
	      (setf element (elt index counter))
	      (if (and (elt filter counter)
		       (apply predicate
			      (mapcar #'(lambda (x)
					 (aref x element))
				      predicate-args)))
		  (setf (aref ,vector element) (elt ,sequence counter)))))
	  ,vector)
	 (filter
	  (unless index
	    (setf index (integer-sequence 0 (1- (length ,vector)))))
	  (dotimes (counter (length ,sequence))
	    (if (elt filter counter)
		(setf (aref ,vector (elt index counter))
		      (elt ,sequence counter))))
	  ,vector)
	 (predicate
	  (let (element)
	    (unless index
	      (setf index (integer-sequence 0 (1- (length ,vector)))))
	    (dotimes (counter (length ,sequence))
	      (setf element (elt index counter))
	      (if (apply predicate
			 (mapcar #'(lambda (x)
				     (aref x element))
				 predicate-args))
		  (setf (aref ,vector element) (elt ,sequence counter)))))
	  ,vector)
	 (index
	  (let (element)
	    (dotimes (counter (length ,sequence))
	      (setf element (elt index counter))
	      (setf (aref ,vector element) (elt ,sequence counter))))
	  ,vector)
	 (t
	  (dotimes (counter (length ,sequence))
	    (setf (aref ,vector counter) (elt ,sequence counter)))
	  ,vector)))))
