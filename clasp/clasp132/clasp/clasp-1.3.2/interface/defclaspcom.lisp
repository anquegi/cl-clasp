;;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Syntax:COMMON-LISP; Base:10 -*-
;;;; *-* Last-edit: Wednesday, February 17, 1993  15:48:29; Edited-By: carlson *-* 
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; Copyright (c) 1990 - 1993 University of Massachusetts
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

;;; ---------------------------------------------------------------------------

(defmacro define-clasp-command
    (name arguments &body body)
  (let* (use-mapping-p
	 mapping-option
	 mapped-command-name
	 mapped-arguments
	 mapping-option-arguments
	 map-list
	 mapped-body
	 (body-place-holder (gensym "BODY-PLACE-HOLDER"))
	 (cross-product-list body-place-holder))
    (dolist (argument arguments)
      (if (and (symbolp argument)
	       (eq argument '&key))
	  (push argument mapped-arguments)
	(progn
	  (setf mapping-option (getf (cddr argument) :mapping-option))
	  (remf (cddr argument) :mapping-option)
	  (push (copy-list argument) mapped-arguments)
	  (when mapping-option
	    (setf use-mapping-p t)
	    (setf (first (car mapped-arguments))
	      (gensym (format nil "~a-SEQUENCE"
			      (first (car mapped-arguments)))))
	    (setf (second (car mapped-arguments))
	      (list 'quote (list 'sequence
				 (second (second (car mapped-arguments))))))
	    (push (first (first mapped-arguments)) mapping-option-arguments)
	    (cond
	     ((eq mapping-option :map)
	      (setf map-list
		(append map-list
			(list 'for (car argument)
			      'in (first (car mapped-arguments))))))
	     ((eq mapping-option :cross-product)
	      (setf cross-product-list
		(cons cross-product-list
		      (nreverse
		       (list 'loop 'for (car argument) 'in
			     (first (car mapped-arguments))
			     'do)))))
	     (t nil))))))
    (when use-mapping-p
      (setf mapped-arguments (reverse mapped-arguments))
      (setf mapped-command-name (copy-list name))
      (setf (first mapped-command-name)
	(intern (format nil "COM-MAP-~a"
			(if (string= "COM-"
				     (subseq (format nil "~a" (car name)) 0 4))
			    (subseq (format nil "~a" (car name)) 4)
			  (format nil "~a-MAP" (first name))))))
      (unless (getf (cdr name) :command-table)
	(setf (getf (cdr name) :command-table) 'clasp))
      (setf (getf (cdr mapped-command-name) :command-table) 'clasp-map)
      (setf mapped-body (reverse-and-substitute*
			 (cons 'progn body)
			 body-place-holder 
			 cross-product-list))
      (when map-list
	(setf mapped-body
	  (append '(loop) map-list (list 'do mapped-body))))
      (setf mapped-body `(let ((mapping-p
				(some #'(lambda (x)
					  (> (length x) 1))
				      (list ,@mapping-option-arguments))))
			   (declare (special mapping-p))
			   ,mapped-body)))
    (setf body `((let ((mapping-p nil))
		   (declare (special mapping-p))
		   ,@body)))
    `(progn
       ,(if `,use-mapping-p
	    (list 'define-command `,name `,mapped-arguments `,mapped-body)
	  (append (list 'define-command `,name `,arguments) `,body)))
    ))

(defun find* (item list &key (test #'eql))
  (cond
   ((null list)
    (null item))
   ((listp (car list))
    (or (find* item (car list) :test test)
	(find* item (cdr list) :test test)))
   (t
    (or (funcall test item (car list))
	(find* item (cdr list) :test test)))))

(defun reverse-and-substitute* (new-item old-item list &rest args)
  (if (atom list)
      (car (apply #'nsubstitute new-item old-item (list list) args))
    (nreverse
     (mapcar #'(lambda (element)
		 (cond
		  ((null element)
		   nil)
		  ((listp element)
		   (apply #'nsubstitute new-item old-item
			  (reverse-and-substitute* new-item old-item
						   element)
			  args))
		  (t (car (apply #'nsubstitute new-item old-item (list element)
				 args)))))
	     list))))

#+ignore
(define-clasp-command (com-test-mapping
		       :command-table clasp-graphing
		       :name t)
    ((dv1 'column :prompt "DV1" :mapping-option :map)
     (dv2 'column :prompt "DV2" :mapping-option :map)
     (iv1 'column :prompt "IV1" :mapping-option :cross-product)
     (iv2 'column :prompt "IV2" :mapping-option :cross-product))
  (place (make-regression-plot-icon iv1 dv1))
  (place (make-regression-plot-icon iv2 dv2)))

(defmacro defclaspcom
    (name menu input map-in map-out output &rest options)
  (assert (= (length input)
	     (length map-in)) ()
  "the length of the input presentation types was ~s; the length in-converters was ~s"
  (length input) (length map-in))
  (assert (= (length map-out)
	     (length output)) ()
      "the length out-converters was ~s; the length of the output presentation types was ~s"
  (length map-out) (length output))
  (let ((sym-counter 0))
    (flet ((newvarsym (prefix)
             (form-uninterned-symbol prefix "-"
				     (princ-to-string (incf sym-counter)))))
      (let* ((documentation (getf options :documentation))
             (menu-symbol (form-symbol "CLASP-" menu))
	     (name-symbol (if (consp name)
			      (car name)
			    name))
	     (function (if (consp name)
			   (getf (cdr name) :function name-symbol)
			 name))
	     (clasp-function (if (symbolp function)
				 `(function ,(form-symbol-in-package
					      'clasp function))
			       function))
	     (menu-name  (or (if (consp name) (getf (cdr name) :menu))
			     (substitute
			      #\space #\- 
			      (string-capitalize 
			       (string name-symbol)))))
	     (command-name (form-symbol "COM-" name-symbol))
             (formal-args (mapcar #'(lambda (input-type options)
				      `(,(newvarsym (if (consp input-type) 
						   (first input-type) 
					           input-type))
				        ',input-type
				        ,@options))
			          input
			          (getf options
					:input-options (circular-list nil))))
             (arg-form (mapcar #'(lambda (arg-spec converter)
			           (cond
				    ((listp converter)
				     `(,(car converter)
				       ,@(mapcar
					  #'(lambda (pos)
					      (car (nth pos formal-args)))
					  (cdr converter))))
				    ((eq converter t)
				     (first arg-spec))
				    (t
				     `(,converter ,(first arg-spec)))))
                               formal-args
			       map-in)))
        
        `(progn (define-command (,command-name 
				 :command-table ,menu-symbol 
				 :name t 
				 :menu ,menu-name)
		    ,formal-args
		  ,@(when documentation `(,documentation))
		  (let ((values (multiple-value-list 
				    (funcall ,clasp-function . ,arg-form))))
		    (assert (= (length values)
			       ,(length output)) ()
		      "`~a' returned ~s values, we were expecting ~s" 
		      ',name-symbol (length values) ,(length output))
		    (fresh-line)
		    (formatting-item-list (*standard-output* :max-height 60)
			,@(loop for output-type in output 
			      for converter in map-out
			      for cnt from 0	   
			      collect `(formatting-cell
					(*standard-output*)
					(present 
					 ,(if (eq converter t)
					      `(nth ,cnt values)
					    `(funcall ',converter
						      (nth ,cnt values)))
					 ',output-type
					 :stream *standard-output*
					 :view clim:+dialog-view+)))))
		  (terpri)))))))

;;; ----------------------------------------------------------------------------
