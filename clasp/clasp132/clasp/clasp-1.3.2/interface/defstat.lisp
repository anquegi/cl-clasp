;;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Syntax:COMMON-LISP; Base:10 -*-
;;;; *-* Last-edit: Wednesday, February 17, 1993  15:48:29; Edited-By: carlson *-* 
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

;;; ---------------------------------------------------------------------------

#+ignore
(defmacro defstat (name menu input map-in map-out output &rest options)
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
             (form-uninterned-symbol prefix "-" (princ-to-string (incf sym-counter)))))
      (let* ((documentation (getf options :documentation))
             (menu-symbol (form-symbol "CLASP-" menu))
	     (name-symbol (if (consp name) (first name) name))
	     (name  (if (consp name) 
                      (getf (rest name) :menu name-symbol)
                      (substitute
                       #\space #\- 
                       (string-capitalize 
                        (string name-symbol)))))
             (command-name (form-symbol "COM-" name-symbol))
	     (clasp-function (form-symbol-in-package 'clasp name-symbol))
             (formal-args (mapcar #'(lambda (input-type options)
				      `(,(newvarsym (if (consp input-type) 
						   (first input-type) 
					           input-type))
				        ',input-type
				        ,@options))
			          input
			          (getf options :input-options (circular-list nil))))
             (arg-form (mapcar #'(lambda (arg-spec converter)
			           (cond
				    ((listp converter)
				     `(,(car converter)
				       ,@(mapcar #'(lambda (pos) (car (nth pos formal-args)))
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
			     :menu ,name)

		,formal-args
	      ,@(when documentation `(,documentation))
	      (let ((values (multiple-value-list 
				(,clasp-function . ,arg-form))))
                (assert (= (length values)
                           ,(length output)) ()
                       "`~a' returned ~s values, we were expecting ~s" 
                       ',clasp-function (length values) ,(length output))
		(fresh-line)
                (formatting-item-list (*standard-output* :max-height 60)
                  ,@(loop for output-type in output 
                          for converter in map-out
                          for cnt from 0	   
                          collect `(formatting-cell (*standard-output*)
                                     (present 
                                      ,(if (eq converter t)
                                         `(nth ,cnt values)
                                         `(funcall ',converter (nth ,cnt values)))
                                      ',output-type
                                      :stream *standard-output*
                                      :view clim:+dialog-view+)))))
	      (terpri)))))))

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
