;;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Base:10 -*-
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

(in-package :clasp-interface)

(define-presentation-method accept ((type result) stream
                                    (view textual-view) &key)
  ;; Allow the user to complete from the list of frame variables.
  (completing-from-suggestions (stream :partial-completers '(#\-))
     (dolist (item (append (clasp::find-instances 'result) (clasp::find-instances 'dataset) (clasp::find-instances 'column)))
       (suggest (clasp::name-string item) item))
     (loop for x being the present-symbols in 'clasp-user
	 when (boundp x)
	 do (suggest (string x) (symbol-value x)))))
		   

(define-presentation-method accept ((type dataset) stream
                                    (view textual-view) &key)
  ;; Allow the user to complete from the list of frame variables.
  (completing-from-suggestions (stream :partial-completers '(#\-))
     (dolist (dataset (clasp::find-instances 'dataset))
       (suggest (clasp::name-string dataset) dataset))))

(define-presentation-method accept ((type variable) stream
				    (view textual-view) &key)
  ;; Allow the user to complete from the list of frame variables.
  (completing-from-suggestions (stream :partial-completers '(#\-))
     (dolist (variable (clasp::find-instances 'variable))
       (suggest (clasp::name-string variable) variable))))

#+NOT-NEEDED?
(define-presentation-method accept ((type numbers) stream
						   (view textual-view) &key)
  (accept '(or column number-sequence) :stream stream :view view :prompt nil))

(define-presentation-method accept ((type column) stream
                                    (view textual-view) &key)
  ;; Allow the user to complete from the list of frame variables.
  (completing-from-suggestions (stream :partial-completers '(#\-))
     (dolist (variable (clasp::find-instances 'column))
       (suggest (clasp::name-string variable) variable))))

#+THIS-REALLY-SUCKS
;; This one does not really work yet. The accept method for (sequence number) sucks.
(define-presentation-method accept ((type number-sequence) stream
						  (view textual-view) &key)
  (accept '(sequence number) :stream stream :view view :prompt nil))

(define-presentation-translator nice-real-to-expression
    (nice-real expression clasp
	    :gesture :select
	    :documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (princ object stream))
	    :pointer-documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (princ object stream))
	    :menu nil)
  (object)
  object)

(define-presentation-translator symbol-to-number-sequence
    (expression number-sequence clasp
	    :gesture :select
	    :tester ((object)
		     (and (symbolp object)
			  (boundp object)
			  (listp (symbol-value object))
			  (every #'numberp (symbol-value object))))
	    :tester-definitive t
	    :documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (format stream "~a" (symbol-name object)))
	    :pointer-documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (format stream "~a" (symbol-name object)))
	    :menu nil)
  (object)
  (symbol-value object))

(define-presentation-translator sequence-to-number-sequence
    (expression number-sequence clasp
	    :gesture :select
	    :tester ((object)
		     (and (consp object)
			  (every #'numberp object)))
	    :tester-definitive t
	    :documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (format stream "~a" object))
	    :pointer-documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (format stream "~a" object))
	    :menu nil)
  (object)
  object)

(define-presentation-translator number-sequence-to-sequence
    (number-sequence expression clasp
		     :gesture :select
		     :documentation
		     ((object presentation context-type frame stream)
		      (declare (ignore presentation context-type frame))
		      (format stream "~a" (value object)))
		     :pointer-documentation
		     ((object presentation context-type frame stream)
		      (declare (ignore presentation context-type frame))
		      (format stream "~a" (value object)))
		     :menu nil)
  (object)
  `',(value object))

(define-presentation-type partition-clause () :inherit-from 'expression)

(define-presentation-translator column-to-sequence
    (column expression clasp
	    :gesture :select
	    :documentation
	    ((object stream)
	     (format stream "~a" (variable-value object)))
	    :pointer-documentation
	    ((object stream)
	     (format stream "~a" (variable-value object)))
	    :menu nil)
  (object)
  `',(variable-value object))

(define-presentation-translator column-to-variable-name
    (column partition-clause clasp
	    :gesture :select
	    :documentation
	    ((object stream)
	     (format stream "~a" (name object)))
	    :pointer-documentation
	    ((object stream)
	     (format stream "~a" (name object)))
	    :menu nil)
  (object)
  (name object))
