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

;;;; --------------------------------------------------------------------------
;;;;   Functions used in the following macros
;;;; --------------------------------------------------------------------------

;;; Sometimes it's nice to have your gensyms mean something when
;;; you're reading the macroexpansion of some form.  The problem
;;; is that if you give a prefix to GENSYM it remains the prefix
;;; until you change it.  

(eval-when (compile load eval)

(defvar *newsym-counter* 0
  "Counter used by NEWSYM for generating print names.")

(defun newsym (&optional (prefix "X"))
  "Create a new uninterned symbol whose print name begins with PREFIX.
   This differs from GENSYM in that the prefix is not sticky."
  (unless (stringp prefix)
    (setf prefix (string prefix)))
  (make-symbol (format nil "~a~4,'0d" prefix (incf *newsym-counter*))))

) ;; End of Eval-When


;;;; --------------------------------------------------------------------------
;;;;   Creating symbols
;;;; --------------------------------------------------------------------------

(defmacro form-symbol-in-package (pkg &rest names)
  "FORM-SYMBOL-IN-PACKAGE pkg &rest names
   Return a symbol interned in PKG whose print name is the concatenation
   of NAMES.  Each name must be acceptable to the string function."
  `(intern (concatenate
	     'simple-string
	     ,@(mapcar #'(lambda (name)
			   (if (stringp name) name `(string ,name)))
		       names))
	   ,pkg))

(defmacro form-symbol (&rest names)
  "FORM-SYMBOL &rest names
   Return a symbol interned in the current package whose print name is the
   concatenation of NAMES.  Each name must be acceptable to the string function."
  `(form-symbol-in-package *package* ,@names))

(defmacro form-keyword (&rest names)
  "FORM-KEYWORD &rest names
   Return a symbol interned in the keyword package whose print name is the
   concatenation of NAMES.  Each name must be acceptable to the string function."
  `(form-symbol-in-package "KEYWORD" ,@names))

(defmacro form-uninterned-symbol (&rest names)
  "FORM-UNINTERNED-SYMBOL &rest names
   Return an uninterned symbol whose print name is the concatenation of NAMES.
   Each name must be acceptable to the string function."
  `(make-symbol (concatenate
		  'simple-string
		  ,@(mapcar #'(lambda (name)
				(if (stringp name) name `(string ,name)))
			    names))))

;;;; --------------------------------------------------------------------------
;;;; naming functions
;;;; --------------------------------------------------------------------------

(defmethod clasp::name ((symbol symbol))
  (format nil "~a" (symbol-name symbol)))

(defmethod clasp::name ((list cons))
  (let ((*print-length* 10))
    (format nil "~a" list)))

(defmethod clasp::name ((real #-lucid real #+lucid float))
  (with-output-to-string (stream)
    (print-real-converting-ratios real stream)))

(defmethod clasp::description ((symbol symbol))
  (format nil "~@(~a~)" (symbol-name symbol)))

(defmethod clasp::description ((list cons))
  (format nil "~@(~a~)" list))

(defmethod clasp::description ((real #-lucid real #+lucid float))
  (with-output-to-string (stream)
    (print-real-converting-ratios real stream)))

#+lucid
(defmethod clasp::name ((real ratio))
  (with-output-to-string (stream)
    (print-real-converting-ratios real stream)))


;;;; --------------------------------------------------------------------------
;;;; translation functions
;;;; --------------------------------------------------------------------------

(defmethod simplify ((the-column column) &rest options)
  (apply #'clasp::variable-value the-column options))

(defmethod simplify ((the-number-sequence number-sequence) &rest options)
  (apply #'value the-number-sequence options))

(defmethod simplify ((list cons) &key)
  (declare (ignore options))
  list)

(defmethod simplify-and-sort ((list cons))
  (declare (ignore options))
  list)

(defmethod simplify-and-sort ((variable column))
  (variable-value variable
		  :order-by (car (clasp::variable-attribute variable))))

(defmethod simplify-and-sort-by ((variable column) (sort-variable column))
  (variable-value variable
		  :order-by (car (clasp::variable-attribute sort-variable))))


;;; ---------------------------------------------------------------------------
;;; Presentation type that converts to ratios to floats for printing.

(define-presentation-type nice-real () :inherit-from 'real)
  
(clim:define-presentation-method clim:present
    (real (type nice-real) stream view &key)
    (declare (ignore view))
    (print-real-converting-ratios real stream))
    
(defun print-real-converting-ratios (real &optional (stream *standard-output*))
  (typecase real
    (integer (format stream "~1,' ,:d" real))
    ((or rational float) (format stream "~1,4f" real))
    (t (format stream "~1,4,1g" real))))

(define-presentation-type short-nice-real () :inherit-from 'real)
  
(clim:define-presentation-method clim:present
    (real (type short-nice-real) stream view &key)
    (declare (ignore view))
    (print-short-real-converting-ratios real stream))
    
(defun print-short-real-converting-ratios
    (real &optional (stream *standard-output*))
  (typecase real
    (integer (format stream "~1,' ,:d" real))
    ((or rational float) (format stream "~1,2f" real))
    (t (format stream "~1,2,1g" real))))

(define-presentation-type labeled-real () :inherit-from 'nice-real :options (label))

(clim:define-presentation-method clim:present
    (real (type labeled-real) stream view &key)
    (clim:with-text-face (:bold)
      (write-string (string label) stream))
    (write-string ": " stream)
    (present real 'nice-real :stream stream :view view :single-box t))
     
#+ignore
(define-presentation-method present
    (reals (type number-sequence) stream view &key)
  (present (value reals) '(sequence nice-real) :stream stream :view view))

(defmethod display ((number-sequence number-sequence) stream)
  (fresh-line stream)
  (present (value number-sequence) '(sequence nice-real)
	   :stream stream :view +dialog-view+))

;;;; --------------------------------------------------------------------------
;;;; other
;;;; --------------------------------------------------------------------------

(defun print-args (&rest args)
  (print args *standard-output*))

(defmethod find-subclasses ((class standard-class))
  (let ((subclasses nil)
	(direct-subclasses (class-direct-subclasses class)))
    (unless (member class subclasses)
      (push class subclasses)
      (dolist (subclass direct-subclasses)
	(dolist (subsubclass (find-subclasses subclass))
	  (unless (member subsubclass subclasses)
	    (push subsubclass subclasses)))))
    subclasses))

;;;; --------------------------------------------------------------------------
;;;; 
;;;; --------------------------------------------------------------------------


#-Explorer
(defun circular-list (element)
  "Return a circular list of one element"
  (let ((cell (cons element nil)))
    (setf (cdr cell) cell)
    cell))

;;;; --------------------------------------------------------------------------
;;;; --------------------------------------------------------------------------
;;;; EOF
