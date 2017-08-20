;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10; Fonts:(MEDFNT) -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clasp/clasp/development-v2/utilities.lisp *-*
;;;; *-* Last-edit: Thursday, April 8, 1993  11:51:48; Edited-By: File Server *-* 
;;;; *-* Machine: Dizzy (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                                Utilities                               *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: David L. Westbrook
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
;;;  01-19-93 File Created.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :CLASP)

;;; --*--
;;; ***************************************************************************

#-Explorer
(defmacro spy (&rest forms &aux (stream '*trace-output*) no-newline)
  "A debugging tool: wrapping this around a form causes both the form and its
values to be printed on *trace-output*.  Like `progn,' this returns the value\(s\)
of the last form, so that it can be safely wrapped about functional code as
well.  In other words, \(spy \(foo\)\) returns the same values as \(foo\).
The first form can optionally be a list of options, eg., keyword value pairs\).
Options supported:
:STREAM <stream>      - directs output to `stream'
:NO-NEWLINE <boolean> - if non-nil suppresses insertion of newlines"
  (when (and (consp (first forms)) (keywordp (first (first forms))))
    (let ((options (pop forms)))
      (setf stream (getf options :stream stream)
            no-newline (getf options :no-newline))))
    
  (flet ((spy-1 (form)
	   `(let ((form   ',form)
		  (values  (multiple-value-list ,form)))
              ,@(unless no-newline
                  `((fresh-line ,stream)))
	      (format ,stream "~s =>~{ ~s~}" form values)
              ,@(if no-newline
                  `((write-char #\space ,stream))
                  `((terpri ,stream)))
	      (values-list values))))
    `(progn ,@(mapcar #'spy-1 forms))))

;;; ----------------------------------------------------------------------------

(defmacro undefclass (name supers slots &rest options)
  (declare (ignore supers slots options))
  `(setf (find-class ',name) nil))

;;;----------------------------------------------------------------------------

(defmacro map-over-classes (function class-list &key key predicate partial-order &aux (class (gensym)))
  `(dolist (,class ,@(if partial-order `((sort-using-list-order (copy-list ,class-list)
                                                                ,partial-order))
                         `(,class-list)))
     ,@(if key `((setf ,class (funcall ,key ,class))))
     (when (funcall ,predicate ,class)
       (funcall ,function ,class))))

;;; ----------------------------------------------------------------------------

(defun sort-using-list-order (x list &key (test #'eq) key (list-key #'identity))
  (flet ((item-lessp-function (list  test item-key list-key)
           (if item-key
             #'(lambda (item-1 item-2)
                 (setf item-2 (funcall item-key item-2)
                       item-1 (funcall item-key item-1))
                 (let ((item-2-present (member item-2 list :test test :key list-key))
                       (item-1-present (member item-1 list :test test :key list-key)))
                   (if (and item-1-present item-2-present)
                     (not (member item-1 item-2-present :test test :key list-key))
                     ;; otherwise
                     (if item-1-present t nil))))
             #'(lambda (item-1 item-2)
                 (let ((item-2-present (member item-2 list :test test :key list-key))
                       (item-1-present (member item-1 list :test test :key list-key)))
                   (if (and item-1-present item-2-present)
                     (not (member item-1 item-2-present :test test :key list-key))
                     ;; otherwise
                     (if item-1-present t nil)))))))
    (let ((lessp-function (item-lessp-function list test key list-key)))
      (stable-sort x lessp-function))))

;;;----------------------------------------------------------------------------

(defun class-built-on-class (class potential-superclass)
  (member potential-superclass
          #-MCL
	  (if #+Explorer (ticlos::class-composed-p class)
              #-Explorer (class-finalized-p class)
	      (class-precedence-list class)
	      (class-direct-superclasses class))
          #+MCL
          (class-precedence-list class)
	  :test #'eq))

;;; ----------------------------------------------------------------------------

(defun format-if (stream format-string &rest format-args)
  (when stream (apply #'format stream format-string format-args)))

(defun report-if (flag stream format-string &rest format-args)
  (when (and flag stream)
    (apply #'format stream format-string format-args)))

;;;----------------------------------------------------------------------------

(defun fill-all (array elt &key (start 0) (end (array-total-size array)))
  "Fills an array ignoring the fill pointer."
  (let ((fp (fill-pointer array)))
    (setf (fill-pointer array) end)
    (fill array elt :start start :end end)
    (setf (fill-pointer array) fp)))

;;; ----------------------------------------------------------------------------

(defmacro doplist ((key value plist) &body body)
  "Iterate over each key/value pair in a plist.  Key and Value are
bound to successive pairs in Plist."
  (let ((atvar (gensym "PLIST")))
    `(do* ((,atvar ,plist (cddr ,atvar))
	   (,key (car ,atvar) (car ,atvar))
	   (,value (second ,atvar) (second ,atvar)))
	  ((null ,atvar) nil)
       ,@body))) 

;;; ----------------------------------------------------------------------------

(defun DATE-AND-TIME (&OPTIONAL (time (get-universal-time)))
  
  "DATE-AND-TIME [universal-time]

Returns formatted date/time string."
  
  (multiple-value-bind (second minute hour date month year day)
                       (decode-universal-time time)
    (format nil 
            "~a, ~a ~a, ~a  ~2,'0d:~2,'0d:~2,'0d"
            (nth day
                 '("Monday" "Tuesday" "Wednesday" "Thursday"
                   "Friday" "Saturday" "Sunday"))
            (nth (1- month)
                 '("January" "February" "March" "April" "May"
                   "June" "July" "August" "September"
                   "October" "November" "December"))
            date
            year
            hour
            minute
            second)))

;;;----------------------------------------------------------------------------

#-lcl4.0
(DEFUN xor (&rest args)
  "Takes any number of arguments and returns T if an odd number of its arguments are non-NIL, 
otherwise returns NIL."
  (LET ((flag nil))
    (DOLIST (form args flag)
      (WHEN form
	    (SETF flag (not flag))))))

;;;----------------------------------------------------------------------------
#-lucid
(declaim (inline square))
#+lucid
(proclaim '(inline square))

(defun square (x)
  "Returns the square of x"
  (* x x))

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
;;;;   Keyword Parsing
;;;; --------------------------------------------------------------------------

;;; WITH-KEYWORDS-BOUND:
;;;
;;; (with-keywords-bound (((key1 default1)
;;;                        (key2 default2)
;;;                        ...)
;;;                       arglist)
;;;   <body>)

(defmacro with-keywords-bound ((key-specs arg-list &rest args)
			       &body body)

  "WITH-KEYWORDS-BOUND (key-specs arg-list [error-msg | keyvar] &rest otherwise)
                 &body body)

   Look through ARG-LIST for keywords and bind them, similar to the
   processing of lambda-list keywords.  ARG-LIST is evaluated and should
   be a list of keywords and values.  KEY-SPECS describes the keywords
   to check for.  Each element should be a symbol that will be bound to
   the keyword value from ARG-LIST or a list of the symbol and a default
   value.

   If the third argument is a string it is used as the format string for
   the error message when an unknown keyword is found.

   If the third argument is a symbol then it is used as the variable
   which is used internally to hold the remaining part of the list during
   processing.  OTHERWISE is code that will be executed (in an implicit
   progn) if any keyword from ARG-LIST is not a recognized keyword.
   This code can refer to KEYVAR."

  (let* ((key-symbols (mapcar #'(lambda (key)
				  (if (symbolp key) key (car key)))
			      key-specs))
	 (error-msg (if (stringp (car args)) (car args) nil))
	 (keyvar (if (and (symbolp (car args))
			  (not (null (car args))))
		     (car args)
		     (newsym "KEY")))
	 (otherwise (if error-msg nil (cdr args))))
    (or otherwise
	error-msg
	(setf error-msg (format nil "~~s is not one of (~{:~a~^ ~})."
				key-symbols)))
    `(let ,key-specs
       (when (consp ,arg-list)
	 (do ((,keyvar ,arg-list (cdr ,keyvar)))
	     ((null ,keyvar))
	   (case (car ,keyvar)
	     ,@(mapcar #'(lambda (key)
			   `(,(form-keyword key)
			     (setf ,key (car (setf ,keyvar (cdr ,keyvar))))))
		       key-symbols)
	     (otherwise
	      ,@(or otherwise
		    `((error ,error-msg (car ,keyvar))))))))
       ,@body)))

#+TEST
(with-keywords-bound ((filter index pred)
			     '(:filter (t nil) :pred fred a b c)
			     keyvar (or stuff (setf stuff keyvar))) 
  (spy filter index pred stuff))
                         
;;; ***************************************************************************
;;; EOF


