;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/v3dev/defs.lisp *-*
;;;; *-* Last-edit: Monday, March 8, 1993  17:34:45; Edited-By: carlson *-* 
;;;; *-* Machine: Lester (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                               Definitions                              *
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
;;;  07-08-92 File Created.  (Carlson)
;;;  02-13-93 Added (named-object remember-instances object-with-properties)
;;;           superclass to `data'.
;;;           Added  *dataset-class* and *variable-class* so the interface can 
;;;           control the class of the objects created.
;;;           Altered `dataset' and `variable' accessors to use slots from 
;;;           `named-object'.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clasp)

;;; --*--
;;; ***************************************************************************


;;; ***************************************************************************
;;;; Data set definition
;;; ***************************************************************************

;;; I made the superclasses of data ignored because leaving any of the
;;; them in caused an error on the TI.  SDA 2/15/93 

;;; Fixed this problem by #-explorer'ing the meta-class options in
;;; intrinsic-mixins.  AC 2/23/93

(defclass data (named-object remember-instances object-with-properties)
  ((id
     :type symbol
     :documentation "Unique data identifier"
     :initarg :id
     :accessor id)
   (source
     :type t
     :documentation "Where did this data come from?"
     :initarg :source
     :accessor source))
  (:documentation
  "The basic data class for clasp.  Dataset, variable etc.  are all subclasses
  of data"))

(defmethod description ((self data))
  (with-slots (description) self
    (if (null description)
	(setf description (format nil "~@(~a~)" (name self)))
      description)))

;;; ***************************************************************************
;;;; Data set definition
;;; ***************************************************************************

;;; There are two basic data structures in clasp.  The variable holds a set of
;;; data and is intended to represent a variable in an experiment.  In most
;;; descriptions of data, the variable will correspond to a column of data.
;;; The dataset is a collection of variables.  Its rows represent tuples of
;;; data which could be particular trials in an experiment.  Its columns are
;;; variables.

;;; A dataset represents a group of related variables, and some extra
;;; information about them

#+:franz-inc
(setf excl:*enable-package-locked-errors* nil)

(defvar *dataset-class* 'dataset)

(defclass dataset (data)
  ((write-data
     :type symbol
     :documentation
     "Storage for the actual value of the variables.  This is the name of the
     rtm table."
     :initarg :write-data
     :accessor dataset-write-data)
   (read-data
    :type symbol
    :documentation
    "Name of views for cases where data must be read and written in different
    ways"
    :initarg :read-data
    :accessor dataset-read-data)
   (where-clause
    :type list
    :documentation "Restrictions of range of data within rtm table"
    :initarg :where-clause
    :accessor dataset-where-clause)
   (variables
     :type list
     :documentation
     "List of the variables in a dataset"
     :initarg :variables
     :accessor dataset-variables)
   (variable-count
     :type hash-table
     :documentation
     "Used to create unique names for variables by adding a counter to
     conflicting names"
     :initarg :variable-count
     :accessor dataset-variable-count)
   (rows
     :type integer
     :documentation "The number of rows in the dataset"
     :initarg :rows
     :accessor dataset-rows))
  (:documentation
  "The dataset is the top-level user class for data representation in CLASP.
  Datasets are collections of variables"))

(defmethod dataset-variable-count ((the-dataset t))
  (declare (special *clasp-variable-count*))
  *clasp-variable-count*)

;;; A variable consists of information about a particular variable in a dataset

(defvar *variable-class*  'variable)

(defclass variable (data)
  ((value
     :type t
     :documentation "The value of a variable, expressed as a list"
     :initarg :value
     :initform nil)
   (dataset
     :type dataset
     :documentation "The dataset that the variable belongs to"
     :initarg :dataset
     :accessor variable-dataset)
   (attribute
     ;;:type (I don't know what types attributes are)
     :documentation
     "The attribute information for the particular column of the dataset rtm
     table"
     :initarg :attribute
     :accessor variable-attribute))
  (:documentation
  "Variables are ordered collections of things.  Valid data types for variables
  are numbers (integers, rationals and floats) and symbols and strings (for
  categorical data)."))

#+:franz-inc
(setf excl:*enable-package-locked-errors* t)

(defmethod description ((self variable))
  (with-slots (description name dataset) self
    (if (null description)
	(setf description (format nil "~@(~a from ~a~)" name (description dataset)))
      description)))

(defmethod variable-value ((the-variable variable) &key
			   (order-by 'row-number)
			   where)
  "Returns the value of `the-variable'.  If included, `order-by' is the name of
another variable which `the-variable' should be sorted by, and `where' is a
where-clause (see ???) selecting a subset of the variable."
  (if (and (slot-value the-variable 'value)
	   (eq order-by 'row-number)
	   (null where))
      (slot-value the-variable 'value)
    (let ((value 
	    (list-selection :attrs (car (variable-attribute the-variable))
			    :from (dataset-read-data
				    (variable-dataset the-variable))
			    :order-by order-by
			    :where (process-where-clause where))))
      (when (and (eq order-by 'row-number) (null where)) ; cache the value if there was
	(setf (slot-value the-variable 'value) value))   ;nothing tricky about this access
      (values value))))

(defmethod (setf variable-value) (list (the-variable variable) &key
				  (order-by 'row-number)
				  where)
  "Sets the value of `the-variable'.  If included, `order-by' is the name of
another variable which `the-variable' should be sorted by, and `where' is a
where-clause (see ???) selecting a subset of the variable."
  (let ((dataset (variable-dataset the-variable)))
    ;; Go through the data, setting each value of variable to the next `list'
    ;; element
    (do-selection (:from (dataset-write-data dataset)
			 :order-by order-by
			 :where (or where (dataset-where-clause dataset)))
      (setf (attr-value (car (variable-attribute the-variable))) (pop list))
      )
    ;; If there is any data left in `list', then you are adding rows to the
    ;; dataset, insert the new elements, incrementing row-number accordingly.
    ;; Then clear the cache, since other variables don't have values for the
    ;; new rows and are therefore out of date.
    (when list
      (dolist (element list)
	(let ((addition (list 'row-number (incf (dataset-rows dataset))
			      (car (variable-attribute the-variable)) element)))
	  (insert-into-table (dataset-write-data dataset) addition)
	  )
	)
      (clear-cache (variable-dataset the-variable))
      )
    )
  ;; Cache the new value and return it
  (setf (slot-value the-variable 'value) (variable-value the-variable))
  )

(defun process-where-clause (clause)
  "Make sure all the symbols in the where-clause are quoted."
  (cond
    ((null clause) nil)
    ((member (car clause) '(.and. .or. .not.))
     (cons (car clause) (mapcar #'process-where-clause (cdr clause))))
    (t (cons (car clause) (mapcar #'(lambda (arg) (list 'quote  arg))
				  (cdr clause))))))

(defun clear-cache (dataset &optional variables)
  (unless variables
    (setf variables (dataset-variables dataset)))
  (dolist (variable variables)
    (setf (slot-value variable 'value) nil)))

;;; ***************************************************************************
;;;; Support functions
;;; ***************************************************************************

;;; These are low-level functions used by the dataset and variable functions to
;;; do such tasks as create new symbols for internally stored objects like
;;; rtm-tables which the user never sees.

(defmethod print-object ((the-variable variable) stream)
  (if (and *print-pretty* (not *print-escape*))
      (format stream "~&id: ~a~%name: ~a~%dataset: ~a~%attribute: ~a
~%sample values: ~a"
	      (id the-variable)
	      (name the-variable)
	      (id (variable-dataset the-variable))
	      (variable-attribute the-variable)
	      (subseq (variable-value the-variable) 0 4))
    (print-unreadable-object (the-variable stream :type t)
      (write-string (name-string the-variable) stream))))

(defmethod print-object ((the-dataset dataset) stream)
  (if (and *print-pretty* (not *print-escape*))
      (format stream "~&id: ~a~%name: ~a~%data: ~a
~%variables: (~{~a~^, ~})~%rows: ~a"
	      (id the-dataset)
	      (name the-dataset)
	      (dataset-write-data the-dataset)
	      (mapcar #'id (dataset-variables the-dataset))
	      (dataset-rows the-dataset))
    (print-unreadable-object (the-dataset stream :type t)
      (write-string (name-string the-dataset) stream))))

;;; The following variables hold numbers which make allow for unique naming of
;;; various clasp internally stored objects.

(defvar *clasp-rtm-table-count*
	(make-hash-table :test #'equal)
  "Used to create unique names for the rtm tables used to store clasp
datasets")

(defvar *clasp-dataset-count*
	(make-hash-table :test #'equal)
  "Used to create unique names for clasp datasets")

(defvar *clasp-variable-count*
	(make-hash-table :test #'equal)
  "Used to create unique names for clasp variables")

(defvar *clasp-data-count*
	(make-hash-table :test #'equal)
  "Used to create unique names for objects not having their own counter")

(defun make-symbol-string (prefix &optional (count 0))
  "Uppercases `prefix' and replaces all the whitespace with dashes.  Also
appends -`count' to the end of the string for non-zero `count's"
  (format nil "~:@(~A~@[-~A~]~)" 
	  (substitute-if #\- 
			 #'(lambda (char) 
			     (member char
				     '(#\SPACE #\TAB #\NEWLINE
				       #\LINEFEED #\PAGE #\RETURN)
				     :test #'char=))
			 (remove-if #'(lambda (char)
					(member char
						'(#\( #\) #\' #\|)
						:test #'char=))
				    prefix))
	  (and (not (= 0 count)) count)))

(defun make-clasp-symbol (prefix type &optional dataset)
  "Generate a unique symbol given a prefix and a type.  Valid types
are :rtm-table, :dataset, :variable and :data.  If the type is
variable, then dataset will be the dataset which the variable is
created for (a variable name that is unique in one dataset may not be
in another.  The first occurence of a given prefix produces a symbol
the same as the prefix (capitalized and with dashes substitued for
whitespace).  Subsequent repetitions of a prefix will cause a number
to be appended to force uniqueness."
  (setf prefix (string-upcase prefix))
  (let (count)
    (cond
     ((eq type :rtm-table)
      (setf count (incf (gethash prefix *clasp-rtm-table-count* -1))))
     ((eq type :dataset)
      (setf count (incf (gethash prefix *clasp-dataset-count* -1))))
     ((eq type :variable)
      (setf count (incf (gethash (format nil "~a" prefix)
				 (dataset-variable-count dataset) -1))))
     ((eq type :data)
      (setf count (incf (gethash prefix *clasp-data-count* -1))))
     (t (error 'invalid-data-type)))
    (intern (make-symbol-string prefix count))))

;;; ***************************************************************************
;;; Special clasp variables
;;; ***************************************************************************

;;; The preferences variables contain options which the user can adjust which
;;; effect the running of the program and allow user customization

(defvar *clasp-preferences* nil
  "Holds settings by the user which control global behavior of the program")

(defvar *preferences-activate-automatically* t
  "Should newly loaded/created datasets be automatically added to the *clasp-active-datasets* list.")

(defun get-preferences (preference-name &optional (preferences *clasp-preferences*))
  "Get a preference value from a preference list"
  (assoc preference-name preferences))

(defun add-preference (preference-name preference-value &optional (preference-list *clasp-preferences*))
  (nconc preference-list (acons preference-name preference-value nil)))

(defun remove-preference (preference preference-list)
  (delete preference preference-list :key #'car))

;;; The variables *clasp-datasets* and *clasp-active-datasets* contain datasets
;;; which are currently extant within a clasp session.  *clasp-datasets*
;;; contains all datasets which have been loaded or created and not
;;; subsequently removed.  *clasp-active-datasets* contains a subset of
;;; *clasp-datasets* which are the choices presented the user when a dataset
;;; selection must occur.  (This is basically for the convenience of the user.
;;; It allows many datasets to be open, without cluttering up the menus.)

(defvar *clasp-datasets* nil
  "A list of the datasets available in the current session of clasp")
(defvar *clasp-active-datasets* nil
  "A list of the datasets which are currently active in clasp")

;;; ***************************************************************************
;;; EOF
