;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clasp/clasp/development/dataset-fns.lisp *-*
;;;; *-* Last-edit: Wednesday, July 21, 1993  17:17:19; Edited-By: carlson *-* 
;;;; *-* Machine: Miles (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                           Data Set Functions                           *
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
;;;  06-24-92 File Created.  (Carlson)
;;;  02-13-93 Added uses of *dataset-class* and *variable-class*.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clasp)

;;; --*--
;;; ***************************************************************************

;;; ***************************************************************************
;;;; Dataset manipulation functions
;;; ***************************************************************************

;;; The following functions are used to maintain the datasets in a session.  At
;;; the dataset level, they include creation, destruction, activatation and
;;; deactivation of datasets.  At the variable level they include adding and
;;; deleting of variables (columns), adding and deleting of tuples (rows), and
;;; movement of variables between datasets.

;;; ***************************************************************************
;;;; Dataset level
;;; ***************************************************************************

(defmethod make-dataset (&key (name "Dataset") (description nil))
  "Creates a new dataset and fills the `name' and `description' slots.
Make-dataset also initializes a row-number variable for the dataset."
  (let* ((new-id (make-clasp-symbol name :dataset))
	 (new-dataset  (make-instance *dataset-class*
			 :id new-id
			 :name (string new-id)
			 :variables nil
			 :variable-count (make-hash-table :test #'equal)
			 :description description
			 :rows 0)))
    (setf (dataset-write-data new-dataset)
      (make-clasp-symbol "RTM-TABLE" :rtm-table))
    (setf (dataset-read-data new-dataset)
      (dataset-write-data new-dataset))
    (unless
	(create-table (dataset-write-data new-dataset))
      (error 'rtm-table-creation-error))
    (push (make-variable :name "Row Number" :attribute
			 '(row-number :number :index)
			 :dataset new-dataset)
	  (dataset-variables new-dataset))
    (setf (dataset-where-clause new-dataset) nil)
    (push new-dataset *clasp-datasets*)
    (when *preferences-activate-automatically*
      (push new-dataset *clasp-active-datasets*))
    new-dataset))


(defmethod make-dataset-from-rows
    (name data column-names &optional description)
  "Creates a new data set and fills the `name' and `description' slots.  Then
it creates variables for the dataset using `column-names' as the names of the
variables and `data' as the data.  `Data' should be in row-major order."
  (check-type name (or symbol string))
  (check-type data sequence)
  (check-type column-names sequence)
  (check-type description (or string null))
  
  (if (not (= (length column-names) (length (car data))))
      (warn 'row-length-mismatch))
  (let ((new-dataset (make-dataset :name name :description description)))
    (dotimes (n (length column-names))
      (add-variable-to-dataset new-dataset (list (nth n (car data)))
			       (nth n column-names)))
    (add-rows-to-dataset new-dataset (cdr data)
			 (remove 'row-number (dataset-variables new-dataset)
				 :key #'id))
    new-dataset))

(defmethod make-dataset-from-columns
    (name data column-names &optional description)
  "Creates a new data set and fills the `name' and `description' slots.  Then
it creates variables for the dataset using `column-names' as the names of the
variables and `data' as the data.  `Data' should be in column-major order."
  (check-type name (or symbol string))
  (check-type data sequence)
  (check-type column-names sequence)
  (check-type description (or string null))

  (let ((new-dataset (make-dataset :name name :description description)))
    (dolist (column column-names)
      (add-variable-to-dataset new-dataset
			       (nth (position column column-names) data)
			       column))
    new-dataset))

(defmethod merge-datasets (datasets)
  (let ((variable-hash-table (make-hash-table :test #'equal))
	variables
	(new-dataset (make-dataset :name (format nil "Merge of ~{~a~^ ~}" (mapcar #'name datasets)))))
    (dolist (dataset datasets)
      (dolist (variable (dataset-variables dataset))
	(unless (equal (name variable) 'row-number)
	  (push dataset (gethash (name variable) variable-hash-table nil)))))
    (maphash #'(lambda (variable-name value)
		 (declare (ignore value))
		 (push variable-name variables))
	     variable-hash-table)
    (push 'source variables)
    (dolist (variable variables)
      (push (add-variable-to-dataset new-dataset '() (format nil "~a" variable))
	    (gethash variable variable-hash-table)))
    (dolist (dataset datasets)
      (add-rows-to-dataset
       new-dataset
       (mapcar #'(lambda (row)
		   (cons (name dataset) row))
	       (dataset-to-rows dataset :columns (remove 'row-number (dataset-variables dataset)
							 :key #'name :test #'equal)))
       (cons 'source
	     (mapcar #'(lambda (x) (car (gethash (name x) variable-hash-table)))
		     (remove 'row-number (dataset-variables dataset) :key #'name :test #'equal)))))
    new-dataset))

(defun rename-dataset (dataset new-name)
  "rename-dataset changes the name of `dataset to `new-name', insuring the
new name is unique.  `dataset' can either be a dataset or the name of a
dataset, and `new-name' must be a string."
  (setf dataset (get-dataset dataset))
  (let (symbol-name
	string-name)
    (cond
      ((stringp new-name)
       (setf symbol-name (make-clasp-symbol new-name :dataset)
	     string-name new-name))
      ((symbolp new-name)
       (setf symbol-name new-name
	     string-name (string new-name)))
      (t (error "Illegal parameter to rename-dataset: ~a" new-name)))
    (assert (not (zerop (length string-name))) () "zero length dataset name")
    (setf (name dataset) string-name)
    (setf (id dataset) symbol-name))
  (values))

(defun delete-dataset (dataset)
  "Destroys a dataset.  `dataset' can either be a dataset or the name of a
dataset."
  (setf dataset (get-dataset dataset)) ;If this line is removed, add checking of dataset existance
  (dolist (variable (dataset-variables dataset))
    (delete-variable variable))
  (setf (id dataset) nil)
  (setf (dataset-variable-count dataset) nil)
  (setf (dataset-write-data dataset) nil)
  (setf (dataset-read-data dataset) nil)
  (setf (dataset-where-clause dataset) nil)
  (setf (dataset-variables dataset) nil)
  (setf (dataset-variable-count dataset) nil)
  (setf (dataset-rows dataset) nil)
  (setf *clasp-datasets* (remove dataset *clasp-datasets*))
  (setf *clasp-active-datasets* (remove dataset *clasp-active-datasets*))
  #+ignore
  (drop-table (dataset-read-data dataset))
  #+ignore
  (drop-table (dataset-write-data dataset))
  (kill dataset)
  (values))


(defun activate-dataset (dataset)
  "Puts a dataset on the clasp active-dataset list."
  (setf dataset (get-dataset dataset nil)) ;If this line is removed, add checking of dataset existance
  (unless (find dataset *clasp-active-datasets*)
    (push dataset *clasp-active-datasets*)))

(defun deactivate-dataset (dataset)
  "Takes a dataset off of the clasp active-datasets list."
  (setf *clasp-active-datasets* (remove (get-dataset dataset)
					*clasp-active-datasets*)))

(defmethod get-dataset ((the-dataset dataset)
			&optional (active-datasets-only-p t))
  "Given a name or id, finds the dataset.  If `active-datasets-only-p' is nil,
will search through all current clasp datasets, otherwise, only the active
datasets are searched."
  (declare (ignore active-datasets-only-p))
  the-dataset)

(defmethod get-dataset ((the-dataset-name string)
			&optional (active-datasets-only-p t))
  "Given a name or id, finds the dataset.  If `active-datasets-only-p' is nil,
will search through all current clasp datasets, otherwise, only the active
datasets are searched."
  (or (find the-dataset-name (if active-datasets-only-p
				 *clasp-active-datasets*
			       *clasp-datasets*)
	    :key #'name
	    :test #'string-equal)
      (error 'dataset-not-found)))

(defmethod get-dataset ((the-dataset-id symbol)
			&optional (active-datasets-only-p t))
  "Given a name or id, finds the dataset.  If `active-datasets-only-p' is nil,
will search through all current clasp datasets, otherwise, only the active
datasets are searched."
  (or (find the-dataset-id (if active-datasets-only-p
			       *clasp-active-datasets*
			       *clasp-datasets*)
	    :key #'id)
      (find the-dataset-id (if active-datasets-only-p
			       *clasp-active-datasets*
			       *clasp-datasets*)
	    :key #'name)
      (error 'dataset-not-found)))

;;; ***************************************************************************
;;;; Variable level
;;; ***************************************************************************

(defun make-variable (&key name type attribute value
			   description dataset rtm-table)
  "make-variable creates a new variable.  `name' is a string.  If `type' is one
of :number, :symbol or :string, then that will be used as the type, otherwise
the type will be inferred from (type-of `type').  `value' is the data the
variable should be initialized with.  `description' is a description of the
variable.  `dataset' is the dataset to which the variable belongs."
  (declare (ignore type))
  (let* ((id (or (car attribute)
		 (make-clasp-symbol name :variable dataset)))
	 (attr (list id
		     #+ignore
		     (cond 
		      ((eq type ':number) ':number)
		      ((eq type ':string) ':string)
		      ((eq type ':symbol) ':symbol)
		      ((numberp type) ':number)
		      ((stringp type) ':string)
		      ((symbolp type) ':symbol)
		      (t (error 'invalid-data-type)))
		     #+ignore
		     ':ugly
		     ':number-or-symbol
		     ))
	new-variable)
    (setf new-variable (make-instance *variable-class*
				      :id id
				      :name (string id)
				      :attribute (or attribute attr)
				      :description description
				      :dataset dataset))
    (create-attribute (or rtm-table (dataset-write-data dataset))
		      (or attribute attr))
    (when value
      (setf (variable-value new-variable) value))
    new-variable
    )
  )


(defmethod add-variable-to-dataset (dataset data variable-name
				    &optional (description nil))
  "add-variable-to-dataset adds the `data', which must be a list of values, to
`dataset' using `variable-name' as the name of the new variable and
`description' as a description."
  (check-type dataset dataset)
  (check-type data sequence)
  (check-type variable-name (or symbol string))
  (check-type description (or string null))

  (if (< (length data) (dataset-rows dataset))
      (warn 'column-length-mismatch))
  (let ((new-variable (make-variable
			:name variable-name
			:type (car data)
			:description description
			:dataset dataset)))
    (push-end new-variable (dataset-variables dataset))
    (setf (variable-value new-variable) data)
    new-variable
    )
  )

(defmethod add-variables-to-dataset (dataset data-list variable-names
				     &optional (descriptions nil))
  "add-variables-to-dataset adds a number of variables to `dataset'.  For each
variable, i, its value (which must be a list) is the ith element of
`data-list', its name is the ith element of `variable-names', and its
description is the ith element of descriptions."
  (check-type dataset dataset)
  (check-type data-list sequence)
  (check-type variable-names sequence)
  (check-type descriptions (or sequence null))

  (dotimes (variable-number (length data-list))
    (let* ((variable-name (nth variable-number variable-names))
	   (description (nth variable-number descriptions))
	   (data (nth variable-number data-list))
	   (new-variable (make-variable
			   :name variable-name
			   :type (car data)
			   :description description
			   :dataset dataset)))
      (push-end new-variable (dataset-variables dataset))
      (setf (variable-value new-variable) data)
      )
    )
  )

(defmethod rename-variable (variable new-name)
  "rename-variable changes the name of `variable' to `new-name', insure the
new name is unique.  `variable' can either be a variable or the name of a
variable, and `new-name' must be a string."
  (setf variable (get-variable variable)) ;If this line is removed, add checking of variable existance
  (let ((dataset (variable-dataset variable))
	symbol-name
	string-name)
    (cond
     ((stringp new-name)
      (setf symbol-name (make-clasp-symbol new-name :variable dataset)
	    string-name new-name))
     ((symbolp new-name)
      (setf symbol-name new-name
	    string-name (string new-name)))
     (t (error "Illegal parameter to rename-dataset: ~a" new-name)))
    (setf (name variable) string-name)
    (setf (id variable) symbol-name))
  (values))


(defmethod delete-variable (variable)
  "Destroys a variable.  `variable' can either be a variable or the name of a
variable."
  (setf variable (get-variable variable)) ;If this line is removed, add checking of variable existance
  (let ((dataset (variable-dataset variable)))
    #+ignore
    (drop-attribute (dataset-write-data dataset) (name variable))
    (setf (dataset-variables dataset)
      (remove variable (dataset-variables dataset))))
  (setf (id variable) nil)
  (setf (name variable) nil)
  (setf (description variable) nil)
  (setf (slot-value variable 'value) nil)
  (setf (variable-dataset variable) nil)
  (setf (variable-attribute variable) nil)
  (kill variable)
  (values))

(defmethod get-variable ((the-variable variable)
			 &optional dataset (active-datasets-only-p t))
  "Returns a variable given the variable name.  If variables in different
datasets have the name being searched for, the optional `dataset' argument
should be used to indicate which dataset(s) to look for the variable in.  The
`active-datasets-only-p' flag is only applicable when no `datasets' variable is
included.  It is used whether to describe all clasp datasets currently loaded,
or just those which are active."
  (declare (ignore dataset active-datasets-only-p))
  the-variable)

(defmethod get-variable ((variable-id symbol)
			 &optional dataset (active-datasets-only-p t))
  "Given a name or id, finds the variable.  If `dataset' is specified, that is
the dataset which will be searched for `variable'.  Otherwise, a search will be
made across datasets.  If this is the case, then `active-datasets-only-p' will
determine whether all datasets currently loaded are searched, or just the
active ones.  Note, since variable names must only be unique within a dataset,
not across datasets, care must be taken to request the correct variable.  It is
advisable to call get-variable with the dataset optional variable set."
  (let ((dataset-list (if dataset
			  (list (get-dataset dataset))
			(if active-datasets-only-p
			    *clasp-active-datasets*
			  *clasp-datasets*)))
	return-val)
    (dolist (dataset dataset-list)
      (when (setf return-val
	      (or (find variable-id (dataset-variables dataset)
			:key #'id)
		  (find variable-id (dataset-variables dataset)
			:key #'name)))
	(return-from get-variable return-val)))
    (error 'variable-not-found)))

(defmethod get-variable ((variable-name string)
			 &optional dataset (active-datasets-only-p t))
  "Given a name or id, finds the variable.  If `dataset' is specified, that is
the dataset which will be searched for `variable'.  Otherwise, a search will be
made across datasets.  If this is the case, then `active-datasets-only-p' will
determine whether all datasets currently loaded are searched, or just the
active ones.  Note, since variable names must only be unique within a dataset,
not across datasets, care must be taken to request the correct variable.  It is
advisable to call get-variable with the dataset optional variable set."
  (let ((dataset-list (if dataset
			  (list (get-dataset dataset))
			(if active-datasets-only-p
			    *clasp-active-datasets*
			  *clasp-datasets*)))
	(variable-symbol (make-symbol-string variable-name))
	return-val)
    (dolist (dataset dataset-list)
      (when (setf return-val
	      (or (find variable-name (dataset-variables dataset)
			:key #'id :test #'string-equal)
		  (find variable-name (dataset-variables dataset)
			:key #'name :test #'string-equal)
		  (find variable-symbol (dataset-variables dataset)
			:key #'id :test #'string-equal)
		  (find variable-symbol (dataset-variables dataset)
			:key #'name :test #'string-equal)))
	    (return-from get-variable return-val)))
	(error 'variable-not-found)))

;;; ***************************************************************************
;;;; Row level
;;; ***************************************************************************

(defmethod add-row-to-dataset (dataset data variables)
  "This function will add `data' to the dataset `dataset' as a new row.
`variables' is a list of variables and specifies what order the new row is in
with respect to the variables in the dataset.  Therefore, `data' must match
`variables' in both length and the types of its values.

Note: Whenever this function is called, clear-cache should be called on the
same dataset."
  (check-type data sequence)
  (check-type variables sequence)
  
  (setf dataset (get-dataset dataset))
  (let ((row-number (incf (dataset-rows dataset)))
	(list-length (length variables))
	pos
	addition)
    (if (not (= list-length (length data)))
	(warn 'row-length-mismatch))
    (dotimes (n list-length)
      (setf pos (- list-length n 1))
      (push (nth pos data) addition)
      (push (car (variable-attribute
		  (get-variable (nth pos variables) dataset))) addition))
    (push row-number addition)
    (push 'row-number addition)
    (insert-into-table (dataset-write-data dataset) addition)))

(defmethod add-rows-to-dataset (dataset data-list variables)
  "This function will add `data-list' to the dataset `dataset'.  `data-list' is
a list of lists representing rows of data.  The rows must be of equal length.
`variables' is a list of variables and specifies what order the new rows are in
with respect to the variables in the dataset.  Therefore, each element of
`data-list' must match variables in both length and the types of its elements."
  (check-type data-list sequence)
  (check-type variables sequence)
  
  (clear-cache dataset)
  (dolist (data data-list)
    (add-row-to-dataset dataset data variables)))

;;; ***************************************************************************
;;; EOF
